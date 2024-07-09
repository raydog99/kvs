use std::collections::HashMap;
use crate::metadata::{Metadata, TraceEntry};
use crate::data_retriever::DataRetriever;
use crate::kv_store::KVStore;

#[derive(Debug, Clone, PartialEq)]
pub enum PlacementStrategy {
    HOT_TABLE,
    HOT_COLUMN,
    LOPT,
}

#[derive(Debug, Clone)]
pub struct Placement {
    pub column_id: u32,
    pub device_id: u32,
}

pub fn hot_table_strategy(metadata: &Metadata) -> Vec<Placement> {
    let mut table_access_counts: HashMap<u32, u32> = HashMap::new();
    for entry in &metadata.trace {
        *table_access_counts.entry(entry.table_id).or_insert(0) += 1;
    }

    let mut sorted_tables: Vec<_> = table_access_counts.into_iter().collect();
    sorted_tables.sort_by(|a, b| b.1.cmp(&a.1));

    let mut sorted_devices: Vec<_> = metadata.devices.values().collect();
    sorted_devices.sort_by(|a, b| b.cost_per_gb.partial_cmp(&a.cost_per_gb).unwrap());

    let mut placements = Vec::new();
    for ((table_id, _), device) in sorted_tables.iter().zip(sorted_devices.iter()) {
        if let Some(table) = metadata.get_table(*table_id) {
            for column in &table.columns {
                placements.push(Placement {
                    column_id: column.id,
                    device_id: device.id,
                });
            }
        }
    }

    placements
}

pub fn hot_column_strategy(metadata: &Metadata) -> Vec<Placement> {
    let mut column_access_counts: HashMap<u32, u32> = HashMap::new();
    for entry in &metadata.trace {
        for &col_id in &entry.column_ids {
            *column_access_counts.entry(col_id).or_insert(0) += 1;
        }
    }

    let mut sorted_columns: Vec<_> = column_access_counts.into_iter().collect();
    sorted_columns.sort_by(|a, b| b.1.cmp(&a.1));

    let mut sorted_devices: Vec<_> = metadata.devices.values().collect();
    sorted_devices.sort_by(|a, b| b.cost_per_gb.partial_cmp(&a.cost_per_gb).unwrap());

    sorted_columns.iter().zip(sorted_devices.iter().cycle()).map(|((col_id, _), device)| {
        Placement {
            column_id: *col_id,
            device_id: device.id,
        }
    }).collect()
}

pub fn lopt_strategy(metadata: &Metadata, budget: Option<f32>) -> Vec<Placement> {
    let mut column_sizes: Vec<_> = metadata.columns.iter().map(|(_, col)| (col.id, col.size)).collect();
    column_sizes.sort_by(|a, b| b.1.cmp(&a.1));

    let mut sorted_devices: Vec<_> = metadata.devices.values().collect();
    sorted_devices.sort_by(|a, b| a.cost_per_gb.partial_cmp(&b.cost_per_gb).unwrap());

    let mut placements = Vec::new();
    let mut total_cost = 0.0;

    for (col_id, size) in column_sizes {
        for device in &sorted_devices {
            let cost = (size as f32 / (1024.0 * 1024.0 * 1024.0)) * device.cost_per_gb;
            if budget.is_none() || total_cost + cost <= budget.unwrap() {
                placements.push(Placement {
                    column_id: col_id,
                    device_id: device.id,
                });
                total_cost += cost;
                break;
            }
        }
    }

    placements
}

pub fn place_data(strategy: PlacementStrategy, metadata: &Metadata, budget: Option<f32>) -> Vec<Placement> {
    match strategy {
        PlacementStrategy::HOT_TABLE => hot_table_strategy(metadata),
        PlacementStrategy::HOT_COLUMN => hot_column_strategy(metadata),
        PlacementStrategy::LOPT => lopt_strategy(metadata, budget),
    }
}

pub fn predict_scan_time(placements: &[Placement], table_scan: &TraceEntry, metadata: &Metadata) -> f32 {
    let mut device_times: HashMap<u32, f32> = HashMap::new();

    for &col_id in &table_scan.column_ids {
        if let Some(placement) = placements.iter().find(|p| p.column_id == col_id) {
            if let (Some(device), Some(device_model), Some(column)) = (
                metadata.get_device(placement.device_id),
                metadata.get_device_model(placement.device_id),
                metadata.get_column(col_id),
            ) {
                let compression_throughput = match device.compression {
                    crate::metadata::CompressionAlgorithm::None => device_model.none_throughput,
                    crate::metadata::CompressionAlgorithm::LZ4 => device_model.lz4_throughput,
                    crate::metadata::CompressionAlgorithm::ZSTD => device_model.zstd_throughput,
                };

                let column_time = device_model.seek_time + (column.size as f32 / compression_throughput);
                *device_times.entry(placement.device_id).or_insert(0.0) += column_time;
            }
        }
    }

    *device_times.values().max_by(|a, b| a.partial_cmp(b).unwrap()).unwrap_or(&0.0)
}

pub fn predict_total_time(placements: &[Placement], metadata: &Metadata) -> f32 {
    metadata.trace.iter().map(|trace| predict_scan_time(placements, trace, metadata)).sum()
}

pub fn calculate_total_cost(placements: &[Placement], metadata: &Metadata) -> f32 {
    placements.iter().map(|p| {
        if let (Some(device), Some(column)) = (metadata.get_device(p.device_id), metadata.get_column(p.column_id)) {
            device.cost_per_gb * (column.size as f32 / (1024.0 * 1024.0 * 1024.0))
        } else {
            0.0
        }
    }).sum()
}

pub fn optimize_placement(budget: f32, metadata: &Metadata) -> Vec<Placement> {
    let placements = place_data(PlacementStrategy::LOPT, metadata, Some(budget));
    let total_time = predict_total_time(&placements, metadata);
    let total_cost = calculate_total_cost(&placements, metadata);
    println!("Predicted total time: {:.2} s", total_time);
    println!("Total cost: {:.2}", total_cost);
    placements
}

pub fn apply_placements(placements: &[Placement], metadata: &Metadata, kv_store: &mut KVStore, data_retriever: &mut DataRetriever) {
    let mut chunks_to_move = Vec::new();

    // Collect all chunks that need to be moved
    for placement in placements {
        if let (Some(column), Some(target_device)) = (metadata.get_column(placement.column_id), metadata.get_device(placement.device_id)) {
            if let Some(chunks) = data_retriever.column_chunks.get(&placement.column_id) {
                for chunk in chunks {
                    if chunk.device_id != placement.device_id {
                        chunks_to_move.push((chunk.clone(), placement.device_id, column.name.clone(), target_device.name.clone()));
                    }
                }
            }
        }
    }

    // Move the collected chunks
    for (chunk, target_device_id, column_name, device_name) in chunks_to_move {
        data_retriever.move_column_chunk(&chunk, target_device_id, metadata, kv_store);
        println!("Moved column {} to device {}", column_name, device_name);
    }
}