#include "strategies.hpp"
#include <algorithm>
#include <limits>
#include <iostream>

namespace Mosaic {

std::vector<Placement> Strategies::place_data(PlacementStrategy strategy, Metadata& metadata, float* budget) {
    switch (strategy) {
        case PlacementStrategy::HOT_TABLE:
            return hot_table_strategy(metadata);
        case PlacementStrategy::HOT_COLUMN:
            return hot_column_strategy(metadata);
        case PlacementStrategy::LOPT:
            return lopt_strategy(metadata, budget);
        default:
            return {};
    }
}

std::vector<Placement> Strategies::hot_table_strategy(Metadata& metadata) {
    std::unordered_map<uint32_t, int> table_access_counts;
    for (const auto& entry : metadata.trace) {
        table_access_counts[entry.table_id]++;
    }

    std::vector<std::pair<uint32_t, int>> sorted_tables(table_access_counts.begin(), table_access_counts.end());
    std::sort(sorted_tables.begin(), sorted_tables.end(),
              [](const auto& a, const auto& b) { return a.second > b.second; });

    std::vector<Device*> sorted_devices;
    for (auto& [_, device] : metadata.devices) {
        sorted_devices.push_back(&device);
    }
    std::sort(sorted_devices.begin(), sorted_devices.end(),
              [](const auto& a, const auto& b) { return a->cost_per_gb > b->cost_per_gb; });

    std::vector<Placement> placements;
    for (size_t i = 0; i < sorted_tables.size(); ++i) {
        Table* table = metadata.get_table(sorted_tables[i].first);
        Device* device = sorted_devices[i % sorted_devices.size()];
        for (const auto& column : table->columns) {
            placements.push_back({column.id, device->id});
        }
    }

    return placements;
}

std::vector<Placement> Strategies::hot_column_strategy(Metadata& metadata) {
    std::unordered_map<uint32_t, int> column_access_counts;
    for (const auto& entry : metadata.trace) {
        for (uint32_t col_id : entry.column_ids) {
            column_access_counts[col_id]++;
        }
    }

    std::vector<std::pair<uint32_t, int>> sorted_columns(column_access_counts.begin(), column_access_counts.end());
    std::sort(sorted_columns.begin(), sorted_columns.end(),
              [](const auto& a, const auto& b) { return a.second > b.second; });

    std::vector<Device*> sorted_devices;
    for (auto& [_, device] : metadata.devices) {
        sorted_devices.push_back(&device);
    }
    std::sort(sorted_devices.begin(), sorted_devices.end(),
              [](const auto& a, const auto& b) { return a->cost_per_gb > b->cost_per_gb; });

    std::vector<Placement> placements;
    for (size_t i = 0; i < sorted_columns.size(); ++i) {
        placements.push_back({sorted_columns[i].first, sorted_devices[i % sorted_devices.size()]->id});
    }

    return placements;
}

std::vector<Placement> Strategies::lopt_strategy(Metadata& metadata, float* budget) {
    std::vector<std::pair<uint32_t, uint64_t>> column_sizes;
    for (const auto& [col_id, column] : metadata.columns) {
        column_sizes.emplace_back(col_id, column.size);
    }
    std::sort(column_sizes.begin(), column_sizes.end(),
              [](const auto& a, const auto& b) { return a.second > b.second; });

    std::vector<Device*> sorted_devices;
    for (auto& [_, device] : metadata.devices) {
        sorted_devices.push_back(&device);
    }
    std::sort(sorted_devices.begin(), sorted_devices.end(),
              [](const auto& a, const auto& b) { return a->cost_per_gb < b->cost_per_gb; });

    std::vector<Placement> placements;
    float total_cost = 0.0f;

    for (const auto& [col_id, size] : column_sizes) {
        for (const auto& device : sorted_devices) {
            float cost = (float)size / (1024 * 1024 * 1024) * device->cost_per_gb;
            if (!budget || total_cost + cost <= *budget) {
                placements.push_back({col_id, device->id});
                total_cost += cost;
                break;
            }
        }
    }

    return placements;
}

float Strategies::predict_scan_time(const std::vector<Placement>& placements, const TraceEntry& table_scan, Metadata& metadata) {
    std::unordered_map<uint32_t, float> device_times;

    for (uint32_t col_id : table_scan.column_ids) {
        auto it = std::find_if(placements.begin(), placements.end(),
                               [col_id](const Placement& p) { return p.column_id == col_id; });
        if (it != placements.end()) {
            Device* device = metadata.get_device(it->device_id);
            DeviceModel* device_model = metadata.get_device_model(it->device_id);
            Column* column = metadata.get_column(col_id);

            if (device && device_model && column) {
                float compression_throughput;
                switch (device->compression) {
                    case CompressionAlgorithm::None:
                        compression_throughput = device_model->none_throughput;
                        break;
                    case CompressionAlgorithm::LZ4:
                        compression_throughput = device_model->lz4_throughput;
                        break;
                    case CompressionAlgorithm::ZSTD:
                        compression_throughput = device_model->zstd_throughput;
                        break;
                }

                float column_time = device_model->seek_time + (float)column->size / compression_throughput;
                device_times[it->device_id] += column_time;
            }
        }
    }

    float max_time = 0.0f;
    for (const auto& [_, time] : device_times) {
        max_time = std::max(max_time, time);
    }

    return max_time;
}

float Strategies::predict_total_time(const std::vector<Placement>& placements, Metadata& metadata) {
    float total_time = 0.0f;
    for (const auto& trace : metadata.trace) {
        total_time += predict_scan_time(placements, trace, metadata);
    }
    return total_time;
}

float Strategies::calculate_total_cost(const std::vector<Placement>& placements, Metadata& metadata) {
    float total_cost = 0.0f;
    for (const auto& placement : placements) {
        Device* device = metadata.get_device(placement.device_id);
        Column* column = metadata.get_column(placement.column_id);
        if (device && column) {
            total_cost += device->cost_per_gb * (float)column->size / (1024 * 1024 * 1024);
        }
    }
    return total_cost;
}

std::vector<Placement> Strategies::optimize_placement(float budget, Metadata& metadata) {
    std::vector<Placement> placements = place_data(PlacementStrategy::LOPT, metadata, &budget);
    float total_time = predict_total_time(placements, metadata);
    float total_cost = calculate_total_cost(placements, metadata);
    std::cout << "Predicted total time: " << total_time << " s" << std::endl;
    std::cout << "Total cost: " << total_cost << std::endl;
    return placements;
}

void Strategies::apply_placements(const std::vector<Placement>& placements, Metadata& metadata, KVStore& kv_store, DataRetriever& data_retriever) {
    for (const auto& placement : placements) {
        Column* column = metadata.get_column(placement.column_id);
        Device* target_device = metadata.get_device(placement.device_id);
        if (column && target_device) {
            auto it = data_retriever.column_chunks.find(placement.column_id);
            if (it != data_retriever.column_chunks.end()) {
                for (auto& chunk : it->second) {
                    if (chunk.device_id != placement.device_id) {
                        data_retriever.move_column_chunk(chunk, placement.device_id, metadata, kv_store);
                        std::cout << "Moved column " << column->name << " to device " << target_device->name << std::endl;
                    }
                }
            }
        }
    }
}

} // namespace Mosaic