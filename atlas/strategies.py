from typing import List, Tuple, Optional
from dataclasses import dataclass
from enum import Enum
from metadata import metadata, TraceEntry
from data_retriever import data_retriever, ColumnChunk

class PlacementStrategy(Enum):
    HOT_TABLE = 1
    HOT_COLUMN = 2
    LOPT = 3

@dataclass
class Placement:
    column_id: int
    device_id: int

def hot_table_strategy() -> List[Placement]:
    table_access_counts = {}
    for entry in metadata.trace:
        table_access_counts[entry.table_id] = table_access_counts.get(entry.table_id, 0) + 1
    
    sorted_tables = sorted(table_access_counts.items(), key=lambda x: x[1], reverse=True)
    sorted_devices = sorted(metadata.devices.values(), key=lambda d: d.cost_per_gb, reverse=True)
    
    placements = []
    for (table_id, _), device in zip(sorted_tables, sorted_devices):
        table = metadata.get_table(table_id)
        placements.extend([Placement(col.id, device.id) for col in table.columns])
    
    return placements

def hot_column_strategy() -> List[Placement]:
    column_access_counts = {}
    for entry in metadata.trace:
        for col_id in entry.column_ids:
            column_access_counts[col_id] = column_access_counts.get(col_id, 0) + 1
    
    sorted_columns = sorted(column_access_counts.items(), key=lambda x: x[1], reverse=True)
    sorted_devices = sorted(metadata.devices.values(), key=lambda d: d.cost_per_gb, reverse=True)
    
    return [Placement(col_id, device.id) for (col_id, _), device in zip(sorted_columns, sorted_devices)]

def lopt_strategy(budget: Optional[float]) -> List[Placement]:
    column_sizes = {col_id: col.size for col_id, col in metadata.columns.items()}
    sorted_columns = sorted(column_sizes.items(), key=lambda x: x[1], reverse=True)
    sorted_devices = sorted(metadata.devices.values(), key=lambda d: d.cost_per_gb)
    
    placements = []
    total_cost = 0
    for col_id, size in sorted_columns:
        for device in sorted_devices:
            cost = (size / (1024 * 1024 * 1024)) * device.cost_per_gb
            if budget is None or total_cost + cost <= budget:
                placements.append(Placement(col_id, device.id))
                total_cost += cost
                break
    
    return placements

def place_data(strategy: PlacementStrategy, budget: Optional[float] = None) -> List[Placement]:
    if strategy == PlacementStrategy.HOT_TABLE:
        return hot_table_strategy()
    elif strategy == PlacementStrategy.HOT_COLUMN:
        return hot_column_strategy()
    elif strategy == PlacementStrategy.LOPT:
        return lopt_strategy(budget)

def predict_scan_time(placements: List[Placement], table_scan: TraceEntry) -> float:
    max_device_time = 0
    devices_used = {}
    
    for col_id in table_scan.column_ids:
        placement = next(p for p in placements if p.column_id == col_id)
        device = metadata.get_device(placement.device_id)
        device_model = metadata.get_device_model(placement.device_id)
        column = metadata.get_column(col_id)
        
        compression_throughput = {
            CompressionAlgorithm.NONE: device_model.none_throughput,
            CompressionAlgorithm.LZ4: device_model.lz4_throughput,
            CompressionAlgorithm.ZSTD: device_model.zstd_throughput
        }[device.compression]
        
        column_time = device_model.seek_time + (column.size / compression_throughput)
        
        if placement.device_id not in devices_used:
            devices_used[placement.device_id] = column_time
        else:
            devices_used[placement.device_id] += column_time
    
    return max(devices_used.values())

def predict_total_time(placements: List[Placement], trace: List[TraceEntry]) -> float:
    return sum(predict_scan_time(placements, table_scan) for table_scan in trace)

def calculate_total_cost(placements: List[Placement]) -> float:
    return sum(
        metadata.get_device(p.device_id).cost_per_gb * 
        (metadata.get_column(p.column_id).size / (1024 * 1024 * 1024))
        for p in placements
    )

def optimize_placement(budget: float) -> List[Placement]:
    placements = place_data(PlacementStrategy.LOPT, budget)
    total_time = predict_total_time(placements, metadata.trace)
    total_cost = calculate_total_cost(placements)
    print(f"Predicted total time: {total_time:.2f} s")
    print(f"Total cost: {total_cost:.2f}")
    return placements

def apply_placements(placements: List[Placement]):
    for placement in placements:
        column = metadata.get_column(placement.column_id)
        target_device = metadata.get_device(placement.device_id)
        chunks = data_retriever.column_chunks.get(placement.column_id, [])
        
        for chunk in chunks:
            if chunk.device_id != placement.device_id:
                data_retriever.move_column_chunk(chunk, placement.device_id)
        
        print(f"Moved column {column.name} to device {target_device.name}")