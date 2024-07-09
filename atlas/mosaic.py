from typing import List
import time
from metadata import metadata, Device, Table, Column, TraceEntry, CompressionAlgorithm
from data_retriever import data_retriever
from strategies import optimize_placement, apply_placements
from kv_store import kv_store

def parse_query(query_str: str) -> TraceEntry:
    parts = query_str.split(':')
    table_id = int(parts[0])
    column_ids = [int(col_id) for col_id in parts[1].split(',')]
    return TraceEntry(table_id, column_ids)

def run_query(query_str: str):
    query = parse_query(query_str)
    data_retriever.fetch_table_scan(query.table_id, query.column_ids)
    result = data_retriever.get_buffer_data()
    print(f"Query executed: {query_str}")
    print(f"Retrieved {len(result)} columns")

def optimize_storage(budget: float):
    placements = optimize_placement(budget)
    apply_placements(placements)

def benchmark(queries: List[str]) -> float:
    start_time = time.time()
    for query in queries:
        run_query(query)
    end_time = time.time()
    return end_time - start_time

def run_mosaic(budget: float, queries: List[str]):
    print("Initial benchmark...")
    initial_time = benchmark(queries)
    print(f"Initial execution time: {initial_time:.2f} s")

    print("\nOptimizing storage...")
    optimize_storage(budget)

    print("\nRunning benchmark after optimization...")
    optimized_time = benchmark(queries)
    print(f"Optimized execution time: {optimized_time:.2f} s")

    improvement = (initial_time - optimized_time) / initial_time * 100
    print(f"\nImprovement: {improvement:.2f}%")

def setup_devices():
    metadata.add_device(Device(1, "/mnt/nvme", "NVMe SSD", 1000, 8, CompressionAlgorithm.ZSTD, 0.25))
    metadata.add_device(Device(2, "/mnt/ssd", "SATA SSD", 2000, 4, CompressionAlgorithm.LZ4, 0.15))
    metadata.add_device(Device(3, "/mnt/hdd", "HDD", 4000, 1, CompressionAlgorithm.NONE, 0.05))

def setup_tables():
    customer_columns = [
        Column(1, 1, "c_custkey", "INT", 4 * 150000),
        Column(2, 1, "c_name", "VARCHAR", 25 * 150000),
        Column(3, 1, "c_address", "VARCHAR", 40 * 150000),
    ]
    metadata.add_table(Table(1, "customer", customer_columns))

    orders_columns = [
        Column(4, 2, "o_orderkey", "INT", 4 * 1500000),
        Column(5, 2, "o_custkey", "INT", 4 * 1500000),
        Column(6, 2, "o_totalprice", "DECIMAL", 8 * 1500000),
    ]
    metadata.add_table(Table(2, "orders", orders_columns))

def load_data():
    data_retriever.load_parquet_file("customer.parquet", 1, 1)  # Load customer table to NVMe SSD
    data_retriever.load_parquet_file("orders.parquet", 2, 2)  # Load orders table to SATA SSD

if __name__ == "__main__":
    setup_devices()
    setup_tables()
    load_data()

    sample_queries = [
        "1:1,2,3",  # Select all columns from customer
        "2:4,5",  # Select o_orderkey and o_custkey from orders
        "1:1;2:5",  # Join customer and orders on customer key
    ]

    print("Running Mosaic with a budget of 500:")
    run_mosaic(500.0, sample_queries)

    print("\nSaving optimized data to Parquet files:")
    data_retriever.save_to_parquet("customer_optimized.parquet", 1)
    data_retriever.save_to_parquet("orders_optimized.parquet", 2)

    print("Mosaic demonstration completed.")