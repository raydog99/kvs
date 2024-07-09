from ycsb_benchmark import YCSBBenchmark
from mosaic import setup_devices, optimize_storage
from metadata import metadata, Column, Table, TraceEntry
from data_retriever import data_retriever

class MosaicYCSBBenchmark(YCSBBenchmark):
    def __init__(self, record_count: int, operation_count: int):
        super().__init__(record_count, operation_count)
        setup_devices()
        self.setup_ycsb_table()

    def setup_ycsb_table(self):
        ycsb_columns = [
            Column(1, 1, "key", "VARCHAR", self.key_length * self.record_count),
            Column(2, 1, "value", "VARCHAR", self.value_length * self.record_count),
        ]
        metadata.add_table(Table(1, "ycsb", ycsb_columns))

    def load_phase(self):
        super().load_phase()
        # Simulate access pattern for optimization
        for _ in range(self.operation_count):
            metadata.add_trace_entry(TraceEntry(1, [1, 2]))

    def run_benchmark(self):
        print("Running YCSB benchmark with Mosaic optimization")
        print("Initial benchmark...")
        super().run_benchmark()

        print("\nOptimizing storage...")
        optimize_storage(1000.0)

        print("\nRunning benchmark after optimization...")
        super().run_benchmark()

if __name__ == "__main__":
    benchmark = MosaicYCSBBenchmark(record_count=100000, operation_count=1000000)
    benchmark.run_benchmark()