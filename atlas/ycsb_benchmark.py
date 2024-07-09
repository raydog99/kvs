import random
import string
import time
from typing import List, Tuple
from kv_store import kv_store

class YCSBBenchmark:
    def __init__(self, record_count: int, operation_count: int):
        self.record_count = record_count
        self.operation_count = operation_count
        self.key_length = 10
        self.value_length = 100

    def generate_key(self) -> str:
        return ''.join(random.choices(string.ascii_lowercase, k=self.key_length))

    def generate_value(self) -> bytes:
        return ''.join(random.choices(string.ascii_lowercase, k=self.value_length)).encode()

    def load_phase(self):
        for _ in range(self.record_count):
            key = self.generate_key()
            value = self.generate_value()
            kv_store.put(key, value)

    def run_workload(self, read_proportion: float, update_proportion: float, insert_proportion: float) -> Tuple[float, List[float]]:
        operations = []
        for _ in range(self.operation_count):
            r = random.random()
            if r < read_proportion:
                operations.append(('read', self.generate_key()))
            elif r < read_proportion + update_proportion:
                operations.append(('update', self.generate_key(), self.generate_value()))
            else:
                operations.append(('insert', self.generate_key(), self.generate_value()))

        start_time = time.time()
        latencies = []

        for op in operations:
            op_start_time = time.time()
            if op[0] == 'read':
                kv_store.get(op[1])
            elif op[0] == 'update':
                kv_store.put(op[1], op[2])
            else:  # insert
                kv_store.put(op[1], op[2])
            latencies.append(time.time() - op_start_time)

        total_time = time.time() - start_time
        return total_time, latencies

    def run_benchmark(self):
        print("Loading data...")
        self.load_phase()

        workloads = [
            ("Workload A (50% read, 50% update)", 0.5, 0.5, 0),
            ("Workload B (95% read, 5% update)", 0.95, 0.05, 0),
            ("Workload C (100% read)", 1, 0, 0),
            ("Workload D (95% read, 5% insert)", 0.95, 0, 0.05),
            ("Workload E (95% scan, 5% insert)", 0, 0, 0.05),  
            ("Workload F (50% read, 50% read-modify-write)", 0.5, 0.5, 0),
        ]

        for name, read_prop, update_prop, insert_prop in workloads:
            print(f"\nRunning {name}")
            total_time, latencies = self.run_workload(read_prop, update_prop, insert_prop)
            avg_latency = sum(latencies) / len(latencies)
            throughput = self.operation_count / total_time

            print(f"Throughput: {throughput:.2f} ops/sec")
            print(f"Average latency: {avg_latency * 1000:.2f} ms")
            print(f"95th percentile latency: {sorted(latencies)[int(0.95 * len(latencies))] * 1000:.2f} ms")
            print(f"99th percentile latency: {sorted(latencies)[int(0.99 * len(latencies))] * 1000:.2f} ms")

if __name__ == "__main__":
    benchmark = YCSBBenchmark(record_count=100000, operation_count=1000000)
    benchmark.run_benchmark()