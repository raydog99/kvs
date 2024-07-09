use rand::Rng;
use std::time::Instant;
use crate::kv_store::KVStore;

pub struct YCSBWorkload {
    pub name: String,
    pub read_proportion: f32,
    pub update_proportion: f32,
    pub insert_proportion: f32,
}

pub struct YCSBBenchmark {
    pub record_count: usize,
    pub operation_count: usize,
    pub key_length: usize,
    pub value_length: usize,
}

impl YCSBBenchmark {
    pub fn new(record_count: usize, operation_count: usize) -> Self {
        YCSBBenchmark {
            record_count,
            operation_count,
            key_length: 10,
            value_length: 100,
        }
    }

    fn generate_key(&self) -> String {
        rand::thread_rng()
            .sample_iter(&rand::distributions::Alphanumeric)
            .take(self.key_length)
            .map(char::from)
            .collect()
    }

    fn generate_value(&self) -> Vec<u8> {
        rand::thread_rng()
            .sample_iter(&rand::distributions::Alphanumeric)
            .take(self.value_length)
            .map(|b| b as u8)
            .collect()
    }

    pub fn load_phase(&self, kv_store: &mut KVStore) {
        for _ in 0..self.record_count {
            let key = self.generate_key();
            let value = self.generate_value();
            kv_store.put(key, value);
        }
    }

    pub fn run_workload(&self, workload: &YCSBWorkload, kv_store: &mut KVStore) -> (f32, Vec<f32>) {
        let mut rng = rand::thread_rng();
        let mut latencies = Vec::with_capacity(self.operation_count);

        let start = Instant::now();
        for _ in 0..self.operation_count {
            let r: f32 = rng.gen();
            let op_start = Instant::now();

            if r < workload.read_proportion {
                let key = self.generate_key();
                kv_store.get(&key);
            } else if r < workload.read_proportion + workload.update_proportion {
                let key = self.generate_key();
                let value = self.generate_value();
                kv_store.put(key, value);
            } else {
                let key = self.generate_key();
                let value = self.generate_value();
                kv_store.put(key, value);
            }

            latencies.push(op_start.elapsed().as_secs_f32());
        }
        let total_time = start.elapsed().as_secs_f32();

        (total_time, latencies)
    }

    pub fn run_benchmark(&self, kv_store: &mut KVStore) {
        println!("Loading data...");
        self.load_phase(kv_store);

        let workloads = vec![
            YCSBWorkload { name: "Workload A (50% read, 50% update)".to_string(), read_proportion: 0.5, update_proportion: 0.5, insert_proportion: 0.0 },
            YCSBWorkload { name: "Workload B (95% read, 5% update)".to_string(), read_proportion: 0.95, update_proportion: 0.05, insert_proportion: 0.0 },
            YCSBWorkload { name: "Workload C (100% read)".to_string(), read_proportion: 1.0, update_proportion: 0.0, insert_proportion: 0.0 },
            YCSBWorkload { name: "Workload D (95% read, 5% insert)".to_string(), read_proportion: 0.95, update_proportion: 0.0, insert_proportion: 0.05 },
            YCSBWorkload { name: "Workload E (95% scan, 5% insert)".to_string(), read_proportion: 0.95, update_proportion: 0.0, insert_proportion: 0.05 },
            YCSBWorkload { name: "Workload F (50% read, 50% read-modify-write)".to_string(), read_proportion: 0.5, update_proportion: 0.5, insert_proportion: 0.0 },
        ];

        for workload in workloads {
            println!("\nRunning {}", workload.name);
            let (total_time, latencies) = self.run_workload(&workload, kv_store);
            let avg_latency: f32 = latencies.iter().sum::<f32>() / latencies.len() as f32;
            let throughput = self.operation_count as f32 / total_time;

            println!("Throughput: {:.2} ops/sec", throughput);
            println!("Average latency: {:.2} ms", avg_latency * 1000.0);

            let mut sorted_latencies = latencies.clone();
            sorted_latencies.sort_by(|a, b| a.partial_cmp(b).unwrap());
            let p95 = sorted_latencies[(self.operation_count as f32 * 0.95) as usize];
            let p99 = sorted_latencies[(self.operation_count as f32 * 0.99) as usize];

            println!("95th percentile latency: {:.2} ms", p95 * 1000.0);
            println!("99th percentile latency: {:.2} ms", p99 * 1000.0);
        }
    }
}