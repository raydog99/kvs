use crate::metadata::{Metadata, Device, Table, Column, TraceEntry, CompressionAlgorithm};
use crate::kv_store::KVStore;
use crate::data_retriever::DataRetriever;
use crate::strategies::{optimize_placement, apply_placements};

pub struct Mosaic {
    pub metadata: Metadata,
    pub kv_store: KVStore,
    pub data_retriever: DataRetriever,
}

impl Mosaic {
    pub fn new() -> Self {
        Mosaic {
            metadata: Metadata::new(),
            kv_store: KVStore::new(),
            data_retriever: DataRetriever::new(),
        }
    }

    pub fn setup_devices(&mut self) {
        self.metadata.add_device(Device {
            id: 1,
            mnt: "/mnt/nvme".to_string(),
            name: "NVMe SSD".to_string(),
            capacity: 1000,
            threads: 8,
            compression: CompressionAlgorithm::ZSTD,
            cost_per_gb: 0.25,
        });
        self.metadata.add_device(Device {
            id: 2,
            mnt: "/mnt/ssd".to_string(),
            name: "SATA SSD".to_string(),
            capacity: 2000,
            threads: 4,
            compression: CompressionAlgorithm::LZ4,
            cost_per_gb: 0.15,
        });
        self.metadata.add_device(Device {
            id: 3,
            mnt: "/mnt/hdd".to_string(),
            name: "HDD".to_string(),
            capacity: 4000,
            threads: 1,
            compression: CompressionAlgorithm::None,
            cost_per_gb: 0.05,
        });
    }

    pub fn setup_tables(&mut self) {
        let customer_columns = vec![
            Column { id: 1, table_id: 1, name: "c_custkey".to_string(), data_type: "INT".to_string(), size: 4 * 150000 },
            Column { id: 2, table_id: 1, name: "c_name".to_string(), data_type: "VARCHAR".to_string(), size: 25 * 150000 },
            Column { id: 3, table_id: 1, name: "c_address".to_string(), data_type: "VARCHAR".to_string(), size: 40 * 150000 },
        ];
        self.metadata.add_table(Table { id: 1, name: "customer".to_string(), columns: customer_columns });

        let orders_columns = vec![
            Column { id: 4, table_id: 2, name: "o_orderkey".to_string(), data_type: "INT".to_string(), size: 4 * 1500000 },
            Column { id: 5, table_id: 2, name: "o_custkey".to_string(), data_type: "INT".to_string(), size: 4 * 1500000 },
            Column { id: 6, table_id: 2, name: "o_totalprice".to_string(), data_type: "DECIMAL".to_string(), size: 8 * 1500000 },
        ];
        self.metadata.add_table(Table { id: 2, name: "orders".to_string(), columns: orders_columns });
    }

    pub fn parse_query(&self, query_str: &str) -> TraceEntry {
        let parts: Vec<&str> = query_str.split(':').collect();
        let table_id = parts[0].parse().unwrap();
        let column_ids: Vec<u32> = parts[1].split(',').map(|s| s.parse().unwrap()).collect();
        TraceEntry { table_id, column_ids }
    }

    pub fn run_query(&mut self, query_str: &str) {
        let query = self.parse_query(query_str);
        self.data_retriever.fetch_table_scan(query.table_id, &query.column_ids, &self.metadata, &self.kv_store);
        let result = self.data_retriever.get_buffer_data();
        println!("Query executed: {}", query_str);
        println!("Retrieved {} columns", result.len());
    }

    pub fn optimize_storage(&mut self, budget: f32) {
        let placements = optimize_placement(budget, &self.metadata);
        apply_placements(&placements, &self.metadata, &mut self.kv_store, &mut self.data_retriever);
    }

    pub fn run_mosaic(&mut self, budget: f32, queries: &[String]) {
        println!("Initial benchmark...");
        let initial_time = self.benchmark(queries);
        println!("Initial execution time: {:.2} s", initial_time);

        println!("\nOptimizing storage...");
        self.optimize_storage(budget);

        println!("\nRunning benchmark after optimization...");
        let optimized_time = self.benchmark(queries);
        println!("Optimized execution time: {:.2} s", optimized_time);

        let improvement = (initial_time - optimized_time) / initial_time * 100.0;
        println!("\nImprovement: {:.2}%", improvement);
    }

    pub fn benchmark(&mut self, queries: &[String]) -> f32 {
        use std::time::Instant;
        let start = Instant::now();
        for query in queries {
            self.run_query(query);
        }
        start.elapsed().as_secs_f32()
    }
}