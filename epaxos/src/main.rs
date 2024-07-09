mod metadata;
mod kv_store;
mod data_retriever;
mod strategies;
mod mosaic;
mod ycsb_benchmark;

use mosaic::Mosaic;
use ycsb_benchmark::YCSBBenchmark;

fn main() {
    let mut mosaic = Mosaic::new();
    mosaic.setup_devices();
    mosaic.setup_tables();

    let sample_queries = vec![
        "1:1,2,3".to_string(),  // Select all columns from customer
        "2:4,5".to_string(),    // Select o_orderkey and o_custkey from orders
        "1:1;2:5".to_string(),  // Join customer and orders on customer key
    ];

    println!("Running Mosaic with a budget of 500:");
    mosaic.run_mosaic(500.0, &sample_queries);

    println!("\nMosaic demonstration completed.");

    println!("\nRunning YCSB benchmark:");
    let benchmark = YCSBBenchmark::new(100000, 1000000);
    benchmark.run_benchmark(&mut mosaic.kv_store);
}