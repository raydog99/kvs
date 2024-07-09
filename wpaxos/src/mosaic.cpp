#include "mosaic.hpp"
#include <iostream>
#include <chrono>
#include <sstream>

namespace Mosaic {

Mosaic::Mosaic() {}

void Mosaic::setup_devices() {
    metadata.add_device({1, "/mnt/nvme", "NVMe SSD", 1000, 8, CompressionAlgorithm::ZSTD, 0.25f});
    metadata.add_device({2, "/mnt/ssd", "SATA SSD", 2000, 4, CompressionAlgorithm::LZ4, 0.15f});
    metadata.add_device({3, "/mnt/hdd", "HDD", 4000, 1, CompressionAlgorithm::None, 0.05f});
}

void Mosaic::setup_tables() {
    std::vector<Column> customer_columns = {
        {1, 1, "c_custkey", "INT", 4ULL * 150000},
        {2, 1, "c_name", "VARCHAR", 25ULL * 150000},
        {3, 1, "c_address", "VARCHAR", 40ULL * 150000}
    };
    metadata.add_table({1, "customer", customer_columns});

    std::vector<Column> orders_columns = {
        {4, 2, "o_orderkey", "INT", 4ULL * 1500000},
        {5, 2, "o_custkey", "INT", 4ULL * 1500000},
        {6, 2, "o_totalprice", "DECIMAL", 8ULL * 1500000}
    };
    metadata.add_table({2, "orders", orders_columns});
}

TraceEntry Mosaic::parse_query(const std::string& query_str) {
    std::istringstream iss(query_str);
    std::string token;
    std::getline(iss, token, ':');
    uint32_t table_id = std::stoi(token);

    std::vector<uint32_t> column_ids;
    while (std::getline(iss, token, ',')) {
        column_ids.push_back(std::stoi(token));
    }

    return {table_id, column_ids};
}

void Mosaic::run_query(const std::string& query_str) {
    TraceEntry query = parse_query(query_str);
    data_retriever.fetch_table_scan(query.table_id, query.column_ids, metadata, kv_store);
    auto result = data_retriever.get_buffer_data();
    std::cout << "Query executed: " << query_str << std::endl;
    std::cout << "Retrieved " << result.size() << " columns" << std::endl;
}

void Mosaic::optimize_storage(float budget) {
    auto placements = Strategies::optimize_placement(budget, metadata);
    Strategies::apply_placements(placements, metadata, kv_store, data_retriever);
}

void Mosaic::run_mosaic(float budget, const std::vector<std::string>& queries) {
    std::cout << "Initial benchmark..." << std::endl;
    float initial_time = benchmark(queries);
    std::cout << "Initial execution time: " << initial_time << " s" << std::endl;

    std::cout << "\nOptimizing storage..." << std::endl;
    optimize_storage(budget);

    std::cout << "\nRunning benchmark after optimization..." << std::endl;
    float optimized_time = benchmark(queries);
    std::cout << "Optimized execution time: " << optimized_time << " s" << std::endl;

    float improvement = (initial_time - optimized_time) / initial_time * 100.0f;
    std::cout << "\nImprovement: " << improvement << "%" << std::endl;
}

float Mosaic::benchmark(const std::vector<std::string>& queries) {
    auto start = std::chrono::high_resolution_clock::now();
    for (const auto& query : queries) {
        run_query(query);
    }
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<float> duration = end - start;
    return duration.count();
}

} // namespace Mosaic