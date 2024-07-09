#include "ycsb.hpp"
#include <random>
#include <algorithm>
#include <chrono>
#include <iostream>

namespace Mosaic {

YCSBBenchmark::YCSBBenchmark(int record_count, int operation_count)
    : record_count(record_count), operation_count(operation_count), key_length(10), value_length(100) {}

std::string YCSBBenchmark::generate_key() {
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";
    std::string key(key_length, ' ');
    for (int i = 0; i < key_length; ++i) {
        key[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
    }
    return key;
}

std::vector<uint8_t> YCSBBenchmark::generate_value() {
    std::vector<uint8_t> value(value_length);
    std::generate(value.begin(), value.end(), []() { return rand() % 256; });
    return value;
}

void YCSBBenchmark::load_phase(KVStore& kv_store) {
    for (int i = 0; i < record_count; ++i) {
        std::string key = generate_key();
        std::vector<uint8_t> value = generate_value();
        kv_store.put(key, value);
    }
}

std::pair<float, std::vector<float>> YCSBBenchmark::run_workload(const YCSBWorkload& workload, KVStore& kv_store) {
    std::vector<float> latencies;
    latencies.reserve(operation_count);

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0.0, 1.0);

    auto start = std::chrono::high_resolution_clock::now();

    for (int i = 0; i < operation_count; ++i) {
        float r = dis(gen);
        auto op_start = std::chrono::high_resolution_clock::now();

        if (r < workload.read_proportion) {
            std::string key = generate_key();
            kv_store.get(key);
        } else if (r < workload.read_proportion + workload.update_proportion) {
            std::string key = generate_key();
            std::vector<uint8_t> value = generate_value();
            kv_store.put(key, value);
        } else {
            std::string key = generate_key();
            std::vector<uint8_t> value = generate_value();
            kv_store.put(key, value);
        }

        auto op_end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<float> op_duration = op_end - op_start;
        latencies.push_back(op_duration.count());
    }

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<float> duration = end - start;

    return {duration.count(), latencies};
}

void YCSBBenchmark::run_benchmark(KVStore& kv_store) {
    std::cout << "Loading data..." << std::endl;
    load_phase(kv_store);

    std::vector<YCSBWorkload> workloads = {
        {"Workload A (50% read, 50% update)", 0.5f, 0.5f, 0.0f},
        {"Workload B (95% read, 5% update)", 0.95f, 0.05f, 0.0f},
        {"Workload C (100% read)", 1.0f, 0.0f, 0.0f},
        {"Workload D (95% read, 5% insert)", 0.95f, 0.0f, 0.05f},
        {"Workload E (95% scan, 5% insert)", 0.95f, 0.0f, 0.05f}, // Treating scan as read
        {"Workload F (50% read, 50% read-modify-write)", 0.5f, 0.5f, 0.0f}
    };

    for (const auto& workload : workloads) {
        std::cout << "\nRunning " << workload.name << std::endl;
        auto [total_time, latencies] = run_workload(workload, kv_store);

        float avg_latency = 0.0f;
        for (float latency : latencies) {
            avg_latency += latency;
        }
        avg_latency /= latencies.size();

        float throughput = operation_count / total_time;

        std::cout << "Throughput: " << throughput << " ops/sec" << std::endl;
        std::cout << "Average latency: " << avg_latency * 1000 << " ms" << std::endl;

        std::sort(latencies.begin(), latencies.end());
        float p95 = latencies[static_cast<size_t>(latencies.size() * 0.95)];
        float p99 = latencies[static_cast<size_t>(latencies.size() * 0.99)];

        std::cout << "95th percentile latency: " << p95 * 1000 << " ms" << std::endl;
        std::cout << "99th percentile latency: " << p99 * 1000 << " ms" << std::endl;
    }
}

} // namespace Mosaic