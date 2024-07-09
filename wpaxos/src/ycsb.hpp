#pragma once

#include "kv_store.hpp"
#include <string>
#include <vector>

namespace Mosaic {

struct YCSBWorkload {
    std::string name;
    float read_proportion;
    float update_proportion;
    float insert_proportion;
};

class YCSBBenchmark {
public:
    YCSBBenchmark(int record_count, int operation_count);
    void run_benchmark(KVStore& kv_store);

private:
    std::string generate_key();
    std::vector<uint8_t> generate_value();
    void load_phase(KVStore& kv_store);
    std::pair<float, std::vector<float>> run_workload(const YCSBWorkload& workload, KVStore& kv_store);

    int record_count;
    int operation_count;
    int key_length;
    int value_length;
};

} // namespace Mosaic