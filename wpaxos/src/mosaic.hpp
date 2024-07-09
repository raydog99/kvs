#pragma once

#include "metadata.hpp"
#include "kv_store.hpp"
#include "data_retriever.hpp"
#include "strategies.hpp"
#include <vector>
#include <string>

namespace Mosaic {

class Mosaic {
public:
    Mosaic();
    void setup_devices();
    void setup_tables();
    void run_query(const std::string& query_str);
    void optimize_storage(float budget);
    void run_mosaic(float budget, const std::vector<std::string>& queries);

private:
    TraceEntry parse_query(const std::string& query_str);
    float benchmark(const std::vector<std::string>& queries);

    Metadata metadata;
    KVStore kv_store;
    DataRetriever data_retriever;
};

} // namespace Mosaic