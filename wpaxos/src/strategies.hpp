#pragma once

#include "metadata.hpp"
#include "kv_store.hpp"
#include "data_retriever.hpp"

namespace Mosaic {

enum class PlacementStrategy {
    HOT_TABLE,
    HOT_COLUMN,
    LOPT
};

struct Placement {
    uint32_t column_id;
    uint32_t device_id;
};

class Strategies {
public:
    static std::vector<Placement> place_data(PlacementStrategy strategy, Metadata& metadata, float* budget = nullptr);
    static float predict_scan_time(const std::vector<Placement>& placements, const TraceEntry& table_scan, Metadata& metadata);
    static float predict_total_time(const std::vector<Placement>& placements, Metadata& metadata);
    static float calculate_total_cost(const std::vector<Placement>& placements, Metadata& metadata);
    static std::vector<Placement> optimize_placement(float budget, Metadata& metadata);
    static void apply_placements(const std::vector<Placement>& placements, Metadata& metadata, KVStore& kv_store, DataRetriever& data_retriever);

private:
    static std::vector<Placement> hot_table_strategy(Metadata& metadata);
    static std::vector<Placement> hot_column_strategy(Metadata& metadata);
    static std::vector<Placement> lopt_strategy(Metadata& metadata, float* budget);
};

} // namespace Mosaic