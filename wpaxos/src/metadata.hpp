#pragma once

#include <unordered_map>
#include <vector>
#include <string>

namespace Mosaic {

enum class CompressionAlgorithm {
    None,
    LZ4,
    ZSTD
};

struct Device {
    uint32_t id;
    std::string mnt;
    std::string name;
    uint64_t capacity;
    uint32_t threads;
    CompressionAlgorithm compression;
    float cost_per_gb;
};

struct Column {
    uint32_t id;
    uint32_t table_id;
    std::string name;
    std::string data_type;
    uint64_t size;
};

struct Table {
    uint32_t id;
    std::string name;
    std::vector<Column> columns;
};

struct TraceEntry {
    uint32_t table_id;
    std::vector<uint32_t> column_ids;
};

struct DeviceModel {
    uint32_t device_id;
    float none_throughput;
    float lz4_throughput;
    float zstd_throughput;
    float seek_time;
};

class Metadata {
public:
    void add_device(const Device& device);
    Device* get_device(uint32_t device_id);
    void add_table(const Table& table);
    Table* get_table(uint32_t table_id);
    Column* get_column(uint32_t column_id);
    void add_trace_entry(const TraceEntry& entry);
    void clear_trace();
    void load_device_model(const DeviceModel& model);
    DeviceModel* get_device_model(uint32_t device_id);

    std::unordered_map<uint32_t, Device> devices;
    std::unordered_map<uint32_t, Table> tables;
    std::unordered_map<uint32_t, Column> columns;
    std::vector<TraceEntry> trace;
    std::unordered_map<uint32_t, DeviceModel> device_models;
};

} // namespace Mosaic