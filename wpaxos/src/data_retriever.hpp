#pragma once

#include <vector>
#include <unordered_map>
#include "metadata.hpp"
#include "kv_store.hpp"

namespace Mosaic {

struct ColumnChunk {
    uint32_t column_id;
    uint32_t device_id;
    uint64_t size;
    uint64_t compressed_size;
    CompressionAlgorithm compression;
};

class DataRetriever {
public:
    void retrieve_column_chunk(const ColumnChunk& chunk, Metadata& metadata, KVStore& kv_store);
    void fetch_table_scan(uint32_t table_id, const std::vector<uint32_t>& column_ids, Metadata& metadata, KVStore& kv_store);
    std::vector<std::pair<uint32_t, std::vector<uint8_t>>> get_buffer_data();
    void move_column_chunk(const ColumnChunk& chunk, uint32_t target_device_id, Metadata& metadata, KVStore& kv_store);

    std::unordered_map<uint32_t, std::vector<ColumnChunk>> column_chunks;

private:
    std::vector<uint8_t> compress_column_chunk(const ColumnChunk& chunk, const std::vector<uint8_t>& data);
    std::vector<uint8_t> decompress_column_chunk(const ColumnChunk& chunk, const std::vector<uint8_t>& data);

    std::vector<std::pair<uint32_t, std::vector<uint8_t>>> buffer;
    uint64_t buffer_size = 0;
};

} // namespace Mosaic