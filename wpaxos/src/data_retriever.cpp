#include "data_retriever.hpp"

namespace Mosaic {

void DataRetriever::retrieve_column_chunk(const ColumnChunk& chunk, Metadata& metadata, KVStore& kv_store) {
    Column* column = metadata.get_column(chunk.column_id);
    if (column) {
        std::string key = KVStore::generate_column_key(*column);
        std::vector<uint8_t> compressed_data = kv_store.get(key);
        if (!compressed_data.empty()) {
            std::vector<uint8_t> decompressed_data = decompress_column_chunk(chunk, compressed_data);
            buffer.emplace_back(chunk.column_id, decompressed_data);
            buffer_size += decompressed_data.size();
        }
    }
}

void DataRetriever::fetch_table_scan(uint32_t table_id, const std::vector<uint32_t>& column_ids, Metadata& metadata, KVStore& kv_store) {
    for (uint32_t col_id : column_ids) {
        auto it = column_chunks.find(col_id);
        if (it != column_chunks.end()) {
            for (const auto& chunk : it->second) {
                retrieve_column_chunk(chunk, metadata, kv_store);
            }
        }
    }
}

std::vector<std::pair<uint32_t, std::vector<uint8_t>>> DataRetriever::get_buffer_data() {
    std::vector<std::pair<uint32_t, std::vector<uint8_t>>> data = std::move(buffer);
    buffer.clear();
    buffer_size = 0;
    return data;
}

void DataRetriever::move_column_chunk(const ColumnChunk& chunk, uint32_t target_device_id, Metadata& metadata, KVStore& kv_store) {
    Column* column = metadata.get_column(chunk.column_id);
    if (column) {
        std::string key = KVStore::generate_column_key(*column);
        std::vector<uint8_t> compressed_data = kv_store.get(key);
        if (!compressed_data.empty()) {
            std::vector<uint8_t> decompressed_data = decompress_column_chunk(chunk, compressed_data);
            Device* target_device = metadata.get_device(target_device_id);
            ColumnChunk new_chunk = {chunk.column_id, target_device_id, chunk.size, 0, target_device->compression};
            std::vector<uint8_t> new_compressed_data = compress_column_chunk(new_chunk, decompressed_data);
            new_chunk.compressed_size = new_compressed_data.size();
            kv_store.put(key, new_compressed_data);

            auto& chunks = column_chunks[chunk.column_id];
            for (auto& c : chunks) {
                if (c.device_id == chunk.device_id) {
                    c = new_chunk;
                    break;
                }
            }
        }
    }
}

std::vector<uint8_t> DataRetriever::compress_column_chunk(const ColumnChunk& chunk, const std::vector<uint8_t>& data) {
    return data;
}

std::vector<uint8_t> DataRetriever::decompress_column_chunk(const ColumnChunk& chunk, const std::vector<uint8_t>& data) {
    return data;
}

} // namespace Mosaic