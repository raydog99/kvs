#include "kv_store.hpp"

namespace Mosaic {

void KVStore::put(const std::string& key, const std::vector<uint8_t>& value) {
    store[key] = value;
}

std::vector<uint8_t> KVStore::get(const std::string& key) {
    auto it = store.find(key);
    return it != store.end() ? it->second : std::vector<uint8_t>();
}

void KVStore::remove(const std::string& key) {
    store.erase(key);
}

std::string KVStore::generate_column_key(const Column& column) {
    return std::to_string(column.table_id) + ":" + std::to_string(column.id) + ":" + column.name;
}

} // namespace Mosaic