#pragma once

#include <unordered_map>
#include <string>
#include <vector>
#include "metadata.hpp"

namespace Mosaic {

class KVStore {
public:
    void put(const std::string& key, const std::vector<uint8_t>& value);
    std::vector<uint8_t> get(const std::string& key);
    void remove(const std::string& key);

    static std::string generate_column_key(const Column& column);

private:
    std::unordered_map<std::string, std::vector<uint8_t>> store;
};

} // namespace Mosaic