#include "metadata.hpp"

namespace Mosaic {

void Metadata::add_device(const Device& device) {
    devices[device.id] = device;
}

Device* Metadata::get_device(uint32_t device_id) {
    auto it = devices.find(device_id);
    return it != devices.end() ? &it->second : nullptr;
}

void Metadata::add_table(const Table& table) {
    tables[table.id] = table;
    for (const auto& column : table.columns) {
        columns[column.id] = column;
    }
}

Table* Metadata::get_table(uint32_t table_id) {
    auto it = tables.find(table_id);
    return it != tables.end() ? &it->second : nullptr;
}

Column* Metadata::get_column(uint32_t column_id) {
    auto it = columns.find(column_id);
    return it != columns.end() ? &it->second : nullptr;
}

void Metadata::add_trace_entry(const TraceEntry& entry) {
    trace.push_back(entry);
}

void Metadata::clear_trace() {
    trace.clear();
}

void Metadata::load_device_model(const DeviceModel& model) {
    device_models[model.device_id] = model;
}

DeviceModel* Metadata::get_device_model(uint32_t device_id) {
    auto it = device_models.find(device_id);
    return it != device_models.end() ? &it->second : nullptr;
}

} // namespace Mosaic