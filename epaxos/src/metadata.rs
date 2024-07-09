use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum CompressionAlgorithm {
    None,
    LZ4,
    ZSTD,
}

#[derive(Debug, Clone)]
pub struct Device {
    pub id: u32,
    pub mnt: String,
    pub name: String,
    pub capacity: u64,
    pub threads: u32,
    pub compression: CompressionAlgorithm,
    pub cost_per_gb: f32,
}

#[derive(Debug, Clone)]
pub struct Column {
    pub id: u32,
    pub table_id: u32,
    pub name: String,
    pub data_type: String,
    pub size: u64,
}

#[derive(Debug, Clone)]
pub struct Table {
    pub id: u32,
    pub name: String,
    pub columns: Vec<Column>,
}

#[derive(Debug, Clone)]
pub struct TraceEntry {
    pub table_id: u32,
    pub column_ids: Vec<u32>,
}

#[derive(Debug, Clone)]
pub struct DeviceModel {
    pub device_id: u32,
    pub none_throughput: f32,
    pub lz4_throughput: f32,
    pub zstd_throughput: f32,
    pub seek_time: f32,
}

pub struct Metadata {
    pub devices: HashMap<u32, Device>,
    pub tables: HashMap<u32, Table>,
    pub columns: HashMap<u32, Column>,
    pub trace: Vec<TraceEntry>,
    pub device_models: HashMap<u32, DeviceModel>,
}

impl Metadata {
    pub fn new() -> Self {
        Metadata {
            devices: HashMap::new(),
            tables: HashMap::new(),
            columns: HashMap::new(),
            trace: Vec::new(),
            device_models: HashMap::new(),
        }
    }

    pub fn add_device(&mut self, device: Device) {
        self.devices.insert(device.id, device);
    }

    pub fn get_device(&self, device_id: u32) -> Option<&Device> {
        self.devices.get(&device_id)
    }

    pub fn add_table(&mut self, table: Table) {
        for column in &table.columns {
            self.columns.insert(column.id, column.clone());
        }
        self.tables.insert(table.id, table);
    }

    pub fn get_table(&self, table_id: u32) -> Option<&Table> {
        self.tables.get(&table_id)
    }

    pub fn get_column(&self, column_id: u32) -> Option<&Column> {
        self.columns.get(&column_id)
    }

    pub fn add_trace_entry(&mut self, entry: TraceEntry) {
        self.trace.push(entry);
    }

    pub fn clear_trace(&mut self) {
        self.trace.clear();
    }

    pub fn load_device_model(&mut self, model: DeviceModel) {
        self.device_models.insert(model.device_id, model);
    }

    pub fn get_device_model(&self, device_id: u32) -> Option<&DeviceModel> {
        self.device_models.get(&device_id)
    }
}