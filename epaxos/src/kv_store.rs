use std::collections::HashMap;
use crate::metadata::Column;

pub struct KVStore {
    store: HashMap<String, Vec<u8>>,
}

impl KVStore {
    pub fn new() -> Self {
        KVStore {
            store: HashMap::new(),
        }
    }

    pub fn put(&mut self, key: String, value: Vec<u8>) {
        self.store.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&Vec<u8>> {
        self.store.get(key)
    }

    pub fn delete(&mut self, key: &str) {
        self.store.remove(key);
    }

    pub fn generate_column_key(column: &Column) -> String {
        format!("{}:{}:{}", column.table_id, column.id, column.name)
    }
}