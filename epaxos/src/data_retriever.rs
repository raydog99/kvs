use std::collections::HashMap;
use crate::metadata::{Metadata, CompressionAlgorithm};
use crate::kv_store::KVStore;

#[derive(Clone)]
pub struct ColumnChunk {
    pub column_id: u32,
    pub device_id: u32,
    pub size: u64,
    pub compressed_size: u64,
    pub compression: CompressionAlgorithm,
}

pub struct DataRetriever {
    pub column_chunks: HashMap<u32, Vec<ColumnChunk>>,
    pub buffer: Vec<(u32, Vec<u8>)>,
    pub buffer_size: u64,
}

impl DataRetriever {
    pub fn new() -> Self {
        DataRetriever {
            column_chunks: HashMap::new(),
            buffer: Vec::new(),
            buffer_size: 0,
        }
    }

    pub fn compress_column_chunk(&self, chunk: &ColumnChunk, data: &[u8]) -> Vec<u8> {
        match chunk.compression {
            CompressionAlgorithm::None => data.to_vec(),
            CompressionAlgorithm::LZ4 => {
                data.to_vec()
            }
            CompressionAlgorithm::ZSTD => {
                data.to_vec()
            }
        }
    }

    pub fn decompress_column_chunk(&self, chunk: &ColumnChunk, data: &[u8]) -> Vec<u8> {
        match chunk.compression {
            CompressionAlgorithm::None => data.to_vec(),
            CompressionAlgorithm::LZ4 => {
                data.to_vec()
            }
            CompressionAlgorithm::ZSTD => {
                data.to_vec()
            }
        }
    }

    pub fn retrieve_column_chunk(&mut self, chunk: &ColumnChunk, metadata: &Metadata, kv_store: &KVStore) {
        if let Some(column) = metadata.get_column(chunk.column_id) {
            let key = KVStore::generate_column_key(column);
            if let Some(compressed_data) = kv_store.get(&key) {
                let decompressed_data = self.decompress_column_chunk(chunk, compressed_data);
                self.buffer.push((chunk.column_id, decompressed_data.clone()));
                self.buffer_size += decompressed_data.len() as u64;
            }
        }
    }

    pub fn fetch_table_scan(&mut self, table_id: u32, column_ids: &[u32], metadata: &Metadata, kv_store: &KVStore) {
        let chunks_to_retrieve: Vec<_> = column_ids
            .iter()
            .filter_map(|&col_id| self.column_chunks.get(&col_id))
            .flatten()
            .cloned()
            .collect();

        for chunk in chunks_to_retrieve {
            self.retrieve_column_chunk(&chunk, metadata, kv_store);
        }
    }

    pub fn get_buffer_data(&mut self) -> Vec<(u32, Vec<u8>)> {
        let data = self.buffer.clone();
        self.buffer.clear();
        self.buffer_size = 0;
        data
    }

    pub fn move_column_chunk(&mut self, chunk: &ColumnChunk, target_device_id: u32, metadata: &Metadata, kv_store: &mut KVStore) {
        if let Some(column) = metadata.get_column(chunk.column_id) {
            let key = KVStore::generate_column_key(column);
            if let Some(compressed_data) = kv_store.get(&key) {
                let decompressed_data = self.decompress_column_chunk(chunk, compressed_data);
                
                let target_device = metadata.get_device(target_device_id).unwrap();
                let new_chunk = ColumnChunk {
                    column_id: chunk.column_id,
                    device_id: target_device_id,
                    size: chunk.size,
                    compressed_size: 0,
                    compression: target_device.compression.clone(),
                };
                
                let new_compressed_data = self.compress_column_chunk(&new_chunk, &decompressed_data);
                let new_chunk = ColumnChunk {
                    compressed_size: new_compressed_data.len() as u64,
                    ..new_chunk
                };
                
                kv_store.put(key, new_compressed_data);
                
                if let Some(chunks) = self.column_chunks.get_mut(&chunk.column_id) {
                    let index = chunks.iter().position(|c| c.device_id == chunk.device_id).unwrap();
                    chunks[index] = new_chunk;
                }
            }
        }
    }
}