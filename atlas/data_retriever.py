import lz4.frame
import zstandard
import pyarrow.parquet as pq
import pyarrow as pa
from typing import List, Dict
from dataclasses import dataclass
from metadata import metadata, Column, CompressionAlgorithm
from kv_store import kv_store

@dataclass
class ColumnChunk:
    column_id: int
    device_id: int
    size: int
    compressed_size: int
    compression: CompressionAlgorithm

class DataRetriever:
    def __init__(self):
        self.column_chunks: Dict[int, List[ColumnChunk]] = {}
        self.buffer: List[tuple[int, bytes]] = []
        self.buffer_size: int = 0

    def compress_column_chunk(self, chunk: ColumnChunk, data: bytes) -> bytes:
        if chunk.compression == CompressionAlgorithm.NONE:
            return data
        elif chunk.compression == CompressionAlgorithm.LZ4:
            return lz4.frame.compress(data)
        elif chunk.compression == CompressionAlgorithm.ZSTD:
            cctx = zstandard.ZstdCompressor(level=3)
            return cctx.compress(data)

    def decompress_column_chunk(self, chunk: ColumnChunk, data: bytes) -> bytes:
        if chunk.compression == CompressionAlgorithm.NONE:
            return data
        elif chunk.compression == CompressionAlgorithm.LZ4:
            return lz4.frame.decompress(data)
        elif chunk.compression == CompressionAlgorithm.ZSTD:
            dctx = zstandard.ZstdDecompressor()
            return dctx.decompress(data, max_output_size=chunk.size)

    def retrieve_column_chunk(self, chunk: ColumnChunk):
        device = metadata.get_device(chunk.device_id)
        compressed_data = kv_store.get(kv_store.generate_column_key(metadata.get_column(chunk.column_id)))
        decompressed = self.decompress_column_chunk(chunk, compressed_data)
        self.buffer.append((chunk.column_id, decompressed))
        self.buffer_size += len(decompressed)

    def fetch_table_scan(self, table_id: int, column_ids: List[int]):
        for col_id in column_ids:
            chunks = self.column_chunks.get(col_id, [])
            for chunk in chunks:
                self.retrieve_column_chunk(chunk)

    def get_buffer_data(self) -> List[tuple[int, bytes]]:
        data = self.buffer
        self.buffer = []
        self.buffer_size = 0
        return data

    def move_column_chunk(self, chunk: ColumnChunk, target_device_id: int):
        source_device = metadata.get_device(chunk.device_id)
        target_device = metadata.get_device(target_device_id)
        
        compressed_data = kv_store.get(kv_store.generate_column_key(metadata.get_column(chunk.column_id)))
        data = self.decompress_column_chunk(chunk, compressed_data)
        
        new_compressed_data = self.compress_column_chunk(
            ColumnChunk(chunk.column_id, target_device_id, chunk.size, 0, target_device.compression),
            data
        )
        
        new_chunk = ColumnChunk(
            chunk.column_id,
            target_device_id,
            chunk.size,
            len(new_compressed_data),
            target_device.compression
        )
        
        kv_store.put(kv_store.generate_column_key(metadata.get_column(chunk.column_id)), new_compressed_data)
        
        chunks = self.column_chunks.get(chunk.column_id, [])
        updated_chunks = [new_chunk if c == chunk else c for c in chunks]
        self.column_chunks[chunk.column_id] = updated_chunks
        
        kv_store.delete(kv_store.generate_column_key(metadata.get_column(chunk.column_id)))

    def load_parquet_file(self, filename: str, table_id: int, device_id: int):
        table = pq.read_table(filename)
        device = metadata.get_device(device_id)
        table_metadata = metadata.get_table(table_id)
        
        for i, column in enumerate(table_metadata.columns):
            data = table.column(column.name).to_pandas().values.tobytes()
            compressed_data = self.compress_column_chunk(
                ColumnChunk(column.id, device_id, len(data), 0, device.compression),
                data
            )
            chunk = ColumnChunk(
                column.id,
                device_id,
                len(data),
                len(compressed_data),
                device.compression
            )
            kv_store.put(kv_store.generate_column_key(column), compressed_data)
            self.column_chunks.setdefault(column.id, []).append(chunk)

    def save_to_parquet(self, filename: str, table_id: int):
        table = metadata.get_table(table_id)
        data = {}
        
        for column in table.columns:
            chunks = self.column_chunks.get(column.id, [])
            column_data = b''.join(
                self.decompress_column_chunk(chunk, kv_store.get(kv_store.generate_column_key(column)))
                for chunk in chunks
            )
            data[column.name] = pa.array(column_data)
        
        pa_table = pa.Table.from_pydict(data)
        pq.write_table(pa_table, filename)