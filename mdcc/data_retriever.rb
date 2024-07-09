module Legostore
  ColumnChunk = Struct.new(:column_id, :device_id, :size, :compressed_size, :compression)

  class DataRetriever
    attr_reader :column_chunks, :buffer, :buffer_size

    def initialize
      @column_chunks = {}
      @buffer = []
      @buffer_size = 0
    end

    def compress_column_chunk(chunk, data)
      data
    end

    def decompress_column_chunk(chunk, data)
      data
    end

    def retrieve_column_chunk(chunk, metadata, kv_store)
      column = metadata.get_column(chunk.column_id)
      if column
        key = KVStore.generate_column_key(column)
        compressed_data = kv_store.get(key)
        if compressed_data
          decompressed_data = decompress_column_chunk(chunk, compressed_data)
          @buffer << [chunk.column_id, decompressed_data]
          @buffer_size += decompressed_data.bytesize
        end
      end
    end

    def fetch_table_scan(table_id, column_ids, metadata, kv_store)
      column_ids.each do |col_id|
        chunks = @column_chunks[col_id] || []
        chunks.each { |chunk| retrieve_column_chunk(chunk, metadata, kv_store) }
      end
    end

    def get_buffer_data
      data = @buffer
      @buffer = []
      @buffer_size = 0
      data
    end

    def move_column_chunk(chunk, target_device_id, metadata, kv_store)
      column = metadata.get_column(chunk.column_id)
      if column
        key = KVStore.generate_column_key(column)
        compressed_data = kv_store.get(key)
        if compressed_data
          decompressed_data = decompress_column_chunk(chunk, compressed_data)
          target_device = metadata.get_device(target_device_id)
          new_chunk = ColumnChunk.new(chunk.column_id, target_device_id, chunk.size, 0, target_device.compression)
          new_compressed_data = compress_column_chunk(new_chunk, decompressed_data)
          new_chunk.compressed_size = new_compressed_data.bytesize
          kv_store.put(key, new_compressed_data)
          @column_chunks[chunk.column_id] = @column_chunks[chunk.column_id].map do |c|
            c.device_id == chunk.device_id ? new_chunk : c
          end
        end
      end
    end
  end
end