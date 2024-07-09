module Legostore
  CompressionAlgorithm = Struct.new(:name)
  COMPRESSION_NONE = CompressionAlgorithm.new(:none)
  COMPRESSION_LZ4 = CompressionAlgorithm.new(:lz4)
  COMPRESSION_ZSTD = CompressionAlgorithm.new(:zstd)

  Device = Struct.new(:id, :mnt, :name, :capacity, :threads, :compression, :cost_per_gb)
  Column = Struct.new(:id, :table_id, :name, :data_type, :size)
  Table = Struct.new(:id, :name, :columns)
  TraceEntry = Struct.new(:table_id, :column_ids)
  DeviceModel = Struct.new(:device_id, :none_throughput, :lz4_throughput, :zstd_throughput, :seek_time)

  class Metadata
    attr_reader :devices, :tables, :columns, :trace, :device_models

    def initialize
      @devices = {}
      @tables = {}
      @columns = {}
      @trace = []
      @device_models = {}
    end

    def add_device(device)
      @devices[device.id] = device
    end

    def get_device(device_id)
      @devices[device_id]
    end

    def add_table(table)
      @tables[table.id] = table
      table.columns.each { |column| @columns[column.id] = column }
    end

    def get_table(table_id)
      @tables[table_id]
    end

    def get_column(column_id)
      @columns[column_id]
    end

    def add_trace_entry(entry)
      @trace << entry
    end

    def clear_trace
      @trace.clear
    end

    def load_device_model(model)
      @device_models[model.device_id] = model
    end

    def get_device_model(device_id)
      @device_models[device_id]
    end
  end
end