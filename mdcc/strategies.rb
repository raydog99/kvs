module Legostore
  class Strategies
    HOT_TABLE = :hot_table
    HOT_COLUMN = :hot_column
    LOPT = :lopt

    Placement = Struct.new(:column_id, :device_id)

    def self.hot_table_strategy(metadata)
      table_access_counts = metadata.trace.each_with_object(Hash.new(0)) do |entry, counts|
        counts[entry.table_id] += 1
      end

      sorted_tables = table_access_counts.sort_by { |_, count| -count }
      sorted_devices = metadata.devices.values.sort_by { |device| -device.cost_per_gb }

      placements = []
      sorted_tables.each_with_index do |(table_id, _), i|
        table = metadata.get_table(table_id)
        device = sorted_devices[i % sorted_devices.size]
        table.columns.each do |column|
          placements << Placement.new(column.id, device.id)
        end
      end

      placements
    end

    def self.hot_column_strategy(metadata)
      column_access_counts = metadata.trace.each_with_object(Hash.new(0)) do |entry, counts|
        entry.column_ids.each { |col_id| counts[col_id] += 1 }
      end

      sorted_columns = column_access_counts.sort_by { |_, count| -count }
      sorted_devices = metadata.devices.values.sort_by { |device| -device.cost_per_gb }

      sorted_columns.map.with_index do |(col_id, _), i|
        Placement.new(col_id, sorted_devices[i % sorted_devices.size].id)
      end
    end

    def self.lopt_strategy(metadata, budget)
      column_sizes = metadata.columns.transform_values(&:size)
      sorted_columns = column_sizes.sort_by { |_, size| -size }
      sorted_devices = metadata.devices.values.sort_by(&:cost_per_gb)

      placements = []
      total_cost = 0

      sorted_columns.each do |col_id, size|
        sorted_devices.each do |device|
          cost = (size.to_f / (1024 * 1024 * 1024)) * device.cost_per_gb
          if !budget || total_cost + cost <= budget
            placements << Placement.new(col_id, device.id)
            total_cost += cost
            break
          end
        end
      end

      placements
    end

    def self.place_data(strategy, metadata, budget = nil)
      case strategy
      when HOT_TABLE then hot_table_strategy(metadata)
      when HOT_COLUMN then hot_column_strategy(metadata)
      when LOPT then lopt_strategy(metadata, budget)
      else []
      end
    end

    def self.predict_scan_time(placements, table_scan, metadata)
      device_times = Hash.new(0)

      table_scan.column_ids.each do |col_id|
        placement = placements.find { |p| p.column_id == col_id }
        next unless placement

        device = metadata.get_device(placement.device_id)
        device_model = metadata.get_device_model(placement.device_id)
        column = metadata.get_column(col_id)

        if device && device_model && column
          compression_throughput = case device.compression
                                   when COMPRESSION_NONE then device_model.none_throughput
                                   when COMPRESSION_LZ4 then device_model.lz4_throughput
                                   when COMPRESSION_ZSTD then device_model.zstd_throughput
                                   end

          column_time = device_model.seek_time + (column.size.to_f / compression_throughput)
          device_times[placement.device_id] += column_time
        end
      end

      device_times.values.max || 0
    end

    def self.predict_total_time(placements, metadata)
      metadata.trace.sum { |trace| predict_scan_time(placements, trace, metadata) }
    end

    def self.calculate_total_cost(placements, metadata)
      placements.sum do |p|
        device = metadata.get_device(p.device_id)
        column = metadata.get_column(p.column_id)
        if device && column
          device.cost_per_gb * (column.size.to_f / (1024 * 1024 * 1024))
        else
          0
        end
      end
    end

    def self.optimize_placement(budget, metadata)
      placements = place_data(LOPT, metadata, budget)
      total_time = predict_total_time(placements, metadata)
      total_cost = calculate_total_cost(placements, metadata)
      puts "Predicted total time: #{total_time} s"
      puts "Total cost: #{total_cost}"
      placements
    end

    def self.apply_placements(placements, metadata, kv_store, data_retriever)
      placements.each do |placement|
        column = metadata.get_column(placement.column_id)
        target_device = metadata.get_device(placement.device_id)
        if column && target_device
          chunks = data_retriever.column_chunks[placement.column_id] || []
          chunks.each do |chunk|
            if chunk.device_id != placement.device_id
              data_retriever.move_column_chunk(chunk, placement.device_id, metadata, kv_store)
              puts "Moved column #{column.name} to device #{target_device.name}"
            end
          end
        end
      end
    end
  end
end