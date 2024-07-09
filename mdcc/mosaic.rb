require_relative 'metadata'
require_relative 'kv_store'
require_relative 'data_retriever'
require_relative 'strategies'

module Legostore
  class Mosaic
    attr_reader :metadata, :kv_store, :data_retriever

    def initialize
      @metadata = Metadata.new
      @kv_store = KVStore.new
      @data_retriever = DataRetriever.new
    end

    def setup_devices
      @metadata.add_device(Device.new(1, "/mnt/nvme", "NVMe SSD", 1000, 8, COMPRESSION_ZSTD, 0.25))
      @metadata.add_device(Device.new(2, "/mnt/ssd", "SATA SSD", 2000, 4, COMPRESSION_LZ4, 0.15))
      @metadata.add_device(Device.new(3, "/mnt/hdd", "HDD", 4000, 1, COMPRESSION_NONE, 0.05))
    end

    def setup_tables
      customer_columns = [
        Column.new(1, 1, "c_custkey", "INT", 4 * 150000),
        Column.new(2, 1, "c_name", "VARCHAR", 25 * 150000),
        Column.new(3, 1, "c_address", "VARCHAR", 40 * 150000)
      ]
      @metadata.add_table(Table.new(1, "customer", customer_columns))

      orders_columns = [
        Column.new(4, 2, "o_orderkey", "INT", 4 * 1500000),
        Column.new(5, 2, "o_custkey", "INT", 4 * 1500000),
        Column.new(6, 2, "o_totalprice", "DECIMAL", 8 * 1500000)
      ]
      @metadata.add_table(Table.new(2, "orders", orders_columns))
    end

    def parse_query(query_str)
      table_id, column_ids = query_str.split(':')
      TraceEntry.new(table_id.to_i, column_ids.split(',').map(&:to_i))
    end

    def run_query(query_str)
      query = parse_query(query_str)
      @data_retriever.fetch_table_scan(query.table_id, query.column_ids, @metadata, @kv_store)
      result = @data_retriever.get_buffer_data
      puts "Query executed: #{query_str}"
      puts "Retrieved #{result.size} columns"
    end

    def optimize_storage(budget)
      placements = Strategies.optimize_placement(budget, @metadata)
      Strategies.apply_placements(placements, @metadata, @kv_store, @data_retriever)
    end

    def run_mosaic(budget, queries)
      puts "Initial benchmark..."
      initial_time = benchmark(queries)
      puts "Initial execution time: #{initial_time} s"

      puts "\nOptimizing storage..."
      optimize_storage(budget)

      puts "\nRunning benchmark after optimization..."
      optimized_time = benchmark(queries)
      puts "Optimized execution time: #{optimized_time} s"

      improvement = (initial_time - optimized_time) / initial_time * 100
      puts "\nImprovement: #{improvement}%"
    end

    def benchmark(queries)
      start_time = Time.now
      queries.each { |query| run_query(query) }
      Time.now - start_time
    end
  end
end