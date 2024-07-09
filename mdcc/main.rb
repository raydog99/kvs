require_relative 'mosaic'
require_relative 'ycsb'

module Legostore
  def self.run_demo
    mosaic = Mosaic.new
    mosaic.setup_devices
    mosaic.setup_tables

    sample_queries = [
      "1:1,2,3", # Select all columns from customer
      "2:4,5",   # Select o_orderkey and o_custkey from orders
      "1:1;2:5"  # Join customer and orders on customer key
    ]

    puts "Running Mosaic with a budget of 500:"
    mosaic.run_mosaic(500.0, sample_queries)

    puts "\nMosaic demonstration completed."

    puts "\nRunning YCSB benchmark:"
    benchmark = YCSBBenchmark.new(100000, 1000000)
    benchmark.run_benchmark(mosaic.kv_store)
  end
end

Legostore.run_demo