require 'securerandom'

module Legostore
  class YCSBWorkload
    attr_reader :name, :read_proportion, :update_proportion, :insert_proportion

    def initialize(name, read_proportion, update_proportion, insert_proportion)
      @name = name
      @read_proportion = read_proportion
      @update_proportion = update_proportion
      @insert_proportion = insert_proportion
    end
  end

  class YCSBBenchmark
    def initialize(record_count, operation_count)
      @record_count = record_count
      @operation_count = operation_count
      @key_length = 10
      @value_length = 100
    end

    def generate_key
      SecureRandom.alphanumeric(@key_length)
    end

    def generate_value
      SecureRandom.bytes(@value_length)
    end

    def load_phase(kv_store)
      @record_count.times do
        key = generate_key
        value = generate_value
        kv_store.put(key, value)
      end

    def run_workload(workload, kv_store)
      latencies = []
      start_time = Time.now

      @operation_count.times do
        r = rand
        op_start_time = Time.now

        if r < workload.read_proportion
          key = generate_key
          kv_store.get(key)
        elsif r < workload.read_proportion + workload.update_proportion
          key = generate_key
          value = generate_value
          kv_store.put(key, value)
        else
          key = generate_key
          value = generate_value
          kv_store.put(key, value)
        end

        latencies << (Time.now - op_start_time)
      end

      total_time = Time.now - start_time
      [total_time, latencies]
    end

    def run_benchmark(kv_store)
      puts "Loading data..."
      load_phase(kv_store)

      workloads = [
        YCSBWorkload.new("Workload A (50% read, 50% update)", 0.5, 0.5, 0.0),
        YCSBWorkload.new("Workload B (95% read, 5% update)", 0.95, 0.05, 0.0),
        YCSBWorkload.new("Workload C (100% read)", 1.0, 0.0, 0.0),
        YCSBWorkload.new("Workload D (95% read, 5% insert)", 0.95, 0.0, 0.05),
        YCSBWorkload.new("Workload E (95% scan, 5% insert)", 0.95, 0.0, 0.05), # Treating scan as read
        YCSBWorkload.new("Workload F (50% read, 50% read-modify-write)", 0.5, 0.5, 0.0)
      ]

      workloads.each do |workload|
        puts "\nRunning #{workload.name}"
        total_time, latencies = run_workload(workload, kv_store)

        avg_latency = latencies.sum / latencies.size
        throughput = @operation_count / total_time

        puts "Throughput: #{throughput.round(2)} ops/sec"
        puts "Average latency: #{(avg_latency * 1000).round(2)} ms"

        sorted_latencies = latencies.sort
        p95 = sorted_latencies[(sorted_latencies.size * 0.95).floor]
        p99 = sorted_latencies[(sorted_latencies.size * 0.99).floor]

        puts "95th percentile latency: #{(p95 * 1000).round(2)} ms"
        puts "99th percentile latency: #{(p99 * 1000).round(2)} ms"
      end
    end
  end
end