module Legostore
  class KVStore
    def initialize
      @store = {}
    end

    def put(key, value)
      @store[key] = value
    end

    def get(key)
      @store[key]
    end

    def delete(key)
      @store.delete(key)
    end

    def self.generate_column_key(column)
      "#{column.table_id}:#{column.id}:#{column.name}"
    end
  end
end