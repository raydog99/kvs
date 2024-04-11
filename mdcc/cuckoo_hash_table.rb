class CuckooHashTable
  TABLE_SIZE = 8
  NUM_TABLES = 2

  def initialize
    @tables = Array.new(NUM_TABLES) { Array.new(TABLE_SIZE, nil) }
  end

  def hash1(x)
    x % TABLE_SIZE
  end

  def hash2(x)
    (x / TABLE_SIZE) % TABLE_SIZE
  end

  def insert(x)
    i1 = hash1(x)
    i2 = hash2(x)

    if @tables[0][i1].nil?
      @tables[0][i1] = x
    elsif @tables[1][i2].nil?
      @tables[1][i2] = x
    else
      y = @tables[0][i1]
      @tables[0][i1] = x
      insert(y)
    end
  end

  def find(x)
    i1 = hash1(x)
    i2 = hash2(x)
    @tables[0][i1] == x || @tables[1][i2] == x
  end
end