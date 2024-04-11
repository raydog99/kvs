class CuckooHashTable:
    def __init__(self):
        self.TABLE_SIZE = 8
        self.NUM_TABLES = 2
        self.tables = [
            [None] * self.TABLE_SIZE,
            [None] * self.TABLE_SIZE
        ]

    def hash1(self, x):
        return x % self.TABLE_SIZE

    def hash2(self, x):
        return (x // self.TABLE_SIZE) % self.TABLE_SIZE

    def insert(self, x):
        i1 = self.hash1(x)
        i2 = self.hash2(x)

        if self.tables[0][i1] is None:
            self.tables[0][i1] = x
        elif self.tables[1][i2] is None:
            self.tables[1][i2] = x
        else:
            y = self.tables[0][i1]
            self.tables[0][i1] = x
            self.insert(y)

    def find(self, x):
        i1 = self.hash1(x)
        i2 = self.hash2(x)
        return x == self.tables[0][i1] or x == self.tables[1][i2]