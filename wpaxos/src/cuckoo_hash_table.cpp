#include <iostream>
#include <vector>

const int TABLE_SIZE = 8;
const int NUM_TABLES = 2;

int hash1(int x) {
    return x % TABLE_SIZE;
}

int hash2(int x) {
    return (x / TABLE_SIZE) % TABLE_SIZE;
}

class CuckooHashTable {
private:
    std::vector<std::vector<int>> tables;

public:
    CuckooHashTable() : tables(NUM_TABLES, std::vector<int>(TABLE_SIZE, 0)) {}

    void insert(int x) {
        int i1 = hash1(x), i2 = hash2(x);
        if (tables[0][i1] == 0) {
            tables[0][i1] = x;
        } else if (tables[1][i2] == 0) {
            tables[1][i2] = x;
        } else {
            int y = tables[0][i1];
            tables[0][i1] = x;
            insert(y);
        }
    }

    bool find(int x) {
        int i1 = hash1(x), i2 = hash2(x);
        return tables[0][i1] == x || tables[1][i2] == x;
    }
};