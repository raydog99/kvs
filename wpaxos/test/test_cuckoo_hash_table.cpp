#include "../src/cuckoo_hash_table.h"
#include <cassert>

void test_insert_and_find() {
    CuckooHashTable hashTable;
    hashTable.insert(42);
    assert(hashTable.find(42));
}

int main() {
    test_insert_and_find();
    return 0;
}