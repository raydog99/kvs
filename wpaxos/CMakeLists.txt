cmake_minimum_required(VERSION 3.10)
project(WPaxos)

add_executable(main src/cuckoo_hash_table.cpp)

add_executable(test_cuckoo_hash_table test/test_cuckoo_hash_table.cpp)

target_include_directories(test_cuckoo_hash_table PRIVATE src)

enable_testing()
add_test(NAME CuckooHashTableTest COMMAND test_cuckoo_hash_table)