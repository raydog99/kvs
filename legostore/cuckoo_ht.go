package main

const tableSize = 8
const numTables = 2

var tables [numTables][]int

func hash1(x int) int {
    return x % tableSize
}

func hash2(x int) int {
    return (x / tableSize) % tableSize
}

func insert(x int) {
    i1, i2 := hash1(x), hash2(x)
    if tables[0][i1] == 0 {
        tables[0][i1] = x
    } else if tables[1][i2] == 0 {
        tables[1][i2] = x
    } else {
        y := tables[0][i1]
        tables[0][i1] = x
        insert(y)
    }
}

func find(x int) bool {
    i1, i2 := hash1(x), hash2(x)
    return tables[0][i1] == x || tables[1][i2] == x
}