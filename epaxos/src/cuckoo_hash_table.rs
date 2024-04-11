const TABLE_SIZE: usize = 8;
const NUM_TABLES: usize = 2;

fn hash1(x: usize) -> usize {
    x % TABLE_SIZE
}

fn hash2(x: usize) -> usize {
    (x / TABLE_SIZE) % TABLE_SIZE
}

struct CuckooHashTable {
    tables: [Vec<Option<usize>>; NUM_TABLES],
}

impl CuckooHashTable {
    fn new() -> CuckooHashTable {
        CuckooHashTable {
            tables: [
                vec![None; TABLE_SIZE],
                vec![None; TABLE_SIZE],
            ],
        }
    }

    fn insert(&mut self, x: usize) {
        let i1 = hash1(x);
        let i2 = hash2(x);

        if self.tables[0][i1].is_none() {
            self.tables[0][i1] = Some(x);
        } else if self.tables[1][i2].is_none() {
            self.tables[1][i2] = Some(x);
        } else {
            let y = self.tables[0][i1].unwrap();
            self.tables[0][i1] = Some(x);
            self.insert(y);
        }
    }

    fn find(&self, x: usize) -> bool {
        let i1 = hash1(x);
        let i2 = hash2(x);

        self.tables[0][i1].map_or(false, |y| y == x)
            || self.tables[1][i2].map_or(false, |y| y == x)
    }
}
