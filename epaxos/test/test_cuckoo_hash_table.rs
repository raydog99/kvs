#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_and_find() {
        let mut table = CuckooHashTable::new();
        
        table.insert(5);
        table.insert(10);

        assert!(table.find(5));
        assert!(table.find(10));
        assert!(!table.find(15));
    }

}