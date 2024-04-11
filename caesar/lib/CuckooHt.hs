module Main where

import qualified Data.Vector as V

data CuckooHashTable a = CuckooHashTable
  { tableSize :: Int,
    numTables :: Int,
    tables :: V.Vector (V.Vector a)
  }

hash1 :: Int -> Int -> Int
hash1 x tableSize = x `mod` tableSize

hash2 :: Int -> Int -> Int
hash2 x tableSize = (x `div` tableSize) `mod` tableSize

newCuckooHashTable :: Num a => Int -> CuckooHashTable a
newCuckooHashTable size =
  let numTables = 2
      tableSize = size
      tables =
        V.replicate
          numTables
          (V.replicate tableSize 0)
   in CuckooHashTable tableSize numTables tables

insert :: (Eq a, Num a) => CuckooHashTable a -> a -> CuckooHashTable a
insert (CuckooHashTable tableSize numTables tables) x =
  let i1 = hash1 x tableSize
      i2 = hash2 x tableSize
      (table1, table2) = (tables V.! 0, tables V.! 1)
   in if table1 V.! i1 == 0
        then CuckooHashTable tableSize numTables (tables V.// [(0, table1 V.// [(i1, x)])])
        else if table2 V.! i2 == 0
          then CuckooHashTable tableSize numTables (tables V.// [(1, table2 V.// [(i2, x)])])
          else
            let y = table1 V.! i1
                table1' = table1 V.// [(i1, x)]
             in insert (CuckooHashTable tableSize numTables (tables V.// [(0, table1')])) y

find :: (Eq a, Num a) => CuckooHashTable a -> a -> Bool
find (CuckooHashTable tableSize numTables tables) x =
  let i1 = hash1 x tableSize
      i2 = hash2 x tableSize
      (table1, table2) = (tables V.! 0, tables V.! 1)
   in table1 V.! i1 == x || table2 V.! i2 == x

main :: IO ()
main = return ()