module Mosaic where

import qualified Data.Map as Map
import Data.List (intercalate)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Metadata
import KVStore
import DataRetriever
import Strategies

parseQuery :: String -> TraceEntry
parseQuery query =
  let [tableIdStr, columnIdsStr] = splitOn ":" query
      tableId = read tableIdStr
      columnIds = map read $ splitOn "," columnIdsStr
  in TraceEntry tableId columnIds

runQuery :: String -> Metadata -> KVStore -> DataRetriever -> IO DataRetriever
runQuery queryStr meta kvstore retriever = do
  let query = parseQuery queryStr
  let updatedRetriever = fetchTableScan (traceTableId query) (traceColumnIds query) meta kvstore retriever
  let (result, newRetriever) = getBufferData updatedRetriever
  putStrLn $ "Query executed: " ++ queryStr
  putStrLn $ "Retrieved " ++ show (length result) ++ " columns"
  return newRetriever

optimizeStorage :: Float -> Metadata -> KVStore -> DataRetriever -> IO (KVStore, DataRetriever)
optimizeStorage budget meta kvstore retriever = do
  placements <- optimizePlacement budget meta
  applyPlacements placements meta kvstore retriever

benchmark :: [String] -> Metadata -> KVStore -> DataRetriever -> IO (Float, DataRetriever)
benchmark queries meta kvstore retriever = do
  startTime <- getCurrentTime
  finalRetriever <- foldM (\ret query -> runQuery query meta kvstore ret) retriever queries
  endTime <- getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime
  return (duration, finalRetriever)

runMosaic :: Float -> [String] -> Metadata -> KVStore -> DataRetriever -> IO ()
runMosaic budget queries meta kvstore retriever = do
  putStrLn "Initial benchmark..."
  (initialTime, updatedRetriever1) <- benchmark queries meta kvstore retriever
  putStrLn $ "Initial execution time: " ++ show initialTime ++ " s"

  putStrLn "\nOptimizing storage..."
  (optimizedKVStore, optimizedRetriever) <- optimizeStorage budget meta kvstore updatedRetriever1

  putStrLn "\nRunning benchmark after optimization..."
  (optimizedTime, _) <- benchmark queries meta optimizedKVStore optimizedRetriever
  putStrLn $ "Optimized execution time: " ++ show optimizedTime ++ " s"

  let improvement = (initialTime - optimizedTime) / initialTime * 100
  putStrLn $ "\nImprovement: " ++ show improvement ++ "%"

setupDevices :: Metadata -> Metadata
setupDevices = addDevice (Device 1 "/mnt/nvme" "NVMe SSD" 1000 8 ZSTD 0.25)
             . addDevice (Device 2 "/mnt/ssd" "SATA SSD" 2000 4 LZ4 0.15)
             . addDevice (Device 3 "/mnt/hdd" "HDD" 4000 1 None 0.05)

setupTables :: Metadata -> Metadata
setupTables meta =
  let customerColumns = [ Column 1 1 "c_custkey" "INT" (4 * 150000)
                        , Column 2 1 "c_name" "VARCHAR" (25 * 150000)
                        , Column 3 1 "c_address" "VARCHAR" (40 * 150000)
                        ]
      ordersColumns = [ Column 4 2 "o_orderkey" "INT" (4 * 1500000)
                      , Column 5 2 "o_custkey" "INT" (4 * 1500000)
                      , Column 6 2 "o_totalprice" "DECIMAL" (8 * 1500000)
                      ]
  in addTable (Table 1 "customer" customerColumns)
   . addTable (Table 2 "orders" ordersColumns)
   $ meta

main :: IO ()
main = do
  let meta = setupTables $ setupDevices emptyMetadata
  let kvstore = emptyKVStore
  let retriever = emptyDataRetriever

  let sampleQueries = [ "1:1,2,3"  -- Select all columns from customer
                      , "2:4,5"    -- Select o_orderkey and o_custkey from orders
                      , "1:1;2:5"  -- Join customer and orders on customer key
                      ]

  putStrLn "Running Mosaic with a budget of 500:"
  runMosaic 500.0 sampleQueries meta kvstore retriever

  putStrLn "\nMosaic demonstration completed."