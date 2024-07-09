module MosaicYCSBBenchmark where

import Mosaic
import YCSBBenchmark
import Metadata
import KVStore
import DataRetriever
import Strategies

setupYCSBTable :: Metadata -> Metadata
setupYCSBTable meta =
  let ycsbColumns = [ Column 1 1 "key" "VARCHAR" (10 * 100000)
                    , Column 2 1 "value" "VARCHAR" (100 * 100000)
                    ]
  in addTable (Table 1 "ycsb" ycsbColumns) meta

loadYCSBData :: Int -> Int -> Int -> Metadata -> KVStore -> DataRetriever -> IO (KVStore, DataRetriever)
loadYCSBData recordCount keyLength valueLength meta kvstore retriever = do
  updatedKVStore <- loadPhase recordCount keyLength valueLength
  let updatedMeta = foldr (\_ m -> addTraceEntry (TraceEntry 1 [1, 2]) m) meta [1..recordCount]
  return (updatedKVStore, retriever)

runMosaicYCSB :: Int -> Int -> IO ()
runMosaicYCSB recordCount operationCount = do
  let meta = setupYCSBTable $ setupDevices emptyMetadata
  let kvstore = emptyKVStore
  let retriever = emptyDataRetriever

  putStrLn "Running YCSB benchmark with Mosaic optimization"
  
  putStrLn "Loading data..."
  (loadedKVStore, loadedRetriever) <- loadYCSBData recordCount 10 100 meta kvstore retriever

  putStrLn "Initial benchmark..."
  initialResults <- ycsb recordCount operationCount

  putStrLn "\nOptimizing storage..."
  (optimizedKVStore, optimizedRetriever) <- optimizeStorage 1000.0 meta loadedKVStore loadedRetriever

  putStrLn "\nRunning benchmark after optimization..."
  optimizedResults <- ycsb recordCount operationCount

main :: IO ()
main = runMosaicYCSB 100000 1000000