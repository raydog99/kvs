module YCSBBenchmark where

import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad (replicateM, forM_)
import Data.List (sort)
import qualified Data.ByteString.Char8 as BS
import KVStore

data YCSBWorkload = YCSBWorkload
  { workloadName :: String
  , readProportion :: Float
  , updateProportion :: Float
  , insertProportion :: Float
  }

ycsb :: Int -> Int -> IO ()
ycsb recordCount operationCount = do
  let keyLength = 10
      valueLength = 100

  putStrLn "Loading data..."
  kvstore <- loadPhase recordCount keyLength valueLength

  let workloads =
        [ YCSBWorkload "Workload A (50% read, 50% update)" 0.5 0.5 0.0
        , YCSBWorkload "Workload B (95% read, 5% update)" 0.95 0.05 0.0
        , YCSBWorkload "Workload C (100% read)" 1.0 0.0 0.0
        , YCSBWorkload "Workload D (95% read, 5% insert)" 0.95 0.0 0.05
        , YCSBWorkload "Workload E (95% scan, 5% insert)" 0.95 0.0 0.05  -- Treating scan as read
        , YCSBWorkload "Workload F (50% read, 50% read-modify-write)" 0.5 0.5 0.0
        ]

  forM_ workloads $ \workload -> do
    putStrLn $ "\nRunning " ++ workloadName workload
    (totalTime, latencies) <- runWorkload operationCount keyLength valueLength workload kvstore
    let avgLatency = sum latencies / fromIntegral (length latencies)
        throughput = fromIntegral operationCount / totalTime
        sortedLatencies = sort latencies
        p95 = sortedLatencies !! floor (0.95 * fromIntegral (length latencies))
        p99 = sortedLatencies !! floor (0.99 * fromIntegral (length latencies))

    putStrLn $ "Throughput: " ++ show throughput ++ " ops/sec"
    putStrLn $ "Average latency: " ++ show (avgLatency * 1000) ++ " ms"
    putStrLn $ "95th percentile latency: " ++ show (p95 * 1000) ++ " ms"
    putStrLn $ "99th percentile latency: " ++ show (p99 * 1000) ++ " ms"

loadPhase :: Int -> Int -> Int -> IO KVStore
loadPhase recordCount keyLength valueLength = do
  let initialKVStore = emptyKVStore
  foldM (\kvs _ -> do
    key <- generateKey keyLength
    value <- generateValue valueLength
    return $ put key value kvs
  ) initialKVStore [1..recordCount]

runWorkload :: Int -> Int -> Int -> YCSBWorkload -> KVStore -> IO (Float, [Float])
runWorkload operationCount keyLength valueLength workload initialKVStore = do
  startTime <- getCurrentTime
  (finalKVStore, latencies) <- foldM (\(kvs, lats) _ -> do
    r <- randomRIO (0.0, 1.0) :: IO Float
    (updatedKVS, latency) <-
      if r < readProportion workload then do
        key <- generateKey keyLength
        t1 <- getCurrentTime
        let _ = get key kvs
        t2 <- getCurrentTime
        return (kvs, realToFrac $ diffUTCTime t2 t1)
      else if r < readProportion workload + updateProportion workload then do
        key <- generateKey keyLength
        value <- generateValue valueLength
        t1 <- getCurrentTime
        let updatedKVS = put key value kvs
        t2 <- getCurrentTime
        return (updatedKVS, realToFrac $ diffUTCTime t2 t1)
      else do
        key <- generateKey keyLength
        value <- generateValue valueLength
        t1 <- getCurrentTime
        let updatedKVS = put key value kvs
        t2 <- getCurrentTime
        return (updatedKVS, realToFrac $ diffUTCTime t2 t1)
    return (updatedKVS, latency : lats)
  ) (initialKVStore, []) [1..operationCount]
  endTime <- getCurrentTime
  let totalTime = realToFrac $ diffUTCTime endTime startTime
  return (totalTime, latencies)

generateKey :: Int -> IO String
generateKey length = replicateM length $ randomRIO ('a', 'z')

generateValue :: Int -> IO String
generateValue length = replicateM length $ randomRIO ('a', 'z')

main :: IO ()
main = ycsb 100000 1000000