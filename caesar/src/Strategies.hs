module Strategies where

import qualified Data.Map as Map
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Metadata
import DataRetriever

data PlacementStrategy = HOT_TABLE | HOT_COLUMN | LOPT deriving (Show, Eq)

data Placement = Placement
  { placementColumnId :: Int
  , placementDeviceId :: Int
  } deriving (Show, Eq)

hotTableStrategy :: Metadata -> [Placement]
hotTableStrategy meta =
  let tableAccessCounts = foldr (\entry m -> Map.insertWith (+) (traceTableId entry) 1 m) Map.empty (metaTrace meta)
      sortedTables = sortOn (Down . snd) $ Map.toList tableAccessCounts
      sortedDevices = sortOn (Down . deviceCostPerGB) $ Map.elems (metaDevices meta)
  in concat
     [ [ Placement (columnId col) (deviceId dev)
       | col <- tableColumns tbl
       ]
     | ((tblId, _), dev) <- zip sortedTables sortedDevices
     , Just tbl <- [getTable tblId meta]
     ]

hotColumnStrategy :: Metadata -> [Placement]
hotColumnStrategy meta =
  let columnAccessCounts = foldr (\entry m -> foldr (\colId n -> Map.insertWith (+) colId 1 n) m (traceColumnIds entry)) Map.empty (metaTrace meta)
      sortedColumns = sortOn (Down . snd) $ Map.toList columnAccessCounts
      sortedDevices = sortOn (Down . deviceCostPerGB) $ Map.elems (metaDevices meta)
  in [ Placement colId (deviceId dev)
     | ((colId, _), dev) <- zip sortedColumns sortedDevices
     ]

loptStrategy :: Maybe Float -> Metadata -> [Placement]
loptStrategy budget meta =
  let columnSizes = Map.map columnSize (metaColumns meta)
      sortedColumns = sortOn (Down . snd) $ Map.toList columnSizes
      sortedDevices = sortOn deviceCostPerGB $ Map.elems (metaDevices meta)
      
      place _ _ [] _ = []
      place [] _ _ _ = []
      place ((colId, size):restCols) (dev:restDevs) acc totalCost
        | Just b <- budget, newCost > b = place ((colId, size):restCols) restDevs acc totalCost
        | otherwise = Placement colId (deviceId dev) : place restCols (dev:restDevs) (newCost:acc) newCost
        where newCost = totalCost + (fromIntegral size / (1024 * 1024 * 1024)) * deviceCostPerGB dev
      
  in place sortedColumns sortedDevices [] 0

placeData :: PlacementStrategy -> Maybe Float -> Metadata -> [Placement]
placeData HOT_TABLE _ meta = hotTableStrategy meta
placeData HOT_COLUMN _ meta = hotColumnStrategy meta
placeData LOPT budget meta = loptStrategy budget meta

predictScanTime :: [Placement] -> TraceEntry -> Metadata -> Float
predictScanTime placements tableScan meta =
  let deviceTimes = foldr (\colId acc ->
        let placement = find (\p -> placementColumnId p == colId) placements
            device = placement >>= \p -> getDevice (placementDeviceId p) meta
            devModel = placement >>= \p -> getDeviceModel (placementDeviceId p) meta
            column = getColumn colId meta
        in case (device, devModel, column) of
             (Just dev, Just model, Just col) ->
               let compressionThroughput = case deviceCompression dev of
                     None -> modelNoneThroughput model
                     LZ4 -> modelLZ4Throughput model
                     ZSTD -> modelZSTDThroughput model
                   columnTime = modelSeekTime model + (fromIntegral (columnSize col) / compressionThroughput)
               in Map.insertWith (+) (deviceId dev) columnTime acc
             _ -> acc
      ) Map.empty (traceColumnIds tableScan)
  in maximum (Map.elems deviceTimes)

predictTotalTime :: [Placement] -> Metadata -> Float
predictTotalTime placements meta =
  sum [predictScanTime placements trace meta | trace <- metaTrace meta]

calculateTotalCost :: [Placement] -> Metadata -> Float
calculateTotalCost placements meta =
  sum [deviceCostPerGB dev * (fromIntegral (columnSize col) / (1024 * 1024 * 1024))
      | Placement colId devId <- placements
      , Just dev <- [getDevice devId meta]
      , Just col <- [getColumn colId meta]
      ]

optimizePlacement :: Float -> Metadata -> IO [Placement]
optimizePlacement budget meta = do
  let placements = placeData LOPT (Just budget) meta
      totalTime = predictTotalTime placements meta
      totalCost = calculateTotalCost placements meta
  putStrLn $ "Predicted total time: " ++ show totalTime ++ " s"
  putStrLn $ "Total cost: " ++ show totalCost
  return placements

applyPlacements :: [Placement] -> Metadata -> KVStore -> DataRetriever -> IO (KVStore, DataRetriever)
applyPlacements placements meta kvstore retriever =
  foldM (\(kv, ret) placement -> do
    let colId = placementColumnId placement
        devId = placementDeviceId placement
        chunks = Map.findWithDefault [] colId (retrieverColumnChunks ret)
    (newKV, newRet) <- foldM (\(kv', ret') chunk ->
      if chunkDeviceId chunk /= devId
      then do
        let (updatedKV, updatedRet) = moveColumnChunk chunk devId meta kv' ret'
        putStrLn $ "Moved column " ++ show colId ++ " to device " ++ show devId
        return (updatedKV, updatedRet)
      else return (kv', ret')
      ) (kv, ret) chunks
    return (newKV, newRet)
  ) (kvstore, retriever) placements