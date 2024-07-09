module Metadata where

import qualified Data.Map as Map
import Data.List (sort)

data CompressionAlgorithm = None | LZ4 | ZSTD deriving (Show, Eq)

data Device = Device
  { deviceId :: Int
  , deviceMnt :: String
  , deviceName :: String
  , deviceCapacity :: Int
  , deviceThreads :: Int
  , deviceCompression :: CompressionAlgorithm
  , deviceCostPerGB :: Float
  } deriving (Show)

data Column = Column
  { columnId :: Int
  , columnTableId :: Int
  , columnName :: String
  , columnDataType :: String
  , columnSize :: Int
  } deriving (Show)

data Table = Table
  { tableId :: Int
  , tableName :: String
  , tableColumns :: [Column]
  } deriving (Show)

data TraceEntry = TraceEntry
  { traceTableId :: Int
  , traceColumnIds :: [Int]
  } deriving (Show)

data DeviceModel = DeviceModel
  { modelDeviceId :: Int
  , modelNoneThroughput :: Float
  , modelLZ4Throughput :: Float
  , modelZSTDThroughput :: Float
  , modelSeekTime :: Float
  } deriving (Show)

data Metadata = Metadata
  { metaDevices :: Map.Map Int Device
  , metaTables :: Map.Map Int Table
  , metaColumns :: Map.Map Int Column
  , metaTrace :: [TraceEntry]
  , metaDeviceModels :: Map.Map Int DeviceModel
  }

emptyMetadata :: Metadata
emptyMetadata = Metadata Map.empty Map.empty Map.empty [] Map.empty

addDevice :: Device -> Metadata -> Metadata
addDevice dev meta = meta { metaDevices = Map.insert (deviceId dev) dev (metaDevices meta) }

getDevice :: Int -> Metadata -> Maybe Device
getDevice devId meta = Map.lookup devId (metaDevices meta)

addTable :: Table -> Metadata -> Metadata
addTable tbl meta =
  let updatedTables = Map.insert (tableId tbl) tbl (metaTables meta)
      updatedColumns = foldr (\col m -> Map.insert (columnId col) col m) (metaColumns meta) (tableColumns tbl)
  in meta { metaTables = updatedTables, metaColumns = updatedColumns }

getTable :: Int -> Metadata -> Maybe Table
getTable tblId meta = Map.lookup tblId (metaTables meta)

getColumn :: Int -> Metadata -> Maybe Column
getColumn colId meta = Map.lookup colId (metaColumns meta)

addTraceEntry :: TraceEntry -> Metadata -> Metadata
addTraceEntry entry meta = meta { metaTrace = entry : metaTrace meta }

clearTrace :: Metadata -> Metadata
clearTrace meta = meta { metaTrace = [] }

loadDeviceModel :: DeviceModel -> Metadata -> Metadata
loadDeviceModel model meta = meta { metaDeviceModels = Map.insert (modelDeviceId model) model (metaDeviceModels meta) }

getDeviceModel :: Int -> Metadata -> Maybe DeviceModel
getDeviceModel devId meta = Map.lookup devId (metaDeviceModels meta)