module DataRetriever where

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Metadata
import KVStore

data ColumnChunk = ColumnChunk
  { chunkColumnId :: Int
  , chunkDeviceId :: Int
  , chunkSize :: Int
  , chunkCompressedSize :: Int
  , chunkCompression :: CompressionAlgorithm
  } deriving (Show, Eq)

data DataRetriever = DataRetriever
  { retrieverColumnChunks :: Map.Map Int [ColumnChunk]
  , retrieverBuffer :: [(Int, BS.ByteString)]
  , retrieverBufferSize :: Int
  }

emptyDataRetriever :: DataRetriever
emptyDataRetriever = DataRetriever Map.empty [] 0

compressColumnChunk :: ColumnChunk -> BS.ByteString -> BS.ByteString
compressColumnChunk chunk data' =
  case chunkCompression chunk of
    None -> data'
    LZ4 -> error "LZ4 compression not implemented"
    ZSTD -> error "ZSTD compression not implemented"

decompressColumnChunk :: ColumnChunk -> BS.ByteString -> BS.ByteString
decompressColumnChunk chunk data' =
  case chunkCompression chunk of
    None -> data'
    LZ4 -> error "LZ4 decompression not implemented"
    ZSTD -> error "ZSTD decompression not implemented"

retrieveColumnChunk :: ColumnChunk -> Metadata -> KVStore -> DataRetriever -> DataRetriever
retrieveColumnChunk chunk meta kvstore retriever =
  let compressedData = KVStore.get (generateColumnKey $ getColumn (chunkColumnId chunk) meta) kvstore
      decompressedData = maybe BS.empty (decompressColumnChunk chunk) compressedData
      newBuffer = (chunkColumnId chunk, decompressedData) : retrieverBuffer retriever
      newBufferSize = retrieverBufferSize retriever + BS.length decompressedData
  in retriever { retrieverBuffer = newBuffer, retrieverBufferSize = newBufferSize }

fetchTableScan :: Int -> [Int] -> Metadata -> KVStore -> DataRetriever -> DataRetriever
fetchTableScan tableId columnIds meta kvstore retriever =
  foldr (\colId acc ->
    case Map.lookup colId (retrieverColumnChunks retriever) of
      Just chunks -> foldr (\chunk r -> retrieveColumnChunk chunk meta kvstore r) acc chunks
      Nothing -> acc
  ) retriever columnIds

getBufferData :: DataRetriever -> ([(Int, BS.ByteString)], DataRetriever)
getBufferData retriever = (retrieverBuffer retriever, retriever { retrieverBuffer = [], retrieverBufferSize = 0 })

moveColumnChunk :: ColumnChunk -> Int -> Metadata -> KVStore -> DataRetriever -> (KVStore, DataRetriever)
moveColumnChunk chunk targetDeviceId meta kvstore retriever =
  let compressedData = KVStore.get (generateColumnKey $ getColumn (chunkColumnId chunk) meta) kvstore
      decompressedData = maybe BS.empty (decompressColumnChunk chunk) compressedData
      targetDevice = getDevice targetDeviceId meta
      newCompressedData = compressColumnChunk (chunk { chunkCompression = deviceCompression <$> targetDevice }) decompressedData
      newChunk = chunk { chunkDeviceId = targetDeviceId
                       , chunkCompressedSize = BS.length newCompressedData
                       , chunkCompression = maybe (chunkCompression chunk) deviceCompression targetDevice
                       }
      newKVStore = KVStore.put (generateColumnKey $ getColumn (chunkColumnId chunk) meta) (C8.unpack newCompressedData) kvstore
      newColumnChunks = Map.adjust (map (\c -> if c == chunk then newChunk else c)) (chunkColumnId chunk) (retrieverColumnChunks retriever)
  in (newKVStore, retriever { retrieverColumnChunks = newColumnChunks })