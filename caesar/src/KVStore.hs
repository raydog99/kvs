module KVStore where

import qualified Data.Map as Map
import Metadata (Column(..))

newtype KVStore = KVStore { unKVStore :: Map.Map String String }

emptyKVStore :: KVStore
emptyKVStore = KVStore Map.empty

put :: String -> String -> KVStore -> KVStore
put key value (KVStore store) = KVStore $ Map.insert key value store

get :: String -> KVStore -> Maybe String
get key (KVStore store) = Map.lookup key store

delete :: String -> KVStore -> KVStore
delete key (KVStore store) = KVStore $ Map.delete key store

generateColumnKey :: Column -> String
generateColumnKey col = show (columnTableId col) ++ ":" ++ show (columnId col) ++ ":" ++ columnName col