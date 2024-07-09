open Metadata

type key = string
type value = string

module KVStore : module type of Map.Make(String)

val store : (key, value) KVStore.t ref

val put : key -> value -> unit

val get : key -> value option

val delete : key -> unit

val generate_column_key : column -> key

val store_column_chunk : Data_retriever.column_chunk -> string -> unit

val retrieve_column_chunk : Data_retriever.column_chunk -> string
