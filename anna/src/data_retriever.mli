open Metadata
open Kv_store

type query = {
  table_id: table_id;
  column_ids: column_id list;
}

type column_chunk = {
  column_id: column_id;
  device_id: device_id;
  size: int; (* in bytes *)
  compressed_size: int; (* in bytes *)
  compression: compression_algorithm;
}

external lz4_compress : string -> string = "lz4_compress"
external lz4_decompress : string -> int -> string = "lz4_decompress"
external zstd_compress : string -> int -> string = "zstd_compress"
external zstd_decompress : string -> int -> string = "zstd_decompress"

module Parquet : sig
  type reader
  type writer
  
  external create_reader : string -> reader = "parquet_create_reader"
  external create_writer : string -> int -> writer = "parquet_create_writer"
  external read_column : reader -> int -> string = "parquet_read_column"
  external write_column : writer -> int -> string -> unit = "parquet_write_column"
  external close_reader : reader -> unit = "parquet_close_reader"
  external close_writer : writer -> unit = "parquet_close_writer"
end

val column_chunks : (column_id, column_chunk list) Hashtbl.t

val compress_column_chunk : column_chunk -> string -> string

val decompress_column_chunk : column_chunk -> string -> string

type buffer = {
  mutable data: (column_id * string) list;
  mutable size: int;
}

val buffer : buffer
val reader_threads : (unit -> unit) Thread.create_table
val decompressor_threads : (unit -> unit) Thread.create_table

val retrieve_column_chunk : column_chunk -> unit

val fetch_table_scan : table_id -> column_id list -> unit

val get_buffer_data : unit -> (column_id * string) list

val move_column_chunk : column_chunk -> device_id -> unit

val load_parquet_file : string -> table_id -> device_id -> unit

val save_to_parquet : string -> table_id -> unit