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

module Parquet = struct
  type reader
  type writer
  
  external create_reader : string -> reader = "parquet_create_reader"
  external create_writer : string -> int -> writer = "parquet_create_writer"
  external read_column : reader -> int -> string = "parquet_read_column"
  external write_column : writer -> int -> string -> unit = "parquet_write_column"
  external close_reader : reader -> unit = "parquet_close_reader"
  external close_writer : writer -> unit = "parquet_close_writer"
end

let column_chunks : (column_id, column_chunk list) Hashtbl.t = Hashtbl.create 1000

let compress_column_chunk (chunk: column_chunk) (data: string) : string =
  match chunk.compression with
  | None -> data
  | LZ4 -> lz4_compress data
  | ZSTD -> zstd_compress data 3 (* compression level 3 *)

let decompress_column_chunk (chunk: column_chunk) (data: string) : string =
  match chunk.compression with
  | None -> data
  | LZ4 -> lz4_decompress data chunk.size
  | ZSTD -> zstd_decompress data chunk.size

type buffer = {
  mutable data: (column_id * string) list;
  mutable size: int;
}

let buffer = { data = []; size = 0 }
let reader_threads = Thread.create_table 100
let decompressor_threads = Thread.create_table 100

let retrieve_column_chunk (chunk: column_chunk) : unit =
  let device = Hashtbl.find devices chunk.device_id in
  let reader () =
    let compressed_data = Kv_store.retrieve_column_chunk chunk in
    Thread.add decompressor_threads (fun () ->
      let decompressed = decompress_column_chunk chunk compressed_data in
      buffer.data <- (chunk.column_id, decompressed) :: buffer.data;
      buffer.size <- buffer.size + String.length decompressed
    )
  in
  Thread.add reader_threads reader

let fetch_table_scan (table_id: table_id) (column_ids: column_id list) : unit =
  List.iter (fun col_id ->
    let chunks = Hashtbl.find column_chunks col_id in
    List.iter retrieve_column_chunk chunks
  ) column_ids

let get_buffer_data () : (column_id * string) list =
  let data = buffer.data in
  buffer.data <- [];
  buffer.size <- 0;
  data

let move_column_chunk (chunk: column_chunk) (target_device_id: device_id) : unit =
  let source_device = Hashtbl.find devices chunk.device_id in
  let target_device = Hashtbl.find devices target_device_id in
  
  (* Retrieve the data from the source device *)
  let compressed_data = Kv_store.retrieve_column_chunk chunk in
  let data = decompress_column_chunk chunk compressed_data in
  
  (* Compress the data according to the target device's compression setting *)
  let new_compressed_data = compress_column_chunk 
    { chunk with compression = target_device.compression } data in
  
  (* Store the data on the target device *)
  let new_chunk = { 
    chunk with 
    device_id = target_device_id; 
    compressed_size = String.length new_compressed_data;
    compression = target_device.compression 
  } in
  Kv_store.store_column_chunk new_chunk new_compressed_data;
  
  (* Update the column_chunks hashtable *)
  let chunks = Hashtbl.find column_chunks chunk.column_id in
  let updated_chunks = List.map (fun c -> if c = chunk then new_chunk else c) chunks in
  Hashtbl.replace column_chunks chunk.column_id updated_chunks;
  
  (* Remove the data from the source device *)
  let column = Hashtbl.find columns chunk.column_id in
  let key = Kv_store.generate_column_key column in
  Kv_store.delete key

let load_parquet_file (filename: string) (table_id: table_id) (device_id: device_id) : unit =
  let reader = Parquet.create_reader filename in
  let device = Hashtbl.find devices device_id in
  let table = Hashtbl.find tables table_id in
  
  List.iteri (fun i column ->
    let data = Parquet.read_column reader i in
    let compressed_data = compress_column_chunk 
      { column_id = column.id; device_id; size = String.length data; 
        compressed_size = 0; compression = device.compression } data in
    let chunk = { 
      column_id = column.id; 
      device_id; 
      size = String.length data; 
      compressed_size = String.length compressed_data; 
      compression = device.compression 
    } in
    Kv_store.store_column_chunk chunk compressed_data;
    Hashtbl.add column_chunks column.id [chunk]
  ) table.columns;
  
  Parquet.close_reader reader

let save_to_parquet (filename: string) (table_id: table_id) : unit =
  let table = Hashtbl.find tables table_id in
  let writer = Parquet.create_writer filename (List.length table.columns) in
  
  List.iter (fun column ->
    let chunks = Hashtbl.find column_chunks column.id in
    let data = List.fold_left (fun acc chunk ->
      let compressed_data = Kv_store.retrieve_column_chunk chunk in
      let decompressed = decompress_column_chunk chunk compressed_data in
      acc ^ decompressed
    ) "" chunks in
    Parquet.write_column writer column.id data
  ) table.columns;
  
  Parquet.close_writer writer