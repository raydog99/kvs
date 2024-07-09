open Metadata

type key = string
type value = string

module KVStore = Map.Make(String)

let store = ref KVStore.empty

let put (k: key) (v: value) : unit =
  store := KVStore.add k v !store

let get (k: key) : value option =
  KVStore.find_opt k !store

let delete (k: key) : unit =
  store := KVStore.remove k !store

let generate_column_key (column: column) : key =
  Printf.sprintf "%d:%d:%s" column.table_id column.id column.name

let store_column_chunk (chunk: Data_retriever.column_chunk) (data: string) : unit =
  let column = Hashtbl.find columns chunk.column_id in
  let key = generate_column_key column in
  put key data

let retrieve_column_chunk (chunk: Data_retriever.column_chunk) : string =
  let column = Hashtbl.find columns chunk.column_id in
  let key = generate_column_key column in
  match get key with
  | Some data -> data
  | None -> failwith (Printf.sprintf "Column chunk not found: %s" key)