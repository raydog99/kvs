type device_id = int
type column_id = int
type table_id = int

type compression_algorithm = None | LZ4 | ZSTD

type device = {
  id: device_id;
  mnt: string;
  name: string;
  capacity: int; (* in GB *)
  threads: int;
  compression: compression_algorithm;
  cost_per_gb: float;
}

type column = {
  id: column_id;
  table_id: table_id;
  name: string;
  data_type: string;
  size: int; (* in bytes *)
}

type table = {
  id: table_id;
  name: string;
  columns: column list;
}

type trace_entry = {
  table_id: table_id;
  column_ids: column_id list;
}

type device_model = {
  device_id: device_id;
  none_throughput: float; (* in GB/s *)
  lz4_throughput: float;
  zstd_throughput: float;
  seek_time: float; (* in seconds *)
}

val devices : (device_id, device) Hashtbl.t
val tables : (table_id, table) Hashtbl.t
val columns : (column_id, column) Hashtbl.t
val trace : trace_entry list ref
val device_models : (device_id, device_model) Hashtbl.t

val add_device : device -> unit

val get_device : device_id -> device option

val add_table : table -> unit

val get_table : table_id -> table option

val get_column : column_id -> column option

val add_trace_entry : trace_entry -> unit

val clear_trace : unit -> unit

val load_device_model : device_model -> unit

val get_device_model : device_id -> device_model option

val load_device_config : string -> unit

val load_table_definitions : string -> unit