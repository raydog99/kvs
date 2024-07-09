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

let devices : (device_id, device) Hashtbl.t = Hashtbl.create 10
let tables : (table_id, table) Hashtbl.t = Hashtbl.create 100
let columns : (column_id, column) Hashtbl.t = Hashtbl.create 1000
let trace : trace_entry list ref = ref []
let device_models : (device_id, device_model) Hashtbl.t = Hashtbl.create 10

let add_device (dev: device) : unit =
  Hashtbl.add devices dev.id dev

let get_device (id: device_id) : device option =
  Hashtbl.find_opt devices id

let add_table (tbl: table) : unit =
  Hashtbl.add tables tbl.id tbl;
  List.iter (fun col -> Hashtbl.add columns col.id col) tbl.columns

let get_table (id: table_id) : table option =
  Hashtbl.find_opt tables id

let get_column (id: column_id) : column option =
  Hashtbl.find_opt columns id

let add_trace_entry (entry: trace_entry) : unit =
  trace := entry :: !trace

let clear_trace () : unit =
  trace := []

let load_device_model (model: device_model) : unit =
  Hashtbl.add device_models model.device_id model

let get_device_model (id: device_id) : device_model option =
  Hashtbl.find_opt device_models id

let load_device_config (filename: string) : unit =
  ()

let load_table_definitions (filename: string) : unit =
  ()