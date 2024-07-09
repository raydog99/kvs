open Metadata

type placement = {
  column_id: column_id;
  device_id: device_id;
}

let predict_scan_time (placements: placement list) (table_scan: trace_entry) : float =
  let max_device_time = ref 0.0 in
  let devices_used = Hashtbl.create 10 in

  List.iter (fun col_id ->
    let placement = List.find (fun p -> p.column_id = col_id) placements in
    let device = Hashtbl.find devices placement.device_id in
    let device_model = Hashtbl.find device_models placement.device_id in
    let column = Hashtbl.find columns col_id in
    
    let compression_throughput = match device.compression with
      | None -> device_model.none_throughput
      | LZ4 -> device_model.lz4_throughput
      | ZSTD -> device_model.zstd_throughput
    in

    let column_time = device_model.seek_time +. (float_of_int column.size /. compression_throughput) in
    
    if not (Hashtbl.mem devices_used placement.device_id) then
      Hashtbl.add devices_used placement.device_id column_time
    else
      let current_time = Hashtbl.find devices_used placement.device_id in
      Hashtbl.replace devices_used placement.device_id (current_time +. column_time)
  ) table_scan.column_ids;

  Hashtbl.iter (fun _ time ->
    if time > !max_device_time then max_device_time := time
  ) devices_used;

  !max_device_time

let predict_total_time (placements: placement list) (trace: trace_entry list) : float =
  List.fold_left (fun total_time table_scan ->
    total_time +. predict_scan_time placements table_scan
  ) 0.0 trace

let calculate_total_cost (placements: placement list) : float =
  List.fold_left (fun total_cost placement ->
    let device = Hashtbl.find devices placement.device_id in
    let column = Hashtbl.find columns placement.column_id in
    let column_size_gb = float_of_int column.size /. (1024. *. 1024. *. 1024.) in
    total_cost +. (column_size_gb *. device.cost_per_gb)
  ) 0.0 placements