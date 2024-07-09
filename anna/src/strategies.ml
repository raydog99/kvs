open Metadata
open Prediction_model
open Lopt_solver

type placement_strategy = HOT_TABLE | HOT_COLUMN | LOPT

let hot_table_strategy () : placement list =
  let table_access_counts = Hashtbl.create 10 in
  List.iter (fun entry ->
    let count = Hashtbl.find_opt table_access_counts entry.table_id
                |> Option.value ~default:0 in
    Hashtbl.replace table_access_counts entry.table_id (count + 1)
  ) !trace;

  let sorted_tables = Hashtbl.to_seq table_access_counts
                      |> List.of_seq
                      |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1) in

  let sorted_devices = Hashtbl.to_seq devices
                       |> List.of_seq
                       |> List.sort (fun (_, d1) (_, d2) -> compare d2.cost_per_gb d1.cost_per_gb) in

  let rec assign_tables tables devices acc =
    match tables, devices with
    | [], _ | _, [] -> acc
    | (table_id, _) :: rest_tables, (device_id, _) :: rest_devices ->
      let table = Hashtbl.find tables table_id in
      let new_placements = List.map (fun col -> 
        {column_id = col.id; device_id = device_id}
      ) table.columns in
      assign_tables rest_tables rest_devices (acc @ new_placements)
  in

  assign_tables sorted_tables sorted_devices []

let hot_column_strategy () : placement list =
  let column_access_counts = Hashtbl.create 1000 in
  List.iter (fun entry ->
    List.iter (fun col_id ->
      let count = Hashtbl.find_opt column_access_counts col_id
                  |> Option.value ~default:0 in
      Hashtbl.replace column_access_counts col_id (count + 1)
    ) entry.column_ids
  ) !trace;

  let sorted_columns = Hashtbl.to_seq column_access_counts
                       |> List.of_seq
                       |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1) in

  let sorted_devices = Hashtbl.to_seq devices
                       |> List.of_seq
                       |> List.sort (fun (_, d1) (_, d2) -> compare d2.cost_per_gb d1.cost_per_gb) in

  let rec assign_columns columns devices acc =
    match columns, devices with
    | [], _ | _, [] -> acc
    | (column_id, _) :: rest_columns, (device_id, _) :: rest_devices ->
      let new_placement = {column_id = column_id; device_id = device_id} in
      assign_columns rest_columns rest_devices (new_placement :: acc)
  in

  assign_columns sorted_columns sorted_devices []

let lopt_strategy (budget: float option) : placement list =
  solve_lopt budget

let place_data (strategy: placement_strategy) (budget: float option) : placement list =
  match strategy with
  | HOT_TABLE -> hot_table_strategy ()
  | HOT_COLUMN -> hot_column_strategy ()
  | LOPT -> lopt_strategy budget

let optimize_placement (budget: float) : placement list =
  let placements = place_data LOPT (Some budget) in
  let total_time = predict_total_time placements !trace in
  let total_cost = calculate_total_cost placements in
  Printf.printf "Predicted total time: %.2f s\n" total_time;
  Printf.printf "Total cost: %.2f\n" total_cost;
  placements

let apply_placements (placements: placement list) : unit =
  List.iter (fun placement ->
    let column = Hashtbl.find columns placement.column_id in
    let target_device = Hashtbl.find devices placement.device_id in
    let chunks = Hashtbl.find Data_retriever.column_chunks placement.column_id in
    
    List.iter (fun chunk ->
      if chunk.device_id <> placement.device_id then
        Data_retriever.move_column_chunk chunk placement.device_id
    ) chunks;
    
    Printf.printf "Moved column %s to device %s\n" column.name target_device.name;
  ) placements