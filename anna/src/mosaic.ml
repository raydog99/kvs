open Metadata
open Strategies
open Data_retriever
open Kv_store

type query = {
  table_id: table_id;
  column_ids: column_id list;
}

let parse_query (query_str: string) : query =
  let parts = String.split_on_char ':' query_str in
  let table_id = int_of_string (List.hd parts) in
  let column_ids = List.map int_of_string (String.split_on_char ',' (List.nth parts 1)) in
  { table_id; column_ids }

let run_query (query: string) : unit =
  let table_id = 0 and column_ids = [] in
  fetch_table_scan table_id column_ids;
  let result = get_buffer_data () in
  ()

let optimize_storage (budget: float) : unit =
  let placements = optimize_placement budget in
  apply_placements placements

let benchmark (queries: string list) : float =
  let start_time = Unix.gettimeofday () in
  List.iter run_query queries;
  let end_time = Unix.gettimeofday () in
  end_time -. start_time

let run_mosaic (budget: float) (queries: string list) : unit =
  Printf.printf "Initial benchmark...\n";
  let initial_time = benchmark queries in
  Printf.printf "Initial execution time: %.2f s\n" initial_time;

  Printf.printf "\nOptimizing storage...\n";
  optimize_storage budget;

  Printf.printf "\nRunning benchmark after optimization...\n";
  let optimized_time = benchmark queries in
  Printf.printf "Optimized execution time: %.2f s\n" optimized_time;

  let improvement = (initial_time -. optimized_time) /. initial_time *. 100.0 in
  Printf.printf "\nImprovement: %.2f%%\n" improvement