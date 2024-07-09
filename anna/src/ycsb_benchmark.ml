open Kv_store

module YCSB = struct
  let record_count = 100000
  let operation_count = 1000000
  let key_length = 10
  let value_length = 100

  let generate_key () =
    String.init key_length (fun _ -> Char.chr (97 + Random.int 26))

  let generate_value () =
    String.init value_length (fun _ -> Char.chr (97 + Random.int 26))

  let load_phase () =
    for _ = 1 to record_count do
      let key = generate_key () in
      let value = generate_value () in
      Kv_store.put key value
    done

  let run_workload read_proportion update_proportion insert_proportion =
    let operations =
      List.init operation_count (fun _ ->
        let r = Random.float 1.0 in
        if r < read_proportion then
          ("read", generate_key (), "")
        else if r < read_proportion +. update_proportion then
          ("update", generate_key (), generate_value ())
        else
          ("insert", generate_key (), generate_value ())
      )
    in

    let start_time = Unix.gettimeofday () in
    let latencies = List.map (fun (op, key, value) ->
      let op_start_time = Unix.gettimeofday () in
      (match op with
      | "read" -> ignore (Kv_store.get key)
      | "update" | "insert" -> Kv_store.put key value
      | _ -> ());
      Unix.gettimeofday () -. op_start_time
    ) operations in

    let total_time = Unix.gettimeofday () -. start_time in
    (total_time, latencies)

  let run_benchmark () =
    Printf.printf "Loading data...\n";
    load_phase ();

    let workloads = [
      ("Workload A (50%% read, 50%% update)", 0.5, 0.5, 0.0);
      ("Workload B (95%% read, 5%% update)", 0.95, 0.05, 0.0);
      ("Workload C (100%% read)", 1.0, 0.0, 0.0);
      ("Workload D (95%% read, 5%% insert)", 0.95, 0.0, 0.05);
      ("Workload E (95%% scan, 5%% insert)", 0.95, 0.0, 0.05); (* Treating scan as read *)
      ("Workload F (50%% read, 50%% read-modify-write)", 0.5, 0.5, 0.0);
    ] in

    List.iter (fun (name, read_prop, update_prop, insert_prop) ->
      Printf.printf "\nRunning %s\n" name;
      let total_time, latencies = run_workload read_prop update_prop insert_prop in
      let avg_latency = List.fold_left (+.) 0.0 latencies /. float_of_int (List.length latencies) in
      let throughput = float_of_int operation_count /. total_time in

      Printf.printf "Throughput: %.2f ops/sec\n" throughput;
      Printf.printf "Average latency: %.2f ms\n" (avg_latency *. 1000.0);
      
      let sorted_latencies = List.sort compare latencies in
      let p95 = List.nth sorted_latencies (int_of_float (0.95 *. float_of_int (List.length latencies))) in
      let p99 = List.nth sorted_latencies (int_of_float (0.99 *. float_of_int (List.length latencies))) in
      
      Printf.printf "95th percentile latency: %.2f ms\n" (p95 *. 1000.0);
      Printf.printf "99th percentile latency: %.2f ms\n" (p99 *. 1000.0);
    ) workloads
end

let () = YCSB.run_benchmark ()