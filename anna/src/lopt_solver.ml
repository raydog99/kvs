open Metadata
open Prediction_model

open Glpk

type lp_variable = {
  col_id: column_id;
  dev_id: device_id;
}

let solve_lopt (budget: float option) : placement list =
  let lp = Glp.create_prob () in
  Glp.set_obj_dir lp Glp.Min;

  let variables = ref [] in
  Hashtbl.iter (fun col_id col ->
    Hashtbl.iter (fun dev_id dev ->
      let var = { col_id; dev_id } in
      variables := var :: !variables;
      ignore (Glp.add_cols lp 1);
      let col_index = Glp.get_num_cols lp in
      Glp.set_col_bnds lp col_index Glp.Db 0.0 1.0;
      Glp.set_col_kind lp col_index Glp.Bv;
    ) devices
  ) columns;

  (* Set objective function *)
  List.iteri (fun i var ->
    let col = Hashtbl.find columns var.col_id in
    let dev = Hashtbl.find devices var.dev_id in
    let dev_model = Hashtbl.find device_models var.dev_id in
    let cost = dev_model.seek_time +. (float_of_int col.size /. dev_model.none_throughput) in
    Glp.set_obj_coef lp (i + 1) cost;
  ) !variables;

  (* Add constraints *)
  (* 1. Each column must be placed on exactly one device *)
  Hashtbl.iter (fun col_id _ ->
    ignore (Glp.add_rows lp 1);
    let row_index = Glp.get_num_rows lp in
    Glp.set_row_bnds lp row_index Glp.Fx 1.0 1.0;
    List.iteri (fun i var ->
      if var.col_id = col_id then
        Glp.set_mat_row lp row_index 1 [|i + 1|] [|1.0|];
    ) !variables;
  ) columns;

  (* 2. Device capacity constraints *)
  Hashtbl.iter (fun dev_id dev ->
    ignore (Glp.add_rows lp 1);
    let row_index = Glp.get_num_rows lp in
    Glp.set_row_bnds lp row_index Glp.Up 0.0 (float_of_int dev.capacity);
    List.iteri (fun i var ->
      if var.dev_id = dev_id then
        let col = Hashtbl.find columns var.col_id in
        let col_size_gb = float_of_int col.size /. (1024. *. 1024. *. 1024.) in
        Glp.set_mat_row lp row_index 1 [|i + 1|] [|col_size_gb|];
    ) !variables;
  ) devices;

  (* 3. Budget constraint (if applicable) *)
  (match budget with
  | Some b ->
    ignore (Glp.add_rows lp 1);
    let row_index = Glp.get_num_rows lp in
    Glp.set_row_bnds lp row_index Glp.Up 0.0 b;
    List.iteri (fun i var ->
      let col = Hashtbl.find columns var.col_id in
      let dev = Hashtbl.find devices var.dev_id in
      let col_size_gb = float_of_int col.size /. (1024. *. 1024. *. 1024.) in
      let cost = col_size_gb *. dev.cost_per_gb in
      Glp.set_mat_row lp row_index 1 [|i + 1|] [|cost|];
    ) !variables;
  | None -> ());

  (* Solve the problem *)
  let parm = Glp.init_iocp () in
  Glp.iocp parm (fun p -> p.tm_lim <- 10000);
  ignore (Glp.intopt lp parm);

  (* Extract the solution *)
  List.filter_map (fun var ->
    let col_index = List.index_of var !variables + 1 in
    if Glp.mip_col_val lp col_index > 0.5 then
      Some { column_id = var.col_id; device_id = var.dev_id }
    else
      None
  ) !variables