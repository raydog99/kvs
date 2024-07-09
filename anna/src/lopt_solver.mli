open Metadata
open Prediction_model

type lp_variable = {
  col_id: column_id;
  dev_id: device_id;
}

val solve_lopt : float option -> placement list
