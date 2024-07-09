open Metadata

type placement = {
  column_id: column_id;
  device_id: device_id;
}

val predict_scan_time : placement list -> trace_entry -> float

val predict_total_time : placement list -> trace_entry list -> float

val calculate_total_cost : placement list -> float
