open Metadata

type placement_strategy = HOT_TABLE | HOT_COLUMN | LOPT

val hot_table_strategy : unit -> placement list

val hot_column_strategy : unit -> placement list

val lopt_strategy : float option -> placement list

val place_data : placement_strategy -> float option -> placement list

val optimize_placement : float -> placement list

val apply_placements : placement list -> unit
