open Metadata

type query = {
  table_id: table_id;
  column_ids: column_id list;
}

val parse_query : string -> query

val run_query : string -> unit

val optimize_storage : float -> unit

val benchmark : string list -> float

val run_mosaic : float -> string list -> unit
