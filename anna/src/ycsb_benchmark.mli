module YCSB : sig
  val record_count : int
  val operation_count : int
  val key_length : int
  val value_length : int

  val generate_key : unit -> string
  val generate_value : unit -> string

  val load_phase : unit -> unit

  val run_workload : float -> float -> float -> float * float list

  val run_benchmark : unit -> unit
end

val () : unit
