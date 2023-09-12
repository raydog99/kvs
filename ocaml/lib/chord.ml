module type ChordNode = sig
  type t

  val init : unit -> t

  val add_key : t -> string -> string -> unit

  val lookup_key : t -> string -> string option
end
