open Atomic

module type LATTICE = sig
  type t
  val bot : t
  val merge : t -> unit
  val reveal : unit -> t
  val assign : t -> unit
end

module MakeAtomicLattice (T : Atomic.AtomicType) : LATTICE with type t = T.t = struct
  type t = T.t Atomic.t
  let bot = Atomic.make (T.zero ())
  let element = Atomic.make (T.zero ())

  let do_merge e =
    let current = Atomic.load element in
    let new_value = T.merge current e in
    Atomic.store element new_value

  let init () =
    Atomic.store element (T.zero ())

  let assign e =
    Atomic.store element e

  let reveal () =
    Atomic.load element

  let bot () =
    Atomic.load bot

  let merge e =
    do_merge e

  let assign e =
    Atomic.store element e
end

module type LATITUDE = sig
  type t
  val bot : t
   so