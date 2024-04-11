module CuckooHashTable :
  sig
    val table_size : int
    val num_tables : int
    val tables : int array array
    val hash1 : int -> int
    val hash2 : int -> int
    val insert : int -> unit
    val find : int -> bool
  end
