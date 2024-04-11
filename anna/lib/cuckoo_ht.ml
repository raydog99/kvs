open Base

module CuckooHashTable = struct
  let table_size = 8
  let num_tables = 2
  let tables = Array.make num_tables [||]

  let hash1 x = x mod table_size
  let hash2 x = (x / table_size) mod table_size

  let rec insert x =
    let i1 = hash1 x and i2 = hash2 x in
    if tables.(0).(i1) = 0 then tables.(0).(i1) <- x
    else if tables.(1).(i2) = 0 then tables.(1).(i2) <- x
    else (
      let y = tables.(0).(i1) in
      tables.(0).(i1) <- x;
      insert y)

  let rec find x =
    let i1 = hash1 x and i2 = hash2 x in
    if tables.(0).(i1) = x || tables.(1).(i2) = x then true else false
end