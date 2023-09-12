let main () =
  let chord_node = init () in
  add_key chord_node "key1" "value1";
  let result = lookup_key chord_node "key1" in
  match result with
  | Some value -> Printf.printf "Found: %s\n" value
  | None -> Printf.printf "Key not found\n"

let () = main ()
