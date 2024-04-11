open Unix
open Capnp_rpc_unix
open Node

let my_addr () =
  try
    let my_host_info = gethostbyname (gethostname() ) in
    let addr_list = (Array.to_list my_host_info.h_addr_list) in
    let rec find_non_loopback_addr = function
      | [] -> failwith "my_addr: no non-loopback IP address found."
      | h :: t ->
        let addr_str = Unix.string_of_inet_addr h in
        if addr_str = "127.0.0.1" then
          find_non_loopback_addr t
        else
          addr_str
      in
      find_non_loopback_addr addr_list
  with
  | Not_found -> 
    failwith "my_addr: cannot find my IP address."

type config = {
  host : string;
  port : int;
}
(* 
let main () =
  let port = 0 in
  let host = my_addr () in
  let node_config = { host = host; port = port } in
  let chord_node_uri = ChordNodeImpl.init chord_config >>= fun chord_node_uri ->
    Printf.printf "Chord Node URI: %s\n" chord_node_uri;
  Lwt.return ();; *)

open Cmdliner

let serve_cmd =
  let doc = "run the server" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const ChordNodeImpl.serve $ Capnp_rpc_unix.Vat_config.cmd)

let () =
  exit (Cmd.eval serve_cmd)