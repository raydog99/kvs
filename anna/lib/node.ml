open Lwt.Infix
open Capnp_rpc_lwt
open Capnp_rpc_net
open Capnp_rpc_unix
open Hashtbl

module Restorer = Capnp_rpc_net.Restorer

module ChordNodeImpl = struct
  type t = {
    id: string;
    address: string;
    uri: string;
  }

  type config = {
    host : string;
    port : int;
  }

  let cap_file = "chord.cap"  

  let serve config =
    Lwt_main.run begin
      let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
      let restore = Restorer.single service_id Chord.local in
      Capnp_rpc_unix.serve config ~restore >>= fun vat ->
      match Capnp_rpc_unix.Cap_file.save_service vat service_id cap_file with
      | Error `Msg m -> failwith m
      | Ok () ->
        Fmt.pr "Server running. Connect using %S.@." cap_file;
        fst @@ Lwt.wait ()
    end

  let init config =
    serve config;;
end