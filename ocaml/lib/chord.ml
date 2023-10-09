module Api = Chord_api.MakeRPC(Capnp_rpc_lwt)

open Lwt.Infix
open Capnp_rpc_lwt

let local =
  let module Chord = Api.Service.Chord in
  Chord.local @@ object
    inherit Chord.service

    method find_successor_impl params release_param_caps =
      let open Chord.findSuccessor in
      let id = Params.msg_get params in
      release_param_caps();

      (* Get predecessor of id *)
      let open Chord.findPredecessor in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.msg_set params id;
      Capability.call_for_value_exn t method_id request >|= 

      (* Get successor of predecessor *)
      let predecessor_uri = Results.reply_get in
      let open Chord.getSuccessor in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.msg_set params id;
      Capability.call_for_value_exn t method_id request >|= 

      (* Return id's predecessor successor *)
      let successor_uri = Results.reply_get;
      let response, results = Service.Response.create Results.init_pointer in
      Results.reply_set results sucessor_uri;
      Service.return response

    method find_predecessor_impl params release_params =
      let open Chord.findPredecessor in
      let id = Params.msg_get params in
      release_param_caps();

      let ref node = t in
      while is_between id !node.id !node.successor.id = False do
        let open Chord.FindClosestPrecedingFinger in
        let request, params = Capability.Request.create Params.init_pointer in
        Params.msg_set params id;
        Capability.call_for_value_exn t method_id request >|=
        let closest_preceding_finger = Results.reply_get in
        node != closest_preceding_finger
      done
      node

    method find_closest_preceding_finger_impl params release_params =
      let m = List.length finger_table in
      let rec find_closest idx =
        | 0 -> t
        | i ->
          let finger = List.nth finger_table i in
          let finger_id = finger.id in
          let in_range = is_in_range finger_id t.id id in
          match in_range
          | True -> finger
          | False -> find_closest idx - 1
      in
      find_closest m
  end