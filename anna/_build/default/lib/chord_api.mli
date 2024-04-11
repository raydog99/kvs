[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t


  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module ChordNode : sig
      type t = [`ChordNode_b0c059eb806c07f5]
      module Node : sig
        type struct_t = [`Node_ff445a35f08facac]
        type t = struct_t reader_t
        val id_get : t -> Stdint.Uint32.t
        val id_get_int_exn : t -> int
        val has_address : t -> bool
        val address_get : t -> string
        val has_uri : t -> bool
        val uri_get : t -> string
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module FindSuccessor : sig
        module Params : sig
          type struct_t = [`FindSuccessor_c3afd012bbcc2aff]
          type t = struct_t reader_t
          val request_get : t -> Stdint.Uint32.t
          val request_get_int_exn : t -> int
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`FindSuccessor_e3102afd8b1bd254]
          type t = struct_t reader_t
          val has_reply : t -> bool
          val reply_get : t -> [`Node_ff445a35f08facac] reader_t
          val reply_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Node_ff445a35f08facac] MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module FindPredecessor : sig
        module Params : sig
          type struct_t = [`FindPredecessor_b0ee27779bbbb226]
          type t = struct_t reader_t
          val request_get : t -> Stdint.Uint32.t
          val request_get_int_exn : t -> int
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`FindPredecessor_a9ef24e0d3a75524]
          type t = struct_t reader_t
          val has_reply : t -> bool
          val reply_get : t -> [`Node_ff445a35f08facac] reader_t
          val reply_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Node_ff445a35f08facac] MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module FindClosestPrecedingFinger : sig
        module Params : sig
          type struct_t = [`FindClosestPrecedingFinger_b52dfb36dd71e2cb]
          type t = struct_t reader_t
          val request_get : t -> Stdint.Uint32.t
          val request_get_int_exn : t -> int
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`FindClosestPrecedingFinger_e611c63eefa04aed]
          type t = struct_t reader_t
          val has_reply : t -> bool
          val reply_get : t -> [`Node_ff445a35f08facac] reader_t
          val reply_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Node_ff445a35f08facac] MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module Join : sig
        module Params : sig
          type struct_t = [`Join_e919e4c9a42d8c39]
          type t = struct_t reader_t
          val has_request : t -> bool
          val request_get : t -> [`Node_ff445a35f08facac] reader_t
          val request_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Node_ff445a35f08facac] MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`Join_efb6a76fa721add1]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
      module GetSuccessor : sig
        module Params : sig
          type struct_t = [`GetSuccessor_f92ca3eecb46578f]
          type t = struct_t reader_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Results : sig
          type struct_t = [`GetSuccessor_c5542eceae3fb949]
          type t = struct_t reader_t
          val has_reply : t -> bool
          val reply_get : t -> [`Node_ff445a35f08facac] reader_t
          val reply_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Node_ff445a35f08facac] MessageWrapper.StructRef.t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
      end
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t
    module ChordNode : sig
      type t = [`ChordNode_b0c059eb806c07f5]
      module Node : sig
        type struct_t = [`Node_ff445a35f08facac]
        type t = struct_t builder_t
        val id_get : t -> Stdint.Uint32.t
        val id_get_int_exn : t -> int
        val id_set : t -> Stdint.Uint32.t -> unit
        val id_set_int_exn : t -> int -> unit
        val has_address : t -> bool
        val address_get : t -> string
        val address_set : t -> string -> unit
        val has_uri : t -> bool
        val uri_get : t -> string
        val uri_set : t -> string -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module FindSuccessor : sig
        module Params : sig
          type struct_t = [`FindSuccessor_c3afd012bbcc2aff]
          type t = struct_t builder_t
          val request_get : t -> Stdint.Uint32.t
          val request_get_int_exn : t -> int
          val request_set : t -> Stdint.Uint32.t -> unit
          val request_set_int_exn : t -> int -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`FindSuccessor_e3102afd8b1bd254]
          type t = struct_t builder_t
          val has_reply : t -> bool
          val reply_get : t -> [`Node_ff445a35f08facac] builder_t
          val reply_set_reader : t -> [`Node_ff445a35f08facac] reader_t -> [`Node_ff445a35f08facac] builder_t
          val reply_set_builder : t -> [`Node_ff445a35f08facac] builder_t -> [`Node_ff445a35f08facac] builder_t
          val reply_init : t -> [`Node_ff445a35f08facac] builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module FindPredecessor : sig
        module Params : sig
          type struct_t = [`FindPredecessor_b0ee27779bbbb226]
          type t = struct_t builder_t
          val request_get : t -> Stdint.Uint32.t
          val request_get_int_exn : t -> int
          val request_set : t -> Stdint.Uint32.t -> unit
          val request_set_int_exn : t -> int -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`FindPredecessor_a9ef24e0d3a75524]
          type t = struct_t builder_t
          val has_reply : t -> bool
          val reply_get : t -> [`Node_ff445a35f08facac] builder_t
          val reply_set_reader : t -> [`Node_ff445a35f08facac] reader_t -> [`Node_ff445a35f08facac] builder_t
          val reply_set_builder : t -> [`Node_ff445a35f08facac] builder_t -> [`Node_ff445a35f08facac] builder_t
          val reply_init : t -> [`Node_ff445a35f08facac] builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module FindClosestPrecedingFinger : sig
        module Params : sig
          type struct_t = [`FindClosestPrecedingFinger_b52dfb36dd71e2cb]
          type t = struct_t builder_t
          val request_get : t -> Stdint.Uint32.t
          val request_get_int_exn : t -> int
          val request_set : t -> Stdint.Uint32.t -> unit
          val request_set_int_exn : t -> int -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`FindClosestPrecedingFinger_e611c63eefa04aed]
          type t = struct_t builder_t
          val has_reply : t -> bool
          val reply_get : t -> [`Node_ff445a35f08facac] builder_t
          val reply_set_reader : t -> [`Node_ff445a35f08facac] reader_t -> [`Node_ff445a35f08facac] builder_t
          val reply_set_builder : t -> [`Node_ff445a35f08facac] builder_t -> [`Node_ff445a35f08facac] builder_t
          val reply_init : t -> [`Node_ff445a35f08facac] builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module Join : sig
        module Params : sig
          type struct_t = [`Join_e919e4c9a42d8c39]
          type t = struct_t builder_t
          val has_request : t -> bool
          val request_get : t -> [`Node_ff445a35f08facac] builder_t
          val request_set_reader : t -> [`Node_ff445a35f08facac] reader_t -> [`Node_ff445a35f08facac] builder_t
          val request_set_builder : t -> [`Node_ff445a35f08facac] builder_t -> [`Node_ff445a35f08facac] builder_t
          val request_init : t -> [`Node_ff445a35f08facac] builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`Join_efb6a76fa721add1]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
      module GetSuccessor : sig
        module Params : sig
          type struct_t = [`GetSuccessor_f92ca3eecb46578f]
          type t = struct_t builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Results : sig
          type struct_t = [`GetSuccessor_c5542eceae3fb949]
          type t = struct_t builder_t
          val has_reply : t -> bool
          val reply_get : t -> [`Node_ff445a35f08facac] builder_t
          val reply_set_reader : t -> [`Node_ff445a35f08facac] reader_t -> [`Node_ff445a35f08facac] builder_t
          val reply_set_builder : t -> [`Node_ff445a35f08facac] builder_t -> [`Node_ff445a35f08facac] builder_t
          val reply_init : t -> [`Node_ff445a35f08facac] builder_t
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
      end
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
    module ChordNode : sig
      type t = [`ChordNode_b0c059eb806c07f5]
      val interface_id : Stdint.Uint64.t
      module FindSuccessor : sig
        module Params = Builder.ChordNode.FindSuccessor.Params
        module Results = Reader.ChordNode.FindSuccessor.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module FindPredecessor : sig
        module Params = Builder.ChordNode.FindPredecessor.Params
        module Results = Reader.ChordNode.FindPredecessor.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module FindClosestPrecedingFinger : sig
        module Params = Builder.ChordNode.FindClosestPrecedingFinger.Params
        module Results = Reader.ChordNode.FindClosestPrecedingFinger.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module Join : sig
        module Params = Builder.ChordNode.Join.Params
        module Results = Reader.ChordNode.Join.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
      module GetSuccessor : sig
        module Params = Builder.ChordNode.GetSuccessor.Params
        module Results = Reader.ChordNode.GetSuccessor.Results
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
  end

  module Service : sig
    module ChordNode : sig
      type t = [`ChordNode_b0c059eb806c07f5]
      val interface_id : Stdint.Uint64.t
      module FindSuccessor : sig
        module Params = Reader.ChordNode.FindSuccessor.Params
        module Results = Builder.ChordNode.FindSuccessor.Results
      end
      module FindPredecessor : sig
        module Params = Reader.ChordNode.FindPredecessor.Params
        module Results = Builder.ChordNode.FindPredecessor.Results
      end
      module FindClosestPrecedingFinger : sig
        module Params = Reader.ChordNode.FindClosestPrecedingFinger.Params
        module Results = Builder.ChordNode.FindClosestPrecedingFinger.Results
      end
      module Join : sig
        module Params = Reader.ChordNode.Join.Params
        module Results = Builder.ChordNode.Join.Results
      end
      module GetSuccessor : sig
        module Params = Reader.ChordNode.GetSuccessor.Params
        module Results = Builder.ChordNode.GetSuccessor.Results
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual find_successor_impl : (FindSuccessor.Params.t, FindSuccessor.Results.t) MessageWrapper.Service.method_t
        method virtual find_predecessor_impl : (FindPredecessor.Params.t, FindPredecessor.Results.t) MessageWrapper.Service.method_t
        method virtual find_closest_preceding_finger_impl : (FindClosestPrecedingFinger.Params.t, FindClosestPrecedingFinger.Results.t) MessageWrapper.Service.method_t
        method virtual join_impl : (Join.Params.t, Join.Results.t) MessageWrapper.Service.method_t
        method virtual get_successor_impl : (GetSuccessor.Params.t, GetSuccessor.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
