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
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) = struct
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t
  module CamlBytes = Bytes
  module DefaultsMessage_ = Capnp.BytesMessage

  let _builder_defaults_message =
    let message_segments = [
      Bytes.unsafe_of_string "\
      ";
    ] in
    DefaultsMessage_.Message.readonly
      (DefaultsMessage_.Message.of_storage message_segments)

  let invalid_msg = Capnp.Message.invalid_msg

  include Capnp.Runtime.BuilderInc.Make[@inlined](MessageWrapper)

  type 'cap message_t = 'cap MessageWrapper.Message.t

  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.BytesMessage)(MessageWrapper)

  let _reader_defaults_message =
    MessageWrapper.Message.create
      (DefaultsMessage_.Message.total_size _builder_defaults_message)


  module Reader = struct
    type array_t = ro MessageWrapper.ListStorage.t
    type builder_array_t = rw MessageWrapper.ListStorage.t
    type pointer_t = ro MessageWrapper.Slice.t option
    let of_pointer = RA_.deref_opt_struct_pointer

    module ChordNode = struct
      type t = [`ChordNode_b0c059eb806c07f5]
      module Node = struct
        type struct_t = [`Node_ff445a35f08facac]
        type t = struct_t reader_t
        let id_get x =
          RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (id_get x)
        let has_address x =
          RA_.has_field x 0
        let address_get x =
          RA_.get_text ~default:"" x 0
        let has_uri x =
          RA_.has_field x 1
        let uri_get x =
          RA_.get_text ~default:"" x 1
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module FindSuccessor = struct
        module Params = struct
          type struct_t = [`FindSuccessor_c3afd012bbcc2aff]
          type t = struct_t reader_t
          let request_get x =
            RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
          let request_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint32_exn (request_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module FindPredecessor = struct
        module Params = struct
          type struct_t = [`FindPredecessor_b0ee27779bbbb226]
          type t = struct_t reader_t
          let request_get x =
            RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
          let request_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint32_exn (request_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module FindClosestPrecedingFinger = struct
        module Params = struct
          type struct_t = [`FindClosestPrecedingFinger_b52dfb36dd71e2cb]
          type t = struct_t reader_t
          let request_get x =
            RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
          let request_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint32_exn (request_get x)
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
      module Join = struct
        module Params = struct
          type struct_t = [`Join_e919e4c9a42d8c39]
          type t = struct_t reader_t
          let has_request x =
            RA_.has_field x 0
          let request_get x =
            RA_.get_struct x 0
          let request_get_pipelined x =
            MessageWrapper.Untyped.struct_field x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        module Results = struct
          type struct_t = [`Join_efb6a76fa721add1]
          type t = struct_t reader_t
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
      end
    end
  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

    module ChordNode = struct
      type t = [`ChordNode_b0c059eb806c07f5]
      module Node = struct
        type struct_t = [`Node_ff445a35f08facac]
        type t = struct_t builder_t
        let id_get x =
          BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (id_get x)
        let id_set x v =
          BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
        let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
        let has_address x =
          BA_.has_field x 0
        let address_get x =
          BA_.get_text ~default:"" x 0
        let address_set x v =
          BA_.set_text x 0 v
        let has_uri x =
          BA_.has_field x 1
        let uri_get x =
          BA_.get_text ~default:"" x 1
        let uri_set x v =
          BA_.set_text x 1 v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
      end
      module FindSuccessor = struct
        module Params = struct
          type struct_t = [`FindSuccessor_c3afd012bbcc2aff]
          type t = struct_t builder_t
          let request_get x =
            BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
          let request_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint32_exn (request_get x)
          let request_set x v =
            BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
          let request_set_int_exn x v = request_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
        end
      end
      module FindPredecessor = struct
        module Params = struct
          type struct_t = [`FindPredecessor_b0ee27779bbbb226]
          type t = struct_t builder_t
          let request_get x =
            BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
          let request_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint32_exn (request_get x)
          let request_set x v =
            BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
          let request_set_int_exn x v = request_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
        end
      end
      module FindClosestPrecedingFinger = struct
        module Params = struct
          type struct_t = [`FindClosestPrecedingFinger_b52dfb36dd71e2cb]
          type t = struct_t builder_t
          let request_get x =
            BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
          let request_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint32_exn (request_get x)
          let request_set x v =
            BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
          let request_set_int_exn x v = request_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
        end
      end
      module Join = struct
        module Params = struct
          type struct_t = [`Join_e919e4c9a42d8c39]
          type t = struct_t builder_t
          let has_request x =
            BA_.has_field x 0
          let request_get x =
            BA_.get_struct ~data_words:1 ~pointer_words:2 x 0
          let request_set_reader x v =
            BA_.set_struct ~data_words:1 ~pointer_words:2 x 0 v
          let request_set_builder x v =
            BA_.set_struct ~data_words:1 ~pointer_words:2 x 0 (Some v)
          let request_init x =
            BA_.init_struct ~data_words:1 ~pointer_words:2 x 0
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
        end
        module Results = struct
          type struct_t = [`Join_efb6a76fa721add1]
          type t = struct_t builder_t
          let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:0 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:0 ()
          let init_pointer ptr =
            BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:0
        end
      end
    end
  end

  module Client = struct
    module ChordNode = struct
      type t = [`ChordNode_b0c059eb806c07f5]
      let interface_id = Stdint.Uint64.of_string "0xb0c059eb806c07f5"
      module FindSuccessor = struct
        module Params = Builder.ChordNode.FindSuccessor.Params
        module Results = Reader.ChordNode.Node
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      module FindPredecessor = struct
        module Params = Builder.ChordNode.FindPredecessor.Params
        module Results = Reader.ChordNode.Node
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:1
      end
      module FindClosestPrecedingFinger = struct
        module Params = Builder.ChordNode.FindClosestPrecedingFinger.Params
        module Results = Reader.ChordNode.Node
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:2
      end
      module Join = struct
        module Params = Builder.ChordNode.Join.Params
        module Results = Reader.ChordNode.Join.Results
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:3
      end
      let method_name = function
        | 0 -> Some "findSuccessor"
        | 1 -> Some "findPredecessor"
        | 2 -> Some "findClosestPrecedingFinger"
        | 3 -> Some "join"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"ChordNode" method_name
    end
  end

  module Service = struct
    module ChordNode = struct
      type t = [`ChordNode_b0c059eb806c07f5]
      let interface_id = Stdint.Uint64.of_string "0xb0c059eb806c07f5"
      module FindSuccessor = struct
        module Params = Reader.ChordNode.FindSuccessor.Params
        module Results = Builder.ChordNode.Node
      end
      module FindPredecessor = struct
        module Params = Reader.ChordNode.FindPredecessor.Params
        module Results = Builder.ChordNode.Node
      end
      module FindClosestPrecedingFinger = struct
        module Params = Reader.ChordNode.FindClosestPrecedingFinger.Params
        module Results = Builder.ChordNode.Node
      end
      module Join = struct
        module Params = Reader.ChordNode.Join.Params
        module Results = Builder.ChordNode.Join.Results
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#find_successor_impl
          | 1 -> MessageWrapper.Untyped.abstract_method self#find_predecessor_impl
          | 2 -> MessageWrapper.Untyped.abstract_method self#find_closest_preceding_finger_impl
          | 3 -> MessageWrapper.Untyped.abstract_method self#join_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "ChordNode"
        method virtual find_successor_impl : (FindSuccessor.Params.t, FindSuccessor.Results.t) MessageWrapper.Service.method_t
        method virtual find_predecessor_impl : (FindPredecessor.Params.t, FindPredecessor.Results.t) MessageWrapper.Service.method_t
        method virtual find_closest_preceding_finger_impl : (FindClosestPrecedingFinger.Params.t, FindClosestPrecedingFinger.Results.t) MessageWrapper.Service.method_t
        method virtual join_impl : (Join.Params.t, Join.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
  end
  module MessageWrapper = MessageWrapper
end [@@inline]

module Make(M:Capnp.MessageSig.S) = MakeRPC[@inlined](Capnp.RPC.None(M)) [@@inline]
