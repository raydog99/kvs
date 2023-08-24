module B = Bencode
module R = Rpc

module type RPC = Rpc.S

module type CONFIG = sig
	val dimension: int
	val redundancy: int
	val finger_frequency: int
	val node_timeout: int
	val timeout: int
end

module ConfigDefault : CONFIG = struct
	let dimension = 100
	let redundancy = 5
	let finger_frequency = 5
	let node_timeout = 300
	let timeout = 3
end

module type S = sig
	module Config : CONFIG
	module Rpc : RPC.S

	(* node UID *)
	module ID : sig
		type t

		val of_string : string -> t
		val to_string : t -> string
		val eq : t -> t -> bool
		val hash : t -> int
	end
end
