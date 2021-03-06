(** This API provides functions to interact with a Tezos node *)

open Tezos_client_009_PsFLoren
open Tezos_protocol_009_PsFLoren.Protocol.Alpha_context
open Api_error

(* open Tezos_raw_protocol_009_PsFLoren *)
open Base

(** A public key of an account (implicit or originated)*)
type puk

(** A public key hash of an account (implicit or originated) *)
type pukh

 (** Contract representation (implicit or originated) *)
type contract

 (** Operation hash *)
type oph

(** Block hash *)
type blockh

type parsed_michelson = Michelson_v1_parser.parsed
type expression_michelson = Script.expr
type tag = string


(* mtype is the michelson types for the arg and initial_storage *)
type mtype = 
| Tstring of string
| Tint of int 
| Tbool of bool
| Tunit of unit
| Tlist of mtype list
| Toption of mtype option
| Tpair of (mtype * mtype)
| Tbytes (*starts with 0x*) of Bytes.t  
| Tkey of Signature.public_key
| Tkey_hash of Signature.public_key_hash
| Tsignature of Signature.t
| Tbls12_381_g1 (*is a byte*) of Bytes.t
| Tbls12_381_g2 (*is a byte*) of Bytes.t
| Tbls12_381_fr of Bls12_381.Fr.t
| Tnever


val mtype_to_string: mtype -> string

(* gets the value of mtype in a string format that can be passed to the originate and call_contract functions *)
val value_to_string: mtype -> string


(** Representation of Tezos tokens (tez) *)
module Tez_t : sig
  type t

  (** [tez f] convert amount to the internal Tez representation
      throws an exception if value is below 1 micro tez (0.000001)
   *)
  val tez : float -> t

  (** [zero] represents the value of 0 tez *)
  val zero : t

  (** [to_float t] returns amount of tez in float represention *)
  val to_float : t -> float
end

(** Result of an operation which has been successfully included into a block *)
type op_result = {
    block_hash : blockh; (** Block in which transaction was included *)
    rpc_position : (int * int); (** The indices where the operation can be found within the block *)
    balance_updates : Receipt.balance_updates; (** List of balance updates *)
    consumed_gas : int; (** Amount of gas burned during execution *)
    storage : Script.expr option; (** Contents of storage *)
    originated_contracts : contract list; (** List of originated contracts *)
    storage_size : int; (** Amount of storage used during transaction *)
    paid_storage_size_diff : int ; (** Storage fees paid *)
    big_map_diff : Lazy_storage.diffs option ; (** Changes in the BigMap *)
    allocated_destination_contract : bool (** Indicates whether the dest account was empty or not *)
  }

(** Possible reasons for a transaction rejection *)
type reason = Timeout (** The transaction timed out and was removed from the Mempool (tbd if this case can be distinguished clearly from others) *)
            | Skipped (** The transaction was skipped due to a previously failed operation *)
            | Backtracked (** The transaction was backtracked due to a subsequently failed operation *)
            | Reason of rejection_message  (** The transaction failed due to {!type: failure_message} *)
            | Unknown_reason of string (** None of the above match - reason list should be extended if this occurs *)

(** Status of an injected transaction *)
type status = Still_pending (** Transaction hasn't been included yet (prevalidated, delayed or unprocessed) *)
            | Accepted of op_result (** Transaction was included with {!type:result} *)
            | Rejected of reason (** Transaction was rejected due to {!type:reason} *)
            | Unprocessed (** Transaction not yet prevalidated *)
            | Missing (** Transaction couldn't be found (tbd should this be timeout?) *)

(** [get_puk_from_alias s] expects an alias of an implicit account and returns
    the associated public key of the account.
    @param s alias of implicit account
    @return {!type:puk} the associated public key
*)
val get_puk_from_alias: string -> puk Answer.t

(** [get_puk_from_hash s] expects a public key hash as string and returns the associated
    public key of the account.
    @param s public key hash
    @return {!type:puk} the associated public key
 *)
val get_puk_from_hash: string -> puk Answer.t

(** [get_pukh_from_alias s] expects an alias of an implicit account and returns
    the associated public key hash.
    @param s alias of implicit account
    @return {!type:pukh} the associated public key hash
*)
val get_pukh_from_alias: string -> pukh Answer.t

(** [get_pukh_from_hash s] expects a public key hash as string and returns
    the associated public key hash.
    @param s public key hash of implicit account
    @return {!type:pukh} the associated public key hash
*)
val get_pukh_from_hash: string -> pukh Answer.t

(** [string_of_pukh pkh] expects a public key hash and returns it's string representation..
    @param pkh public key hash of implicit account
    @return its string representation
*)
val string_of_pukh: pukh -> string

(** [get_contract s] expects an alias, public key hash or contract hash as string
    and returns the associated contract representation.
    @param s alias, public key hash or contract hash
    @return {!type:contract} the associated contract representation
*)
val get_contract: string -> contract Answer.t

(** [set_port p] specifies under which port the RPC interface of the node is
    reachable if the default (8732) does not apply.
    @param p the RPC port of the node
*)
val set_port: int -> unit

(** [set_debugmode f] enable/disable the debug mode - in debug mode, the whole Tezos error trace is printed *)
val set_debugmode : bool -> unit

(** [set_basedir d] specifies the path of the tezos-client base directory
    (normally /home/<username>/.tezos-client/)
    @param d path of the tezos-client base directory
*)
val set_basedir: string -> unit

(** [set_fee_parameters min_fee min_ntz_byte min_ntz_gas force_low fee_cap burn_cap]
 sets the fee parameters for transactions and calls. Unspecified parameters are reset to their default values.
    @param min_fee exclude operations with fees lower than this threshold
    @param min_ntz_byte exclude operations with fees per byte lower than this threshold (in nanotez)
    @param min_ntz_gas exclude operations with fees per gas lower than this threshold (in nanotez)
    @param force_low Don't check that the fee is lower than the estimated default value
    @param fee_cap
    @param burn_cap
*)
val set_fee_parameters: ?min_fee:Tez_t.t
                        -> ?min_ntz_byte:int
                        -> ?min_ntz_gas:int
                        -> ?force_low:bool
                        -> ?fee_cap:Tez_t.t
                        -> ?burn_cap:Tez_t.t
                        -> unit
                        -> unit

(** [transfer a src dst fee] injects a transfer transaction.
    @param a the amount of tez to transfer
    @param src the public key hash of the sender
    @param dst the contract representation of the receiver
    @param fee the amount of fees to pay to the baker
    @return {!type:oph} the operation hash of the injected transaction
*)
val transfer: Tez_t.t -> pukh -> contract -> Tez_t.t -> oph Answer.t

(** [query op] retrieves the current status of an injected transaction
    @param op the operation hash of the injected transaction
    @return {!type:status} the status of the transaction
*)
val query : oph -> status Answer.t

(** [get_balance c] returns the balance of a contract (implicit or originated)
    @param f the contract representation of the target
    @return {!type:Tez_t.t} the balance of the target contract
*)
val get_balance : contract -> Tez_t.t Answer.t

(** [call_contract a src dst ?ep ?arg fee] calls a contract.
    @param a amount of tez to transfer
    @param src the public key hash of the caller
    @param dst the contract representation of the callee
    @param ?ep specifies the entrypoint - if not set, the default entrypoint will be called
    @param ?arg argument passed to the contract's script (if needed)
    @param fee the amount of fees to pay to the baker
 *)
 (* what is the differnce between call_contract and transfer *)
val call_contract : Tez_t.t -> pukh -> contract -> ?entrypoint:string -> ?arg:string -> Tez_t.t -> oph Answer.t

(** [get_contract_code dst] retrieves the code of a smart contract.
    @param dst contract representation
    @return the parsed contract code
 *)
val get_contract_code : contract -> parsed_michelson Answer.t

(** [parse_script code] parses a Michelson script
    @param code Michelson script
    @return the result of parsing and expanding a Michelson script
 *)
 (* I don't really aunderstand what is the parsed_michelson *)
val parse_script : string -> parsed_michelson Answer.t

(** [parse_expression expr] parses a Michelson expression/data
    @param code Michelson expression
    @return the result of parsing and expanding a Michelson expression/data
 *)
val parse_expression : string -> parsed_michelson Answer.t

(** [list_entrypoints parsed] returns the list of entrypoints of the given Michelson script
    @param a parsed Michelson script
    @return the entrypoints with tag and script
 *)
val list_entrypoints : parsed_michelson -> (tag * expression_michelson) list Answer.t

(* gets the balance of a given alias *)
val get_balance1 : string -> Tez_t.t Answer.t

(* prints the tez_t.t *)
val get_float : Tez_t.t -> unit

(*  gets all of the entrypoint for a contract 
    @param a string having the puplic_key_hash of a conract
    @return list of paris entrypoints containing the name of the entrypoint and its type *)
val get_entrypoints: string -> (string * Script.expr) list Answer.t

(*  prints the names of the given entrypoints
    @param list of pairs entrypoints *)
val print_elements: (tag * Script.expr) list -> unit 


(* val print_entrypoints: (tag * Script.expr) list tzresult -> unit tzresult Lwt.t *)

(*  gets the string of an expression
    @param the expression
    @return string fromat of expression *)
val string_of_expression: Script.expr -> string

(*  gets the expression of a lazy expression 
    @param lazy_expr
    @retuen expr *)
val get_expr_from_lexpr: Script.lazy_expr -> Script.expr Answer.t


val check_type: string -> contract -> mtype -> string Answer.t

val parse_arg_transfer: string option -> Script.expr Answer.t

val try1: ?arg:string -> unit -> string Answer.t

val check_type2: string -> contract -> ?arg:string -> unit -> string Answer.t

val get_entry: ?entrypoint:string -> unit -> string

val call_contract2: Tez_t.t -> pukh -> contract -> ?entrypoint:string -> ?arg:string -> Tez_t.t -> oph Answer.t

val call_contract3: Tez_t.t -> pukh -> contract -> ?entrypoint:string -> ?arg:mtype -> Tez_t.t -> oph Answer.t

val get_mtype_option: ?param:mtype -> unit -> mtype

val originate: 
  string ->
  Tez_t.t ->
  Tez_t.t ->
  pukh ->
  string ->
  (Kind.origination Kind.manager Injection.result * Contract.t) Answer.t
    (* the next 8 can be not used *)
  (* ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?branch:int ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  (* will put delegate with none now *)
  delegate:public_key_hash option -> *)
  (* I am not really sure but maybe this will be compared to the storage type from the script of the contract code, 
  take a string with the initial storage *)
  (* initial_storage:string -> *)
  (* the transfered balanced from source to the "to be originated" contract, take Tez_t.t*)
  (* balance:Tez.t -> *)
  (* pukh, pk, sk for source -> will use pukh instead of the next 3*)
  (* source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri -> *)
  (* the code of the contract in the script code of the contract instead of it I will take the string of the michelson contract*)
  (* code:Script.expr -> *)
  (* fee parameter is !feeparameter in the API code
  fee_parameter:Injection.fee_parameter -> *)

  val originate2: 
  mtype ->
  Tez_t.t ->
  Tez_t.t ->
  pukh ->
  string ->
  (Kind.origination Kind.manager Injection.result * Contract.t) Answer.t

  val print_code: string -> string Answer.t

  val micheline_string_of_expression: Script.expr -> string

  val check_storage_type: string -> string -> string Answer.t