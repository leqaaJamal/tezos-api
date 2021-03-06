open Client_keys
open Tezos_client_009_PsFLoren
open Tezos_client_009_PsFLoren.Protocol_client_context
open Tezos_client_009_PsFLoren.Injection
open Tezos_client_009_PsFLoren.Client_proto_contracts
open Tezos_protocol_009_PsFLoren.Protocol.Alpha_context
open Tezos_raw_protocol_009_PsFLoren
open Tezos_protocol_environment_009_PsFLoren
open Apply_results
open Api_error
open Api_context
open Format
open Base
open Int64
open Tezos_micheline
open Micheline


(* "String (%d, \"%s\")" *)
type puk = Signature.public_key
type pukh = Signature.public_key_hash
type contract = Contract.t
type oph = Operation_hash.t
type blockh = Block_hash.t
type parsed_michelson = Michelson_v1_parser.parsed
type expression_michelson = Script.expr
type tag = string



(* get_contract_address_maybe_chain_id
Contract.to_b58check *)

(*  
    lambda ty1 ty2 -> mtype mtype
    or ty1 ty2 -> mtype mtype
    address of untyped contract ->
    chain_id 
    mutez ->
    nat natural numbers ->
    timestamp ->
    (* 
    sapling_state n
    sapling_transaction n
    set cty 
    ticket cty
    *)

    *)

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
(* | Tlambda -> mtype mtype *)
(* | Tor of mtype|mtype *)
(* | Or of [‘Left of ’l | ‘Right of ’r] Mtype *)

let mtype_to_string = function
| Tstring _-> "T_string"
| Tint _-> "T_int"
| Tbool _ -> "T_bool"
| Tunit _ -> "T_unit"
| Tlist _ -> "T_list"
| Toption _ -> "T_option"
| Tpair _ -> "T_pair"
| Tbytes _ -> "T_bytes"  
| Tkey _ -> "T_key"
| Tkey_hash _ -> "T_key_hash"
| Tsignature _ -> "T_signature"
| Tbls12_381_g1 _ -> "T_bls12_381_g1"
| Tbls12_381_g2 _ -> "T_bls12_381_g2"
| Tbls12_381_fr _ -> "T_bls12_381_fr"
| Tnever -> "T_never"


let rec value_to_string value =
  let rec string_of_list = function 
    [] -> asprintf ""
    | (hd::[]) ->(
        asprintf "(%s)%s"
      (value_to_string hd)
      (string_of_list [])
      )
    | (hd::tl) -> (
      asprintf "(%s) %s"
      (value_to_string hd)
      (string_of_list tl)
      )
  in 
  match value with 
  | Tstring x -> asprintf "\"%s\"" x
  | Tint x -> asprintf "%i" x
  | Tbool x ->
  (
    if x 
    then asprintf "bool True" 
    else asprintf "bool False"
  ) 
  | Tunit x -> asprintf "unit \"%s\"" (Unit.to_string x)
  | Tlist x -> 
  ( asprintf "list %s"
    (string_of_list x)
  )
  | Toption x -> 
  (
    match x with 
    | None -> asprintf "option None"
    | Some v -> asprintf "option Some %s" (value_to_string v)
  )
  | Tpair (rightx,leftx) -> asprintf "pair (%s) (%s)" (value_to_string rightx) (value_to_string leftx)
  | Tbytes x -> asprintf "%s" (Bytes.to_string x)
  | Tkey x -> asprintf "key \"%s\""  (Signature.Public_key.to_b58check x)
  | Tkey_hash x -> asprintf "key_hash \"%s\"" (Signature.Public_key_hash.to_string x)
  | Tsignature x -> asprintf "signature \"%s\"" (Signature.to_string x)
  | Tbls12_381_g1 x -> asprintf "bls12_381_g1 %s" (Bytes.to_string x)
  | Tbls12_381_g2 x -> asprintf "bls12_381_g2 %s" (Bytes.to_string x)
  | Tbls12_381_fr x -> asprintf "bls12_381_fr %s" (Bls12_381.Fr.to_string x)
  | Tnever -> asprintf "never"
(* Bytes.to_string (to_bytes s) *)
    
(* 
  | T_lambda ->
      "lambda"
  | T_nat ->
      "nat"
  | T_or ->
      "or"
  | T_mutez ->
      "mutez"
  | T_timestamp ->
      "timestamp"
  | T_address ->
      "address"
*)



module Tez_t : sig
  type t = Tez.t
  val tez : float -> t
  val zero : t
  val to_float : Tez.t -> float
end = struct
  type t = Tez.t
  let conversion_factor = 1000000.0
  let tez f =
    let mutez = Int64.of_float( f *. conversion_factor ) in
    if (mutez <= Int64.zero) then failwith ("Invalid value - must be at least 1 mutez");
    match Tez.( *? ) Tez.one_mutez mutez with
    | Ok tz -> tz
    | _ -> failwith "Illegal Tez value"
  let zero = Tez.zero
  let to_float tz =
    let mutez_f = Int64.to_float @@ Tez.to_mutez tz in
    (mutez_f /. conversion_factor)
end



type op_result = {
    block_hash : blockh;
    rpc_position : (int * int);
    balance_updates : Receipt.balance_updates;
    consumed_gas : int;
    storage : Script.expr option;
    originated_contracts : contract list;
    storage_size : int;
    paid_storage_size_diff : int ;
    big_map_diff : Lazy_storage.diffs option;
    allocated_destination_contract : bool
  }

type reason = Timeout
            | Skipped
            | Backtracked
            | Reason of rejection_message
            | Unknown_reason of string

type status = Still_pending
            | Accepted of op_result
            | Rejected of reason
            | Unprocessed
            | Missing

type config = {
    port : int ref;
    basedir : string ref;
    debug_mode : bool ref
  }

type fee_config = {
    minimal_fees : Tez.t ref;
    minimal_nanotez_per_byte : int ref;
    minimal_nanotez_per_gas_unit : int ref;
    force_low_fee : bool ref;
    fee_cap : Tez.t ref;
    burn_cap : Tez.t ref
}

type fee_config_default = {
    minimal_fees : Tez_t.t;
    minimal_nanotez_per_byte : int;
    minimal_nanotez_per_gas_unit : int;
    force_low_fee : bool;
    fee_cap : Tez_t.t;
    burn_cap : Tez_t.t
  }

let fee_config_def = {
    minimal_fees = (match Tez.of_mutez 100L with None -> assert false | Some t -> t);
    minimal_nanotez_per_byte = 1000;
    minimal_nanotez_per_gas_unit = 100;
    force_low_fee = false;
    fee_cap = (match Tez.of_string "1.0" with None -> assert false | Some t -> t);
    burn_cap = (match Tez.of_string "0" with None -> assert false | Some t -> t);
}

let current_config = {
    port = ref 8732;
    basedir = ref "/home/tezos/.tezos-client";
    debug_mode = ref false
  }

let current_fee_config : fee_config = {
    minimal_fees = ref fee_config_def.minimal_fees;
    minimal_nanotez_per_byte =
      ref fee_config_def.minimal_nanotez_per_byte;
    minimal_nanotez_per_gas_unit =
      ref fee_config_def.minimal_nanotez_per_gas_unit;
    force_low_fee = ref fee_config_def.force_low_fee;
    fee_cap = ref fee_config_def.fee_cap;
    burn_cap = ref fee_config_def.burn_cap
  }

let make_context () =
  let rpc_config : RPC_client_unix.config = {
      RPC_client_unix.default_config with
      endpoint = Uri.of_string "http://localhost:8732";
      
    }
  in
  new unix_full_silent
    ~chain:Client_config.default_chain
    ~block:Client_config.default_block
    ~confirmations:None
    ~password_filename:None
    ~base_dir: !(current_config.basedir)
    ~rpc_config:rpc_config

let ctxt = ref (make_context ()) (*Client_context.wallet *)
let catch_error_f err = if !(current_config.debug_mode) then catch_trace err
                        else catch_last_error err
let catch_error_env_f err errs s = if !(current_config.debug_mode) then
                                     catch_trace_env err errs s
                                   else catch_last_env_error err s

let make_fee_parameter () =
  let fp : fee_parameter = {
    minimal_fees = !(current_fee_config.minimal_fees);
    minimal_nanotez_per_byte = Q.of_int !(current_fee_config.minimal_nanotez_per_byte);
    minimal_nanotez_per_gas_unit = Q.of_int !(current_fee_config.minimal_nanotez_per_gas_unit);
    force_low_fee = !(current_fee_config.force_low_fee);
    fee_cap = !(current_fee_config.fee_cap);
    burn_cap = !(current_fee_config.burn_cap);
    }
  in
  fp

let fee_parameter = ref (make_fee_parameter ())

let exception_handler =
  (function
  | Failure msg ->
     Lwt.return_error [(Exn (Failure msg))]
  | exn ->
     Lwt.return @@ error_exn exn )

let get_puk_from_alias name =
  Public_key_hash.find !ctxt name
  >>= function
  | Ok pkh -> (
     Client_keys.get_key !ctxt pkh
     >>= function
     | Ok (_, src_pk, _) -> Answer.return src_pk
     | Error err -> catch_error_f err )
  | Error err -> catch_error_f err

let get_puk_from_hash pk_hash =
  Public_key_hash.of_source pk_hash
  >>= function
  | Ok pkh -> (
    Client_keys.get_key !ctxt pkh
    >>= function
    | Ok (_, src_pk, _) -> Answer.return src_pk
    | Error err -> catch_error_f err)
  | Error err -> catch_error_f err

let get_pukh_from_alias name =
  Public_key_hash.find !ctxt name
  >>= function
  | Ok pkh -> Answer.return pkh
  | Error err -> catch_error_f err

let get_pukh_from_hash pk_hash =
  Public_key_hash.of_source pk_hash
  >>= function
  | Ok pkh -> Answer.return pkh
  | Error err -> catch_error_f err

let string_of_pukh pkh =
  asprintf "%a" Signature.Public_key_hash.pp pkh

let get_contract s =
  ContractAlias.get_contract !ctxt s
  >>= function
  | Ok (_,v) -> Answer.return v
  | Error err -> (
    match Contract.of_b58check s with
    | Ok v -> Answer.return v
    | Error _ as err2 -> catch_error_env_f
                           err2
                           err
                           "B58 check of address failed")

let set_port p =
  (current_config.port) := p;
  ctxt := make_context ()

let set_basedir path =
  (current_config.basedir) := path;
  ctxt := make_context ()

let set_debugmode flag =
  (current_config.debug_mode) := flag;
  ()

let set_fee_parameters
      ?(min_fee = fee_config_def.minimal_fees)
      ?(min_ntz_byte = fee_config_def.minimal_nanotez_per_byte)
      ?(min_ntz_gas = fee_config_def.minimal_nanotez_per_gas_unit)
      ?(force_low = fee_config_def.force_low_fee)
      ?(fee_cap = fee_config_def.fee_cap)
      ?(burn_cap = fee_config_def.burn_cap)
      ()
  = (current_fee_config.minimal_fees) := min_fee;
    (current_fee_config.minimal_nanotez_per_byte) := min_ntz_byte;
    (current_fee_config.minimal_nanotez_per_gas_unit) := min_ntz_gas;
    (current_fee_config.force_low_fee) := force_low;
    (current_fee_config.fee_cap) := fee_cap;
    (current_fee_config.burn_cap) := burn_cap;
    fee_parameter := make_fee_parameter ()

let setup_remote_signer =
  Client_keys.register_signer (module Tezos_signer_backends.Unencrypted)

let transfer amount src destination fee =
  setup_remote_signer;
  Client_keys.get_key !ctxt src
  >>= function
  | Error err -> catch_error_f err
  | Ok (_, src_pk, src_sk) ->
     begin
       let ctxt_proto = new wrap_full !ctxt in
       Lwt.catch
         (fun () ->
           Client_proto_context.transfer
             ctxt_proto
             ~chain:!ctxt#chain
             ~block:!ctxt#block
             ?confirmations:!ctxt#confirmations
             ~dry_run:false
             ~verbose_signing:false
             ~source:src
             ~fee
             ~src_pk
             ~src_sk
             ~destination
             ~amount
             ~fee_parameter: !fee_parameter
             ())
         exception_handler
       >>= fun res ->
       match res with
       | Ok ((oph,_,_),_) -> Answer.return oph
       | Error err -> catch_error_f err
     end

(* entrypoint int arg=5  *)
let call_contract amount src destination ?entrypoint ?arg fee =
  let open Answer in
  (match Contract.is_implicit destination with
   | None -> Answer.return ()
   | Some _ -> Answer.fail Not_callable )
  >>=? fun () ->
  Client_keys.get_key !ctxt src
  >>= function
  | Error err -> catch_error_f err
  | Ok (_, src_pk, src_sk) ->
     begin
     (* here should check the type and change the arg to string *)
       let ctxt_proto = new wrap_full !ctxt in
       Lwt.catch
         (fun () ->
           Client_proto_context.transfer
             ctxt_proto
             ~chain:!ctxt#chain
             ~block:!ctxt#block
             ?confirmations:!ctxt#confirmations
             ~dry_run:false
             ~verbose_signing:false
             ~source:src
             ~fee
             ~src_pk
             ~src_sk
             ~destination
             ?entrypoint
             ?arg
             ~amount
             ~fee_parameter: !fee_parameter
             ())
         exception_handler
       >>= fun res ->
       match res with
       | Ok ((oph,_,_),_) -> Answer.return oph
       | Error err -> catch_error_f err
     end

let get_result ((op, res) : 'kind contents_list * 'kind contents_result_list) (b,i,j) =
  let rec cr : type kind. kind contents_and_result_list -> status Answer.t =
    function
    | Single_and_result (Manager_operation _,
                         Manager_operation_result {operation_result;_}) ->
       begin
         let open Tezos_protocol_009_PsFLoren.Protocol.Contract_storage in
         match operation_result with
           | Failed (_, errs) -> (
             match Environment.wrap_tztrace errs with
             | ( Environment.Ecoproto_error (Non_existing_contract _)) ::_ ->
                Answer.return (Rejected (Reason Invalid_receiver))
             | err :: _ ->
                let err_str = asprintf "%a" Error_monad.pp err in
                Answer.return (Rejected (Unknown_reason err_str))
             | _ -> Answer.return (Rejected (Unknown_reason "Empty trace")))
           | Applied (Transaction_result r) ->
              begin
                let consumed = Int.of_string
                               @@ asprintf "%a" Gas.Arith.pp r.consumed_gas
                in
                let res : op_result = {
                    block_hash = b;
                    rpc_position = (i,j);
                    balance_updates = r.balance_updates;
                    consumed_gas = consumed;
                    storage = r.storage;
                    originated_contracts = r.originated_contracts;
                    storage_size = Z.to_int r.storage_size;
                    paid_storage_size_diff = Z.to_int r.paid_storage_size_diff;
                    big_map_diff = r.lazy_storage_diff;
                    allocated_destination_contract = r.allocated_destination_contract
                  } in
                Answer.return (Accepted res)
              end
           | Backtracked ((Transaction_result _), _) -> Answer.return (Rejected Backtracked)
           | Skipped _ -> Answer.return (Rejected Skipped)
           | _ -> Answer.fail Unexpected_result
       end
    | Cons_and_result ((Manager_operation {operation;_} as op), (Manager_operation_result _ as res), rest) ->
       begin
         match operation with
         | Transaction _ -> cr @@ Single_and_result (op, res)
         | _ -> cr rest
       end
    | _ -> Answer.fail Unexpected_result
  in
  let contents_result_l = Apply_results.pack_contents_list op res
  in
  cr contents_result_l

let check_mempool oph =
  Alpha_block_services.Mempool.pending_operations
       !ctxt
       ~chain: !ctxt#chain
       ()
  >>= function
  | Ok {applied; unprocessed; refused; branch_refused; branch_delayed} ->
     begin
       let rec find_op_subpool = function
         | [] -> false
         | (oph_, _)::ops -> if Operation_hash.equal oph_ oph then true else find_op_subpool ops
       in
       let rec find_op = function
         | [] -> Missing
         | (pool, status) :: pools -> if find_op_subpool pool then status else find_op pools
       in
       let f = (fun (a, (b, _)) -> (a,b)) in
       let unproc_ops = Operation_hash.Map.bindings unprocessed in
       let refused_ops = List.map ~f (Operation_hash.Map.bindings refused)
       in
       let branch_refused_ops = List.map ~f (Operation_hash.Map.bindings branch_refused)
       in
       let branch_delayed_ops = List.map ~f (Operation_hash.Map.bindings branch_delayed)
       in
       let pools_pending = applied @ branch_delayed_ops in
       let pools = (pools_pending, Still_pending)
                   :: (refused_ops, Rejected (Unknown_reason "Unknown"))
                   :: (branch_refused_ops, Still_pending)
                   :: (branch_delayed_ops, Still_pending)
                   :: (unproc_ops, Unprocessed)
                   :: [] in
      Answer.return @@ find_op pools
     end
  | Error errs -> catch_error_f errs

let query oph =
  let ctxt_proto = new wrap_full !ctxt in
  Client_confirmations.lookup_operation_in_previous_blocks
    ctxt_proto
    ~chain:ctxt_proto#chain
    ~predecessors:60
    oph
  >>= function
  | Ok None ->
    check_mempool oph
  | Ok (Some (block, i, j)) ->
     begin
      Alpha_block_services.Operations.operation
        ctxt_proto
        ~chain:ctxt_proto#chain
        ~block:(`Hash (block, 0))
        i
        j
      >>= function
      | Ok op -> (
         match (op.receipt, op.protocol_data) with
         | (Some Apply_results.Operation_metadata omd, Operation_data od) ->
            begin
              match Apply_results.kind_equal_list od.contents omd.contents with
              | Some Apply_results.Eq ->
                 get_result (od.contents, omd.contents) (block, i, j)
              | None -> Answer.fail Unexpected_result
            end
         | _ -> Answer.fail Unexpected_result
      )
      | Error err -> catch_error_f err
     end
  | Error err -> catch_error_f err

let get_balance c =
  let ctxt_proto = new wrap_full !ctxt in
  Client_proto_context.get_balance
    ctxt_proto
    ~chain:ctxt_proto#chain
    ~block:ctxt_proto#block
    c
  >>= function
  | Ok amount -> Answer.return amount
  | Error err -> catch_error_f err

let get_contract_code c =
  let ctxt_proto = new wrap_full !ctxt in
  Client_proto_context.get_script
    ctxt_proto
    ~chain:ctxt_proto#chain
    ~block:ctxt_proto#block
    c
  >>= function
  | Ok None -> Answer.fail Not_callable
  | Ok (Some {code; storage = _}) ->
     begin
       match Script_repr.force_decode code with
       | Error _ as err2 ->
          catch_error_env_f err2 [] "Error while decoding contract code"
       | Ok (code, _) ->
          Answer.return @@ Michelson_v1_printer.unparse_toplevel code
     end
  | Error errs -> catch_error_f errs

let parse_script s =
  Lwt.catch
    (fun () ->
      let parsed = Michelson_v1_parser.parse_toplevel s in
      Lwt.return @@ Micheline_parser.no_parsing_error parsed
    )
    exception_handler
  >>= function
  | Ok parsed -> Answer.return parsed
  | Error err -> catch_error_f err

let parse_expression s =
   Lwt.catch
    (fun () ->
      let parsed = Michelson_v1_parser.parse_expression s in
      Lwt.return @@ Micheline_parser.no_parsing_error parsed
    )
    exception_handler
   >>= function
   | Ok parsed -> Answer.return parsed
   | Error err -> catch_error_f err


(* (s : Michelson_v1_parser.parsed) same thing as (program : Script.expr) *)
(* s.expand same thing as ~script:program *)
(* needs the contarct representation of the contract*)
let list_entrypoints (s : Michelson_v1_parser.parsed)  =
  let ctxt_proto = new wrap_full !ctxt in
  Michelson_v1_entrypoints.list_entrypoints
    ctxt_proto
    ~chain:Client_config.default_chain
    ~block:Client_config.default_block
    s.expanded
  >>= function
  | Ok eps -> Answer.return eps
  | Error err -> catch_error_f err


(* get_balance in Cleint_proto_context takes as parameter a contract 
so I need to have the type contract from the alias string and then call the function get_balance on it *)
let get_balance1 s = 
  (* takes cleint_context.wallet "ctxt" and a string s and 
  returns (string"we don't care about it now and we will not use it"*contract.t "cont") *)
  ContractAlias.get_contract !ctxt s 
  >>= function 
  | Ok (_,cont) -> (
      (* takes 1- prortocol_client_context.rpc_context "we make this from wrap_full of ctxt",
      2- shell_service.chain 
      3- shell_service.block 
      4- contract.t "cont"
      returns tez.t "tamount" *)
      let ctxt_rpc = new wrap_full !ctxt in 
      Client_proto_context.get_balance
        ctxt_rpc
        ~chain:ctxt_rpc#chain
        ~block:ctxt_rpc#block
        cont
      >>=function
      | Ok tamount -> Answer.return tamount
      | Error err -> catch_error_f err
     )
  | Error err -> catch_error_f err

let get_float tz = 
  Stdlib.print_endline (Alpha_context.Tez.to_string tz)

let get_entrypoints s =
  (* s is the string name of the contract, get_contract returns contract.t of s *)
  ContractAlias.get_contract !ctxt s 
  >>= function
  | Ok (_,cont) -> 
    (
      (* get_script :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.t option tzresult Lwt.t *)
    let ctxt_rpc = new wrap_full !ctxt in 
    Client_proto_context.get_script 
      ctxt_rpc
      ~chain:ctxt_rpc#chain
      ~block:ctxt_rpc#block
      cont
      >>=function 
      | Ok None -> Answer.fail Not_callable
      | Ok (Some {code; storage = _}) ->
        (
          match Script_repr.force_decode code with
          | Error _ as err2 ->
            catch_error_env_f err2 [] "Error while decoding contract code"
          | Ok (code1, _) -> 
            (
              let ctxt_proto = new wrap_full !ctxt in
              Michelson_v1_entrypoints.list_entrypoints
                ctxt_proto
                ~chain:ctxt_proto#chain
                ~block:ctxt_proto#block
                code1
                >>=function
                | Ok listofentrypoints -> Answer.return listofentrypoints
                | Error err -> catch_error_f err
            )      
        )
      | Error errs -> catch_error_f errs
    )
  | Error err -> catch_error_f err



let rec print_elements mylist =
  match mylist with
  [] -> Stdlib.print_endline " "
  | (entrypoint,_)::l -> Stdlib.print_endline entrypoint;print_elements l


(* let print_entrypoints entrylist =
  let cctxt = new wrap_full !ctxt in 
  Michelson_v1_entrypoints.print_entrypoints_list cctxt ~emacs:false entrylist *)


let string_of_expression expression =
  let string_of_node = function
    |Int (_, _) ->
        asprintf "T_int"
    |String (_, _) ->
        asprintf "T_string"
    |Bytes (_, _) ->
        asprintf
          "T_bytes"
    |Prim (_, prim, _, _) ->
        asprintf
          "%s"
          (Michelson_v1_printer.ocaml_constructor_of_prim prim)
    |Seq (_, _) ->
        asprintf
          "Seq"
  in
  string_of_node (Micheline.root expression)



(* check_type is for my 1st appraoch, will use check_type2 for the 2nd approach *)
let check_type entrypointname contr arg =
  let ctxt_rpc = new wrap_full !ctxt in 
  Michelson_v1_entrypoints.contract_entrypoint_type 
    ctxt_rpc
    ~chain:ctxt_rpc#chain
    ~block:ctxt_rpc#block
    ~contract:contr
    ~entrypoint:entrypointname
    >>=function
    | Ok (None) -> ctxt_rpc#error
                   "Cannot find a %%do or %%set_delegate entrypoint in \
                    contract@."
    | Ok Some entrytype -> (
      (* let stringty = (Michelson_v1_primitives.strings_of_prims entrytype) in *)
        let stringty = string_of_expression entrytype in
        (
          let argty = mtype_to_string arg in 
          (
            (* Answer.return true *)
            (* Stdlib.print_endline stringty *)
            Stdlib.print_endline stringty;
            if Int64.of_int (String.compare stringty argty) = Int64.zero
            then Answer.return "true"
            else Answer.return "false"
          )
        )
    )
    | Error err -> catch_error_f err
    (* Answer.return listofentrypoints *)



let get_expr_from_lexpr lexpr = 
  let ctxt_rpc = new wrap_full !ctxt in (
  match Script_repr.force_decode lexpr with
  | Ok (expr,_) -> Answer.return expr 
  | Error err -> ctxt_rpc#error "%a" Environment.Error_monad.pp_trace err
  )

(* let get_arg_type ?arg () = 
  Client_proto_context.parse_arg_transfer arg 
  >>=? function lexpr ->
  (
    Script_repr.force_decode lexpr  >>? fun (expr, _) -> expr
  ) *)

(* let parse_expression arg =
  Lwt.return
    (Micheline_parser.no_parsing_error
       (Michelson_v1_parser.parse_expression arg)) *)

let parse_arg_transfer arg =
  ( match arg with
  | Some arg ->
      parse_expression arg >>=? fun {expanded = arg; _} -> Answer.return arg
  | None -> Answer.fail (Unknown "no arg")
  )

(* try1 is to get the type of arg *)
let try1 ?arg () =
  parse_arg_transfer arg >>=? fun expr ->(
    Answer.return (string_of_expression expr)
    (* Answer.return (Michelson_v1_printer.micheline_string_of_expression ~zero_loc:false expr) *)
  )



(* val check_type2: string -> contract -> ?arg:string -> unit -> string Answer.t *)
let check_type2 entrypointname contr ?arg ()=
  let ctxt_rpc = new wrap_full !ctxt in 
  Michelson_v1_entrypoints.contract_entrypoint_type 
    ctxt_rpc
    ~chain:ctxt_rpc#chain
    ~block:ctxt_rpc#block
    ~contract:contr
    ~entrypoint:entrypointname
    >>=function
    | Ok (None) -> ctxt_rpc#error
                   "Cannot find a %%do or %%set_delegate entrypoint in \
                    contract@."
    | Ok Some entrytype -> (
      (* let stringty = (Michelson_v1_primitives.strings_of_prims entrytype) in *)
        let stringty = string_of_expression entrytype in
        (
          try1 ?arg () >>=? fun argty ->
          (* let argty = try1 ?arg () in  *)
          (
            (* Answer.return true *)
            (* Stdlib.print_endline stringty *)
            Stdlib.print_endline stringty;
            Stdlib.print_endline argty;
            if Int64.of_int (String.compare stringty argty) = Int64.zero
            then Answer.return "true"
            else Answer.return "false"
          )
        )
    )
    | Error err -> catch_error_f err
    (* Answer.return listofentrypoints *)

let get_entry ?entrypoint () =
  match entrypoint with 
  | Some x -> x
  | None -> "default"

(* entrypoint int arg=5  *)
let call_contract2 amount src destination ?entrypoint ?arg fee =
  let open Answer in
  (match Contract.is_implicit destination with
   | None -> Answer.return ()
   | Some _ -> Answer.fail Not_callable )
  >>=? fun () ->
  Client_keys.get_key !ctxt src
  >>= function
  | Error err -> catch_error_f err
  | Ok (_, src_pk, src_sk) ->
     begin
     (* here should check the type and change the arg to string *)
     let entryp = get_entry ?entrypoint:entrypoint () in 
     (
       check_type2 entryp destination ?arg:arg () >>= function 
        | Ok out -> 
        (
          if Int64.of_int (String.compare out "true") = Int64.zero 
          then 
          (
            let ctxt_proto = new wrap_full !ctxt in
            Lwt.catch
              (fun () ->
                Client_proto_context.transfer
                  ctxt_proto
                  ~chain:!ctxt#chain
                  ~block:!ctxt#block
                  ?confirmations:!ctxt#confirmations
                  ~dry_run:true
                  ~verbose_signing:false
                  ~source:src
                  ~fee
                  ~src_pk
                  ~src_sk
                  ~destination
                  ?entrypoint
                  ?arg
                  ~amount
                  ~fee_parameter: !fee_parameter
                  ())
                exception_handler
              >>= fun res ->
              match res with
              | Ok ((oph,_,_),_) -> Answer.return oph
              | Error err -> catch_error_f err
          )
          else
          (
            Answer.fail (Unknown "types of the entrypoint and arg do not match")
          )
        )
        | Error _ -> Answer.fail (Unknown "Cannot find entry point")
       
     )
     end

let get_mtype_option ?param () =
  match param with 
  | None -> (Tstring "")
  | Some x -> x

let call_contract3 amount src destination ?entrypoint ?arg fee =
  let open Answer in
  (match Contract.is_implicit destination with
   | None -> Answer.return ()
   | Some _ -> Answer.fail Not_callable )
  >>=? fun () ->
  Client_keys.get_key !ctxt src
  >>= function
  | Error err -> catch_error_f err
  | Ok (_, src_pk, src_sk) ->
     begin
     (* here should check the type and change the arg to string *)
     let get_stringoption_of_mtype = function 
     | (Tstring "") -> None
     | x -> (Some (value_to_string x))
     in
     let argstringoption = get_stringoption_of_mtype (get_mtype_option ?param:arg ()) in 
     let entryp = get_entry ?entrypoint:entrypoint () in 
     (
       check_type2 entryp destination ?arg:(argstringoption) () >>= function 
        | Ok out -> 
        (
          if Int64.of_int (String.compare out "true") = Int64.zero 
          then 
          (
            let ctxt_proto = new wrap_full !ctxt in
            Lwt.catch
              (fun () ->
                Client_proto_context.transfer
                  ctxt_proto
                  ~chain:!ctxt#chain
                  ~block:!ctxt#block
                  ?confirmations:!ctxt#confirmations
                  ~dry_run:true
                  ~verbose_signing:false
                  ~source:src
                  ~fee
                  ~src_pk
                  ~src_sk
                  ~destination
                  ?entrypoint
                  ?arg:argstringoption
                  ~amount
                  ~fee_parameter: !fee_parameter
                  ())
                exception_handler
              >>= fun res ->
              match res with
              | Ok ((oph,_,_),_) -> Answer.return oph
              | Error err -> catch_error_f err
          )
          else
          (
            Answer.fail (Unknown "types of the entrypoint and arg do not match")
          )
        )
        | Error _ -> Answer.fail (Unknown "Cannot find entry point")
       
     )
     end





let micheline_string_of_expression expression =
  (* let string_of_list : string list -> string =
   fun xs -> String.concat ?sep:(Some "; ") xs |> asprintf "[%s]"
  in *)
  let storage_type_to_string = function
    [] -> "T_unit"
    | node::_ ->(
      match node with
      |Int (_, _) ->
        asprintf "T_int"
      |String (_, _) ->
          asprintf "T_string"
      |Bytes (_, _) ->
          asprintf
            "T_bytes"
      |Prim (_, prim, _, _) ->
          asprintf
            "%s"
            (Michelson_v1_printer.ocaml_constructor_of_prim prim)
      |Seq (_, _) ->
          asprintf
            "Seq"
    )
  in
  let rec search_for_storage = function
  [] -> asprintf "T_unit" 
  | node::nodes -> (
    match node with 
    | Int (_, _) ->
        asprintf "%s" (search_for_storage nodes)
    | String (_, _) ->
        asprintf "%s" (search_for_storage nodes)
    | Bytes (_, _) ->
        asprintf "%s" (search_for_storage nodes)
    | Prim (_, prim, nodes_of_storage, _) ->
      if Int64.of_int (String.compare "K_storage" (Michelson_v1_printer.ocaml_constructor_of_prim prim)) = Int64.zero
      then 
      (
        (* asprintf "%s" (Michelson_v1_printer.ocaml_constructor_of_prim prim) *)
        asprintf "%s" (storage_type_to_string nodes_of_storage)
      )
      else
      (
        asprintf "%s" (search_for_storage nodes)
      )      
    | Seq (_, _) ->
        asprintf "%s" (search_for_storage nodes)
  )
  in
  let string_of_node = function
    | Int (_, _) ->
        asprintf "T_int"
    | String (_, _) ->
        asprintf "T_string"
    | Bytes (_, _) ->
        asprintf
          "T_bytes"
    | Prim (_, prim, _, _) ->
        asprintf
          "%s"
          (Michelson_v1_printer.ocaml_constructor_of_prim prim)
          (* (string_of_list @@ List.map string_of_node nodes) *)
    | Seq (_, nodes) ->
        asprintf
          "%s"
          (search_for_storage nodes)
    (* | Seq (_, (_::[])) | Seq (_, [])->
        asprintf
          "T_unit" *)
  in
  string_of_node (root expression)


let print_code s =
  parse_script s  >>=? fun parsed ->
  (* Answer.return parsed *)
  (* Answer.return parsed.source *)
  (* Answer.return (Michelson_v1_printer.micheline_string_of_expression ~zero_loc:false parsed.expanded) *)
  Answer.return (micheline_string_of_expression parsed.expanded)


let check_storage_type initial_storage code =
  print_code code >>=? fun storage_type  ->
  (
    try1 ?arg:(Some initial_storage) ()>>=? fun initial_type ->
    (
      if Int64.of_int (String.compare storage_type initial_type) = Int64.zero
      then 
      (
        (* asprintf "%s" (Michelson_v1_printer.ocaml_constructor_of_prim prim) *)
        Answer.return "true"
      )
      else
      (
        Answer.return "false"
      )   
    )
  )   

(* val originate: string -> Tez_t.t -> pukh -> string ->
  (Kind.origination Kind.manager Injection.result * Contract.t) Answer.t *)
let originate initial_storage balance fee src contractstring =
  Client_keys.get_key !ctxt src
  >>= function
  | Error err -> catch_error_f err
  | Ok (_, src_pk, src_sk) ->
     begin
      let ctxt_proto = new wrap_full !ctxt in
      (
        parse_script contractstring  >>=? fun parsed ->
        (
          check_storage_type initial_storage contractstring >>=? fun out ->
          (
            if Int64.of_int (String.compare out "true") = Int64.zero
            then 
            (
            set_fee_parameters ?burn_cap:(Some fee)();
            Client_proto_context.originate_contract
              ctxt_proto
              ~chain:!ctxt#chain
              ~block:!ctxt#block
              (* ?dry_run:(Some true) *)
              (* ?fee:(Some fee) *)
              (* ?branch
              ?confirmations
              ?fee
              ?gas_limit
              ?storage_limit
              ?verbose_signing *)
              ~delegate: None
              ~initial_storage:initial_storage
              ~balance:balance
              ~source:src
              ~src_pk:src_pk
              ~src_sk:src_sk
              ~code:parsed.expanded
              ~fee_parameter:!fee_parameter
              () >>= fun res ->
                    match res with
                    | Ok ans -> Answer.return ans
                    | Error err -> catch_error_f err
            )
            else
            (
              Answer.fail (Unknown "types of the storage and initial storage do not match")
            )
          )
        )
      )
     end


let originate2 initial_storage balance fee src contractstring =
  Client_keys.get_key !ctxt src
  >>= function
  | Error err -> catch_error_f err
  | Ok (_, src_pk, src_sk) ->
     begin
      let ctxt_proto = new wrap_full !ctxt in
      (
        parse_script contractstring  >>=? fun parsed ->
        (
          let string_of_initial_storage = value_to_string initial_storage in 
          check_storage_type string_of_initial_storage contractstring >>=? fun out ->
          (
            if Int64.of_int (String.compare out "true") = Int64.zero
            then 
            (
            set_fee_parameters ?burn_cap:(Some fee)();
            Client_proto_context.originate_contract
              ctxt_proto
              ~chain:!ctxt#chain
              ~block:!ctxt#block
              (* ?dry_run:(Some true) *)
              (* ?fee:(Some fee) *)
              (* ?branch
              ?confirmations
              ?fee
              ?gas_limit
              ?storage_limit
              ?verbose_signing *)
              ~delegate: None
              ~initial_storage:string_of_initial_storage
              ~balance:balance
              ~source:src
              ~src_pk:src_pk
              ~src_sk:src_sk
              ~code:parsed.expanded
              ~fee_parameter:!fee_parameter
              () >>= fun res ->
                    match res with
                    | Ok ans -> Answer.return ans
                    | Error err -> catch_error_f err
            )
            else
            (
              Answer.fail (Unknown "types of the storage and initial storage do not match")
            )
          )
        )
      )
     
     end


