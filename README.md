# Tezos API

This typed OCaml API provides functions to interact with a Tezos smart contract. It uses the Tezos libraries released on ```opam``` and
wraps them inside a more simplified and easy-to-use interface in addition to a typed invoking and originating functions that checks the types before starting the blockchain opertaions. The typed API heavily utilizes the tezos-client library with
its built-in input verification and prevalidation of operations in order to provide instant feedback and thus avoiding injections
of invalid operations.
The source code of the Tezos libraries can be found on [Gitlab](https://gitlab.com/tezos/tezos/).
## Using Tezos_Api
To install the Tezos libraries with opam:  
```opam install tezos```

Once you have the Tezos libraries installed, you can easily build the Tezos_Api into your project using ```dune```. Your project structure should look like this:

```
.  
+-- dune  
+-- dune-project
+-- your_main.ml
+-- ...
+-- lib
|   +-- dune
|   +-- SyncAPIV1.mli
|   +-- SyncAPIV1.ml
|   +-- Api.mli
|   +-- Api.ml
|   +-- Api_context.mli
|   +-- Api_context.ml
|   +-- Api_error.mli
|   +-- Api_error.ml
|   +-- test/
```

Inside your projects root dune file, include the Tezos_api and the following dependencies:
```
(executables
 (names my_exe)
 (libraries SyncAPIV1
 	    lwt
	    lwt.unix
	    unix
            ... )

  (flags (:standard -open Lwt
                     ...
  	 	     -linkall)))
```
Your dune-projects file should contain the following line:
``` (lang dune 1.11) ```

Build your project with  
``` dune build test.exe```
## The Michelson Types that we Included in this typed OCaml API version
```ocaml
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
```
## Code examples
### Example 1: Get to know all of the entrypoints and their types in a smart contract 
```ocaml
  Api.get_entrypoints "id1"
  >>= function 
    ....
```

### Example 2: Invoking a smart contract using the typed arguments for entrypoints 
```ocaml
Api.get_pukh_from_alias "test3"
  >>=? fun pukh ->
  Api.get_contract "id1"
  >>=? fun contr ->
  let amount = Api.Tez_t.tez 10.0 in
  let fees = Api.Tez_t.tez 0.001 in
  Api.call_contract3 amount pukh contr ?entrypoint:(Some "default") ?arg:(Some (Tstring "xxx")) fees
  >>= function
    ....
```

### Example 3: Originating a smart contract with a typed initial_storage
```ocaml
Api.get_pukh_from_alias "test3"
 >>=? fun pukh ->
 let amount = Api.Tez_t.tez 1.0 in
 (* let fees = Api.Tez_t.tez 0.0001 in *)
 let fees = Api.Tez_t.tez 0.07575 in
 let contractcode = 
  "parameter (int); \n\
   storage (int); \n\n\
   code\n\
  \  {\n\
  \    CAR;\n\
  \    PUSH int 1;\n\
  \    ADD;\n\
  \    NIL operation;\n\
  \    PAIR }\n" in 
  Api.originate2 
  (Tint 1) amount fees pukh contractcode
  >>= function 
....
```

### Example 4: Type-checking before starting invoking the smart contract for a string argument 
```ocaml
Api.get_pukh_from_alias "test3"
  >>=? fun pukh ->
  Api.get_contract "id1"
  >>=? fun contr ->
  let amount = Api.Tez_t.tez 10.0 in
  let fees = Api.Tez_t.tez 0.001 in
  Api.call_contract2 amount pukh contr ?entrypoint:(Some "default") ?arg:(Some "\"true\"") fees
  >>= function
  ....
```

### Example 5: Type-checking before starting originating the smart contract for a string initial_storage
```ocaml
Api.get_pukh_from_alias "test3"
 >>=? fun pukh ->
 let amount = Api.Tez_t.tez 1.0 in
 (* let fees = Api.Tez_t.tez 0.0001 in *)
 let fees = Api.Tez_t.tez 0.07575 in
 let contractcode = 
  "parameter (int); \n\
   storage (int); \n\n\
   code\n\
  \  {\n\
  \    CAR;\n\
  \    PUSH int 1;\n\
  \    ADD;\n\
  \    NIL operation;\n\
  \    PAIR }\n" in 
  Api.originate 
  "1" amount fees pukh contractcode
  >>= function 
  ....
```

## Running the quick tests
Inside the ```test/``` directory you can find an executable to quick test all functions of the API. For most of the tests, it needs to establish a connection to a local node and needs the following setup:
- you need to have an implicit account and a contract and adapt them in the test file accordingly, for some functions like the origination function you can keep the dummy contract as it is or you can adapt the file with one of your choice. 
- you should also adapt the basedir to your tezos_client directory location.
- the port is set to default which is 8732 so you need to adapt it if you will change the default port of the node. 
Build the test file within the ```test/``` directory:
```dune build ./test.exe```
Run the tests:
```dune exec ./test.exe```
