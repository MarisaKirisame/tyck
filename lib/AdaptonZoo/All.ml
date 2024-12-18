(** Lists of all incremental computation modules and applications. *)

open AdaptonUtil

(** List of all incremental computation modules. *)
let a_list =
  [
    (*("Adapton", (module Adapton : Signatures.AType));*)
    ("EagerTotalOrder", (module EagerTotalOrder : Signatures.AType))
    (*("EagerNonInc", (module EagerNonInc : Signatures.AType));*)
    (*("LazyNonInc", (module LazyNonInc : Signatures.AType));*);
  ]
(*
(** List of all incremental list modules. *)
let alist_list =
  List.map
    (fun (name, atype) ->
      ("AList (" ^ name ^ ")", (module AList.Make ((val atype : Signatures.AType)) : Signatures.AListType)))
    a_list

(** List of all incremental array mapped trie modules. *)
let aamt_list =
  List.map
    (fun (name, atype) ->
      ( "AArrayMappedTrie (" ^ name ^ ")",
        (module AArrayMappedTrie.Make ((val atype : Signatures.AType)) : Signatures.AArrayMappedTrieType) ))
    a_list
*)
