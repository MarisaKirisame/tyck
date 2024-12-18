(** Adapton array mapped tries. *)

open AdaptonInternal

(**/**) (* helper parameters *)

let depth = 7
let bits = LazySparseArray.key_bits
let width = 1 lsl bits
let mask = width - 1
let mask' = lnot mask
let key_bits' = bits * (depth - 1)
let _ = assert (key_bits' < Sys.word_size - 3)

(**/**)

(** Key-width of Adapton array mapped tries. *)
let key_bits = bits * depth

(** Size of Adapton array mapped tries. *)
let size = 1 lsl key_bits

(** Functor to make Adapton array mapped tries, given a particular module for Adapton thunks. *)
module Make (M : Signatures.AType) :
  Signatures.AArrayMappedTrieType with type atype = M.atype and type 'a thunk = 'a M.thunk = struct
  type 'a aamt = 'a aamt' M.thunk
  (** Adapton array mapped tries containing ['a]. *)

  (** Constructor tags for Adapton array mapped tries containing ['a]. *)
  and 'a aamt' = Branches of 'a aamt' LazySparseArray.t | Leaves of 'a LazySparseArray.t | Empty

  (** Types and operations common to Adapton array mapped tries containing any type. *)
  module T = struct
    type atype = M.atype
    (** Abstract type identifying the given module for Adapton thunks used to create this module for Adapton array mapped tries. *)

    type 'a thunk = 'a M.thunk
    (** Adapton thunks from the given module used to create this module for Adapton array mapped tries. *)

    (** True if this module implements Adapton array mapped tries. *)
    let is_incremental = M.is_incremental

    (** True if this module implements lazy array mapped tries. *)
    let is_lazy = M.is_lazy

    (** Compute the hash value of an Adapton array mapped trie. *)
    let hash = M.hash

    (** Compute whether two Adapton array mapped tries are equal. *)
    let equal = M.equal

    (** Recompute Adapton array mapped tries if necessary. *)
    let refresh = M.refresh

    (** Return the value at index [k] of an Adapton array mapped trie. *)
    let get xs k =
      if k < 0 || k >= size then invalid_arg "index out of bounds";
      let rec get xs s =
        let k = (k lsr s) land mask in
        match xs with
        | Branches xs -> ( match LazySparseArray.get xs k with Some xs -> get xs (s - bits) | None -> None)
        | Leaves xs -> LazySparseArray.get xs k
        | Empty -> None
      in
      get (M.force xs) key_bits'
  end

  include T

  module type S = Signatures.AArrayMappedTrieType.S
  (** Output module types of {!AArrayMappedTrie.Make}. *)

  (** Helper functor to make a constructor for Adapton array mapped tries of a specific type. *)
  module Make (R : Hashtbl.SeededHashedType) :
    S with type atype = atype and type 'a thunk = 'a thunk and type data = R.t and type t = R.t aamt = struct
    module A = M.Make (struct
      type t = R.t aamt'

      let hash seed = function
        | Branches xs -> LazySparseArray.hash (Hashtbl.seeded_hash seed `Branches) xs
        | Leaves xs -> LazySparseArray.hash (Hashtbl.seeded_hash seed `Leaves) xs
        | Empty -> Hashtbl.seeded_hash seed `Empty

      let equal xs xs' =
        xs == xs'
        ||
        match (xs, xs') with
        | Branches xs, Branches xs' -> LazySparseArray.equal xs xs'
        | Leaves xs, Leaves xs' -> LazySparseArray.equal xs xs'
        | _ -> false
    end)

    type data = R.t
    (** Value contained by Adapton array mapped tries for a specific type. *)

    type t = A.t
    (** Adapton array mapped tries for a specific type. *)

    include T

    (** An empty Adapton array mapped trie. *)
    let empty = A.const Empty

    (** Create memoizing constructor that adds a binding to an Adapton array mapped trie. *)
    let memo_add =
      let add =
        A.memo3
          (module A)
          (module Types.Int)
          (module R)
          (fun _ xs k v ->
            let rec add xs s =
              (* if along k, initialize the next branch/leaf node, else lookup the subtrie under the prior AMT *)
              if s > 0 then
                Branches
                  (LazySparseArray.make (fun d ->
                       if (k lsr s) land mask == d then Some (add xs (s - bits))
                       else
                         (* perform a partial key lookup for the corresponding subtrie under the prior AMT *)
                         let rec subtrie xs s' =
                           match xs with
                           | Branches xs -> (
                               if s' == s then LazySparseArray.get xs d
                               else
                                 let k = (k lsr s') land mask in
                                 match LazySparseArray.get xs k with Some xs -> subtrie xs (s' - bits) | None -> None)
                           | Empty -> None
                           | Leaves _ -> assert false
                         in
                         subtrie (M.force xs) key_bits'))
              else
                Leaves
                  (LazySparseArray.make (fun d -> if k land mask == d then Some v else get xs (k land mask' lor d)))
            in
            add xs key_bits')
      in
      fun xs k v ->
        if k < 0 || k >= size then invalid_arg "index out of bounds";
        add xs k v
  end
end
