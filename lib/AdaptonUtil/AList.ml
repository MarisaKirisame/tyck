(** Adapton lists. *)

open AdaptonInternal

(** Functor to make Adapton lists, given a particular module for Adapton thunks. *)
module Make (M : Signatures.AType) :
  Signatures.AListType
    with type atype = M.atype
     and type 'a thunk = 'a M.thunk
     and type 'a alist = [ `Cons of 'a * 'b | `Nil ] M.thunk as 'b = struct
  type 'a alist = 'a alist' M.thunk
  (** Adapton lists containing ['a]. *)

  and 'a alist' = [ `Cons of 'a * 'a alist | `Nil ]
  (** Constructor tags for Adapton lists containing ['a]. *)

  (** Types and operations common to Adapton lists containing any type. *)
  module T = struct
    type atype = M.atype
    (** Abstract type identifying the given module for Adapton thunks used to create this module for Adapton lists. *)

    type 'a thunk = 'a M.thunk
    (** Adapton thunks from the given module used to create this module for Adapton lists. *)

    (** True if this module implements Adapton lists. *)
    let is_incremental = M.is_incremental

    (** True if this module implements lazy lists. *)
    let is_lazy = M.is_lazy

    (** Return the id of an Adapton list. *)
    let id = M.id

    (** Compute the hash value of an Adapton list. *)
    let hash = M.hash

    (** Compute whether two Adapton lists are equal. *)
    let equal = M.equal

    (** Return the tag of an Adapton list, (re-)computing it if necessary. *)
    let force = M.force

    (** Recompute Adapton lists if necessary. *)
    let refresh = M.refresh

    (** Create a regular list from an Adapton list. *)
    let to_list xs =
      let rec to_list acc xs = match force xs with `Cons (x, xs) -> to_list (x :: acc) xs | `Nil -> List.rev acc in
      to_list [] xs

    (** Create a regular list of ids of elements from an Adapton list. *)
    let to_ids xs =
      let rec to_ids acc xs =
        match force xs with `Cons (_, xs) -> to_ids (id xs :: acc) xs | `Nil -> List.rev (id xs :: acc)
      in
      to_ids [] xs

    (** Create a regular list from the first [k] elements of an Adapton list. *)
    let take k xs =
      let rec take k xs acc =
        if k = 0 then List.rev acc
        else match force xs with `Cons (x, xs) -> take (pred k) xs (x :: acc) | `Nil -> List.rev acc
      in
      take k xs []

    (** Return the head of an Adapton list. *)
    let hd xs = match force xs with `Cons (x, _) -> x | `Nil -> failwith "hd"

    (** Return the tail of an Adapton list. *)
    let tl xs = match force xs with `Cons (_, xs) -> xs | `Nil -> failwith "tl"
  end

  include T

  module type BasicS = Signatures.AListType.BasicS
  (** Output module types of {!AList.MakeBasic}. *)

  module type S = Signatures.AListType.S
  (** Output module types of {!AList.Make}. *)

  (** Helper functor to make basic list constructors and combinators for Adapton lists of a specific type. *)
  module MakeBasic (R : Hashtbl.SeededHashedType) :
    BasicS
      with type atype = atype
       and type 'a thunk = 'a thunk
       and type data = R.t
       and type t = R.t alist
       and type t' = R.t alist' = struct
    module L = M.Make (struct
      type t = R.t alist'

      let hash seed = function
        | `Cons (x, xs) -> hash (R.hash (Hashtbl.seeded_hash seed `Cons) x) xs
        | `Nil -> Hashtbl.seeded_hash seed `Nil

      let equal xs xs' =
        xs == xs' || match (xs, xs') with `Cons (h, t), `Cons (h', t') -> R.equal h h' && equal t t' | _ -> false
    end)

    module AData = M.Make (R)
    (** Adapton thunks for a specific type, return by certain list operations. *)

    type data = R.t
    (** Value contained by Adapton lists for a specific type. *)

    type t = L.t
    (** Adapton lists for a specific type. *)

    type t' = L.data
    (** Tags for Adapton lists for a specific type. *)

    include T

    (** Create an Adapton list from a constant list constructor that does not depend on other Adapton thunks. *)
    let const = L.const

    (** Update an Adapton list with a constant list constructor that does not depend on other Adapton thunks. *)
    let update_const = L.update_const

    (** Create an Adapton list from a thunk returning a list constructor that may depend on other Adapton thunks. *)
    let thunk = L.thunk

    (** Update an Adapton list with a thunk returning a list constructor that may depend on other Adapton thunks. *)
    let update_thunk = L.update_thunk

    include MemoN.Make (struct
      type data = L.data
      type t = L.t

      (** Create memoizing constructor of an Adapton list. *)
      let memo = L.memo
    end)

    (** Create an Adapton list from a regular list. *)
    let of_list xs =
      let rec of_list acc = function x :: xs -> of_list (const (`Cons (x, acc))) xs | [] -> acc in
      of_list (const `Nil) (List.rev xs)

    (** Update the head of an Adapton list to push a value in front. *)
    let push x xs =
      match force xs with
      | `Cons (x', xs') -> update_const xs (`Cons (x, const (`Cons (x', xs'))))
      | `Nil -> update_const xs (`Cons (x, const `Nil))

    (** Update the head of an Adapton list to pop a value from the front. *)
    let pop xs =
      match force xs with
      | `Cons (x', xs') ->
          update_const xs (force xs');
          x'
      | `Nil -> failwith "pop"

    (** Update the [k]th element of an Adapton list to insert a value [x]. *)
    let insert k x xs =
      if k < 0 then invalid_arg "insert";
      let rec insert k xs =
        match force xs with
        | `Cons (_, xs) when k > 0 -> insert (k - 1) xs
        | `Nil when k > 0 -> failwith "insert"
        | `Cons _ | `Nil -> push x xs
      in
      insert k xs

    (** Update the [k]th element of an Adapton list to remove a value and return it. *)
    let remove k xs =
      if k < 0 then invalid_arg "remove";
      let rec remove k xs =
        match force xs with
        | `Cons (_, xs) when k > 0 -> remove (k - 1) xs
        | `Cons _ -> pop xs
        | `Nil -> failwith "remove"
      in
      remove k xs

    (** Create memoizing constructor to concatenate two Adapton lists. *)
    let memo_append =
      memo2
        (module L)
        (module L)
        (fun append xs ys -> match force xs with `Cons (x, xs) -> `Cons (x, append xs ys) | `Nil -> force ys)

    (** Create memoizing constructor to filter an Adapton list with a predicate. *)
    let memo_filter f =
      memo
        (module L)
        (fun filter xs ->
          match force xs with `Cons (x, xs) -> if f x then `Cons (x, filter xs) else force (filter xs) | `Nil -> `Nil)

    (** Create memoizing constructor to filter an Adapton list with a predicate and key. *)
    let memo_filter_with_key (type a) (module K : Hashtbl.SeededHashedType with type t = a) f =
      memo2
        (module K)
        (module L)
        (fun filter k xs ->
          match force xs with
          | `Cons (x, xs) -> if f k x then `Cons (x, filter k xs) else force (filter k xs)
          | `Nil -> `Nil)

    (** Create memoizing constructor to simultaneously filter and map an Adapton list with a predicate/mapping function. *)
    let memo_filter_map (type a b)
        (module L : Signatures.AListType.BasicS with type atype = atype and type data = a and type t = b) f =
      memo
        (module L)
        (fun filter xs ->
          match L.force xs with
          | `Cons (x, xs) -> ( match f x with Some y -> `Cons (y, filter xs) | None -> force (filter xs))
          | `Nil -> `Nil)

    (** Create memoizing constructor to map an Adapton list with a mapping function. *)
    let memo_map (type a b)
        (module L : Signatures.AListType.BasicS with type atype = atype and type data = a and type t = b) f =
      memo (module L) (fun map xs -> match L.force xs with `Cons (x, xs) -> `Cons (f x, map xs) | `Nil -> `Nil)

    (** Create memoizing constructor to map an Adapton list with a mapping function and key. *)
    let memo_map_with_key (type a) (module K : Hashtbl.SeededHashedType with type t = a) (type b c)
        (module L : Signatures.AListType.BasicS with type atype = atype and type data = b and type t = c) f =
      memo2
        (module K)
        (module L)
        (fun map k xs -> match L.force xs with `Cons (x, xs) -> `Cons (f k x, map k xs) | `Nil -> `Nil)

    (** Create memoizing constructor to scan (fold over prefixes of) an Adapton list with an scanning function. *)
    let memo_scan (type a b)
        (module L : Signatures.AListType.BasicS with type atype = atype and type data = a and type t = b) f =
      memo2
        (module L)
        (module R)
        (fun scan xs acc ->
          match L.force xs with
          | `Cons (x, xs) ->
              let acc = f x acc in
              `Cons (acc, scan xs acc)
          | `Nil -> `Nil)

    (** Create memoizing constructor to tree-fold an Adapton list with an associative fold function. *)
    let memo_tfold f =
      let fold_pairs =
        L.memo2
          (module Types.Int)
          (module L)
          (fun fold_pairs seed xs ->
            match L.force xs with
            | `Cons (x', xs') as xs'' -> (
                if L.hash seed xs mod 2 == 0 then `Cons (x', fold_pairs seed xs')
                else match L.force xs' with `Cons (y', ys') -> `Cons (f x' y', fold_pairs seed ys') | `Nil -> xs'')
            | `Nil -> `Nil)
      in
      let tfold =
        AData.memo2
          (module Types.Seeds)
          (module L)
          (fun tfold seeds xs ->
            match L.force xs with
            | `Cons (x', xs') -> (
                match L.force xs' with
                | `Cons _ ->
                    let seed, seeds = Types.Seeds.pop seeds in
                    force (tfold seeds (fold_pairs seed xs))
                | `Nil -> x')
            | `Nil -> failwith "tfold")
      in
      let seeds = Types.Seeds.make () in
      fun xs -> tfold seeds xs
  end

  (** Functor to make various list constructors and combinators for Adapton lists of a specific type. *)
  module Make (R : Hashtbl.SeededHashedType) :
    S
      with type atype = atype
       and type 'a thunk = 'a thunk
       and type data = R.t
       and type t = R.t alist
       and type t' = R.t alist' = struct
    module L = MakeBasic (R)
    include L

    (** Create memoizing constructor to quicksort an Adapton list with a comparator. *)
    let memo_quicksort cmp =
      let filter_left = memo_filter_with_key (module R) (fun k x -> cmp x k < 0) in
      let filter_right = memo_filter_with_key (module R) (fun k x -> cmp x k >= 0) in
      let quicksort =
        memo2
          (module L)
          (module L)
          (fun quicksort xs rest ->
            match L.force xs with
            | `Cons (x, xs) ->
                let left = filter_left x xs in
                let right = filter_right x xs in
                L.force (quicksort left (const (`Cons (x, quicksort right rest))))
            | `Nil -> L.force rest)
      in
      fun xs -> quicksort xs (const `Nil)

    (**/**) (* internal type of mergesort *)

    module RunType = MakeBasic (L)

    (**/**)

    (** Create memoizing constructor to mergesort an Adapton list with a comparator. *)
    let memo_mergesort cmp =
      let nil = const `Nil in
      let single = memo (module R) (fun _ x -> `Cons (x, nil)) in
      let lift = RunType.memo_map (module L) single in
      let merge =
        memo2
          (module L)
          (module L)
          (fun merge xs ys ->
            match (force xs, force ys) with
            | `Cons (x', xs'), `Cons (y', ys') ->
                if cmp x' y' < 0 then `Cons (x', merge xs' ys) else `Cons (y', merge xs ys')
            | xs'', `Nil -> xs''
            | `Nil, ys'' -> ys'')
      in
      let mergesort = RunType.memo_tfold merge in
      memo
        (module L)
        (fun _ xs -> match force xs with `Cons _ -> force (RunType.AData.force (mergesort (lift xs))) | `Nil -> `Nil)
  end
end
