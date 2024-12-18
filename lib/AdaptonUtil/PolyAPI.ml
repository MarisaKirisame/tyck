(** Functor that provides a polymorphic API for an Adapton module.

    Note that thunk constructors will provide conservative hash as well as equality functions by default (to compare
    thunk values as well as memoization keys). These functions are too conservative for types other than [int] and
    ['a thunk], so custom hash and equality functions should be provided for most types.
*)

(*open AdaptonInternal*)

module type S = sig
  type 'a thunk

  val is_incremental : bool
  val is_lazy : bool
  val id : 'a thunk -> int
  val hash : 'a thunk -> int
  val equal : 'a thunk -> 'a thunk -> bool
  val refresh : unit -> unit
  val force : 'a thunk -> 'a
  val const : ?hash:(int -> 'a -> int) -> ?equal:('a -> 'a -> bool) -> 'a -> 'a thunk
  val update_const : 'a thunk -> 'a -> unit
  val thunk : ?hash:(int -> 'a -> int) -> ?equal:('a -> 'a -> bool) -> (unit -> 'a) -> 'a thunk
  val update_thunk : 'a thunk -> (unit -> 'a) -> unit
  val return : ?hash:(int -> 'a -> int) -> ?equal:('a -> 'a -> bool) -> 'a -> 'a thunk
  val map : ?hash:(int -> 'b -> int) -> ?equal:('b -> 'b -> bool) -> ('a -> 'b) -> 'a thunk -> 'b thunk

  val map2 :
    ?hash:(int -> 'c -> int) -> ?equal:('c -> 'c -> bool) -> ('a -> 'b -> 'c) -> 'a thunk -> 'b thunk -> 'c thunk

  val join : ?hash:(int -> 'a -> int) -> ?equal:('a -> 'a -> bool) -> 'a thunk thunk -> 'a thunk
  val bind : ?hash:(int -> 'b -> int) -> ?equal:('b -> 'b -> bool) -> 'a thunk -> ('a -> 'b thunk) -> 'b thunk

  val memo :
    ?inp_hash:(int -> 'inp -> int) ->
    ?inp_equal:('inp -> 'inp -> bool) ->
    ?hash:(int -> 'a -> int) ->
    ?equal:('a -> 'a -> bool) ->
    ('memo -> 'inp -> 'a thunk) ->
    ('inp -> 'a thunk as 'memo)

  val memo2 :
    ?inp1_hash:(int -> 'inp1 -> int) ->
    ?inp1_equal:('inp1 -> 'inp1 -> bool) ->
    ?inp2_hash:(int -> 'inp2 -> int) ->
    ?inp2_equal:('inp2 -> 'inp2 -> bool) ->
    ?hash:(int -> 'a -> int) ->
    ?equal:('a -> 'a -> bool) ->
    ('memo -> 'inp1 -> 'inp2 -> 'a thunk) ->
    ('inp1 -> 'inp2 -> 'a thunk as 'memo)

  val memo3 :
    ?inp1_hash:(int -> 'inp1 -> int) ->
    ?inp1_equal:('inp1 -> 'inp1 -> bool) ->
    ?inp2_hash:(int -> 'inp2 -> int) ->
    ?inp2_equal:('inp2 -> 'inp2 -> bool) ->
    ?inp3_hash:(int -> 'inp3 -> int) ->
    ?inp3_equal:('inp3 -> 'inp3 -> bool) ->
    ?hash:(int -> 'a -> int) ->
    ?equal:('a -> 'a -> bool) ->
    ('memo -> 'inp1 -> 'inp2 -> 'inp3 -> 'a thunk) ->
    ('inp1 -> 'inp2 -> 'inp3 -> 'a thunk as 'memo)

  val memo4 :
    ?inp1_hash:(int -> 'inp1 -> int) ->
    ?inp1_equal:('inp1 -> 'inp1 -> bool) ->
    ?inp2_hash:(int -> 'inp2 -> int) ->
    ?inp2_equal:('inp2 -> 'inp2 -> bool) ->
    ?inp3_hash:(int -> 'inp3 -> int) ->
    ?inp3_equal:('inp3 -> 'inp3 -> bool) ->
    ?inp4_hash:(int -> 'inp4 -> int) ->
    ?inp4_equal:('inp4 -> 'inp4 -> bool) ->
    ?hash:(int -> 'a -> int) ->
    ?equal:('a -> 'a -> bool) ->
    ('memo -> 'inp1 -> 'inp2 -> 'inp3 -> 'inp4 -> 'a thunk) ->
    ('inp1 -> 'inp2 -> 'inp3 -> 'inp4 -> 'a thunk as 'memo)

  val tweak_gc : unit -> unit
end

module Make (M : Signatures.AType) = struct
  type 'a thunk =
    'a M.thunk * (module Signatures.AType.S with type atype = M.atype and type data = 'a and type t = 'a M.thunk)

  let is_incremental = M.is_incremental
  let is_lazy = M.is_lazy
  let id (type a) ((m, (module S)) : a thunk) = S.id m
  let hash m = Hashtbl.hash (id m)
  let equal (type a) (m : a thunk) (m' : a thunk) = id m = id m'
  let refresh = M.refresh
  let force (type a) ((m, (module S)) : a thunk) = S.force m

  let default_hash seed x =
    (* the various thunk types are carefully laid out such that the following will hash the thunk ID only *)
    Hashtbl.seeded_hash_param 1 100 seed x

  let default_equal = ( == )

  let const (type a) ?(hash = default_hash) ?(equal = default_equal) : a -> a thunk =
    let module S = M.Make (struct
      type t = a

      let hash = hash
      let equal = equal
    end) in
    fun x -> (S.const x, (module S))

  let update_const (type a) ((m, (module S)) : a thunk) x = S.update_const m x

  let thunk (type a) ?(hash = default_hash) ?(equal = default_equal) : (unit -> a) -> a thunk =
    let module S = M.Make (struct
      type t = a

      let hash = hash
      let equal = equal
    end) in
    fun f -> (S.thunk f, (module S))

  let update_thunk (type a) ((m, (module S)) : a thunk) f = S.update_thunk m f
  let return ?(hash = default_hash) ?(equal = default_equal) : 'a -> 'a thunk = const ~hash ~equal

  let map ?(hash = default_hash) ?(equal = default_equal) (f : 'a -> 'b) (x : 'a thunk) : 'b thunk =
    thunk ~hash ~equal (fun _ -> f (force x))

  let map2 ?(hash = default_hash) ?(equal = default_equal) (f : 'a -> 'b -> 'c) (x : 'a thunk) (y : 'b thunk) : 'b thunk
      =
    thunk ~hash ~equal (fun _ -> f (force x) (force y))

  let join ?(hash = default_hash) ?(equal = default_equal) (x : 'a thunk thunk) : 'a thunk =
    thunk ~hash ~equal (fun _ -> force (force x))

  let bind ?(hash = default_hash) ?(equal = default_equal) (x : 'a thunk) (f : 'a -> 'b thunk) : 'b thunk =
    thunk ~hash ~equal (fun _ -> force (f (force x)))

  let memo (type inp a) ?(inp_hash = default_hash) ?(inp_equal = default_equal) ?(hash = default_hash)
      ?(equal = default_equal) : ('memo -> inp -> a thunk) -> (inp -> a thunk as 'memo) =
    let module S = M.Make (struct
      type t = a

      let hash = hash
      let equal = equal
    end) in
    fun f ->
      let f memo a = fst (f (fun a -> (memo a, (module S))) a) in
      let memo =
        S.memo
          (module struct
            type t = inp

            let hash = inp_hash
            let equal = inp_equal
          end)
          f
      in

      fun a -> (memo a, (module S))

  let memo2 (type inp1 inp2 a) ?(inp1_hash = default_hash) ?(inp1_equal = default_equal) ?(inp2_hash = default_hash)
      ?(inp2_equal = default_equal) ?(hash = default_hash) ?(equal = default_equal) :
      ('memo2 -> inp1 -> inp2 -> a thunk) -> (inp1 -> inp2 -> a thunk as 'memo2) =
    let module S = M.Make (struct
      type t = a

      let hash = hash
      let equal = equal
    end) in
    fun f ->
      let f memo2 a b = fst (f (fun a b -> (memo2 a b, (module S))) a b) in
      let memo2 =
        S.memo2
          (module struct
            type t = inp1

            let hash = inp1_hash
            let equal = inp1_equal
          end)
          (module struct
            type t = inp2

            let hash = inp2_hash
            let equal = inp2_equal
          end)
          f
      in
      fun a b -> (memo2 a b, (module S))

  let memo3 (type inp1 inp2 inp3 a) ?(inp1_hash = default_hash) ?(inp1_equal = default_equal)
      ?(inp2_hash = default_hash) ?(inp2_equal = default_equal) ?(inp3_hash = default_hash)
      ?(inp3_equal = default_equal) ?(hash = default_hash) ?(equal = default_equal) :
      ('memo3 -> inp1 -> inp2 -> inp3 -> a thunk) -> (inp1 -> inp2 -> inp3 -> a thunk as 'memo3) =
    let module S = M.Make (struct
      type t = a

      let hash = hash
      let equal = equal
    end) in
    fun f ->
      let f memo3 a b c = fst (f (fun a b c -> (memo3 a b c, (module S))) a b c) in
      let memo3 =
        S.memo3
          (module struct
            type t = inp1

            let hash = inp1_hash
            let equal = inp1_equal
          end)
          (module struct
            type t = inp2

            let hash = inp2_hash
            let equal = inp2_equal
          end)
          (module struct
            type t = inp3

            let hash = inp3_hash
            let equal = inp3_equal
          end)
          f
      in
      fun a b c -> (memo3 a b c, (module S))

  let memo4 (type inp1 inp2 inp3 inp4 a) ?(inp1_hash = default_hash) ?(inp1_equal = default_equal)
      ?(inp2_hash = default_hash) ?(inp2_equal = default_equal) ?(inp3_hash = default_hash)
      ?(inp3_equal = default_equal) ?(inp4_hash = default_hash) ?(inp4_equal = default_equal) ?(hash = default_hash)
      ?(equal = default_equal) :
      ('memo4 -> inp1 -> inp2 -> inp3 -> inp4 -> a thunk) -> (inp1 -> inp2 -> inp3 -> inp4 -> a thunk as 'memo4) =
    let module S = M.Make (struct
      type t = a

      let hash = hash
      let equal = equal
    end) in
    fun f ->
      let f memo4 a b c d = fst (f (fun a b c d : a thunk -> (memo4 a b c d, (module S))) a b c d) in
      let memo4 =
        S.memo4
          (module struct
            type t = inp1

            let hash = inp1_hash
            let equal = inp1_equal
          end)
          (module struct
            type t = inp2

            let hash = inp2_hash
            let equal = inp2_equal
          end)
          (module struct
            type t = inp3

            let hash = inp3_hash
            let equal = inp3_equal
          end)
          (module struct
            type t = inp4

            let hash = inp4_hash
            let equal = inp4_equal
          end)
          f
      in
      fun a b c d -> (memo4 a b c d, (module S))

  let tweak_gc = M.tweak_gc
end
