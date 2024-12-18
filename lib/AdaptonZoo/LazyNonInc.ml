(*(** Lazy non-incremental computation. *)

  open AdaptonInternal
  open AdaptonUtil

  (** Types and operations common to LazyNonInc thunks containing any type. *)
  module T = struct
    type atype
    (** Abstract type identifying this module. *)

    type 'a thunk = { id : int; thunk : 'a Lazy.t }
    (** LazyNonInc thunks containing ['a]. *)

    (** This module implements non-incremental thunks. *)
    let is_incremental = false

    (** This module implements lazy values. *)
    let is_lazy = true

    (**/**) (* internal state *)

    let lazy_id_counter = Types.Counter.make 0

    (**/**)

    (** Return the id of a LazyNonInc thunk. *)
    let id m = m.id

    (** Compute the hash value of a LazyNonInc thunk. *)
    let hash seed m = Hashtbl.seeded_hash seed m.id

    (** Compute whether two LazyNonInc thunks are equal. *)
    let equal = ( == )

    (** Recompute LazyNonInc thunks if necessary (not supported by this module; raises {!NonIncremental}). *)
    let refresh () = raise Exceptions.NonIncremental

    (** Return the value contained by a LazyNonInc thunk, computing it if necessary. *)
    let force { thunk = (lazy value); _ } = value
  end

  include T

  (** Functor to make constructors for LazyNonInc thunks of a specific type. *)
  module Make (R : Hashtbl.SeededHashedType) :
    Signatures.AType.S with type atype = atype and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk =
  struct
    include T

    type data = R.t
    (** Value contained by LazyNonInc thunks for a specific type. *)

    type t = R.t thunk
    (** LazyNonInc thunks for a specific type. *)

    module Data = R
    (** Module representing type [data]. *)

    (** Create a LazyNonInc thunk from a constant value. *)
    let const x = { id = Types.Counter.next lazy_id_counter; thunk = lazy x }

    (** Update a LazyNonInc thunk with a constant value (not supported by this module; raises {!NonIncremental}). *)
    let update_const _ _ = raise Exceptions.NonIncremental

    (** Create a LazyNonInc thunk from a function that may depend on other LazyNonInc thunks. *)
    let thunk f =
      {
        id = Types.Counter.next lazy_id_counter;
        thunk =
          lazy
            (incr Statistics.Counts.evaluate;
             f ());
      }

    (** Update a LazyNonInc thunk with a function (not supported by this module; raises {!NonIncremental}). *)
    let update_thunk _ _ = raise Exceptions.NonIncremental

    (* create memoizing constructors *)
    include MemoN.Make (struct
      type data = R.t
      type t = R.t thunk

      (** Create non-memoizing constructor for a LazyNonInc thunk. *)
      let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
        (* non-memoizing constructor *)
        let rec memo x =
          {
            id = Types.Counter.next lazy_id_counter;
            thunk =
              lazy
                (incr Statistics.Counts.evaluate;
                 f memo x);
          }
        in
        memo
    end)
  end

  (** Tweak GC for this module. *)
  let tweak_gc () = ()
*)
