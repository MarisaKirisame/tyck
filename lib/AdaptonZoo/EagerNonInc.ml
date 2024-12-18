(** Eager non-incremental computation. *)

open AdaptonInternal
open AdaptonUtil

(** Types and operations common to EagerNonInc thunks containing any type. *)
module T = struct
  type atype
  (** Abstract type identifying this module. *)

  type 'a thunk = { id : int; value : 'a }
  (** EagerNonInc thunks containing ['a]. *)

  (** This module implements non-incremental thunks. *)
  let is_incremental = false

  (** This module implements eager thunks. *)
  let is_lazy = false

  (**/**) (* internal state *)

  let eager_id_counter = Types.Counter.make 0

  (**/**)

  (** Return the id of an EagerNonInc thunk. *)
  let id m = m.id

  (** Compute the hash value of a EagerNonInc thunk. *)
  let hash seed m = Hashtbl.seeded_hash seed m.id

  (** Compute whether two EagerNonInc thunks are equal. *)
  let equal = ( == )

  (** Recompute EagerNonInc thunks if necessary (not supported by this module; raises {!NonIncremental}). *)
  let refresh () = raise Exceptions.NonIncremental

  (** Return the value contained by an EagerNonInc thunk, computing it if necessary. *)
  let force { value; _ } = value
end

include T

(** Functor to make constructors for EagerNonInc thunks of a specific type. *)
module Make (R : Hashtbl.SeededHashedType) :
  Signatures.AType.S with type atype = atype and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk =
struct
  include T

  type data = R.t
  (** Value contained by EagerNonInc thunks for a specific type. *)

  type t = R.t thunk
  (** EagerNonInc thunks for a specific type. *)

  module Data = R
  (** Module representing type [data]. *)

  (** Create an EagerNonInc thunk from a constant value. *)
  let const x = { id = Types.Counter.next eager_id_counter; value = x }

  (** Update an EagerNonInc thunk with a constant value (not supported by this module; raises {!NonIncremental}). *)
  let update_const _ _ = raise Exceptions.NonIncremental

  (** Create an EagerNonInc thunk from a function that may depend on other EagerNonInc thunks. *)
  let thunk f =
    incr Statistics.Counts.evaluate;
    { id = Types.Counter.next eager_id_counter; value = f () }

  (** Update an EagerNonInc thunk with a function (not supported by this module; raises {!NonIncremental}). *)
  let update_thunk _ _ = raise Exceptions.NonIncremental

  (* create memoizing constructors *)
  include MemoN.Make (struct
    type data = R.t
    type t = R.t thunk

    (** Create non-memoizing constructor for an EagerNonInc thunk. *)
    let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
      (* non-memoizing constructor *)
      let rec memo x =
        incr Statistics.Counts.evaluate;
        { id = Types.Counter.next eager_id_counter; value = f memo x }
      in
      memo
  end)
end

(** Tweak GC for this module. *)
let tweak_gc () = ()
