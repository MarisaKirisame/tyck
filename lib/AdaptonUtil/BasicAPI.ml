(** Functor that provides a basic polymorphic API for an Adapton module.

    Instead of providing ['a thunk], this provides two types: ['a aref], which are input thunks that can only hold
    values but is updateable by the outer program, and ['a athunk], which can hold computations, but cannot be updated
    by the outer program.

    These should only be used with [int], ['a aref] or ['a athunk], due to the use of conservative hash as well as
    equality functions internally.
*)

module Make (M : Signatures.AType) : sig
  type 'a aref

  val aref : 'a -> 'a aref
  val get : 'a aref -> 'a
  val set : 'a aref -> 'a -> unit

  type 'a athunk

  val force : 'a athunk -> 'a
  val thunk : (unit -> 'a) -> 'a athunk
  val memo : ('fn -> 'arg -> 'a athunk) -> ('arg -> 'a athunk as 'fn)
end = struct
  module P = PolyAPI.Make (M)

  type 'a aref = 'a P.thunk

  let aref x = P.const x
  let get m = P.force m
  let set m x = P.update_const m x

  type 'a athunk = 'a P.thunk

  let force m = P.force m
  let thunk f = P.thunk f
  let memo f = P.memo f
end
