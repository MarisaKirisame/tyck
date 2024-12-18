(** Module types for {i Adapton}. *)

open AdaptonInternal

(** {2 Adapton thunks} *)

(** Output module types of modules for Adapton thunks. *)
module rec AType : sig
  (** Module type for Adapton thunks for a specific type. *)
  module type S = sig
    type atype
    type 'a thunk
    type data
    type t

    module Data : Hashtbl.SeededHashedType with type t = data

    val is_incremental : bool
    val is_lazy : bool
    val id : t -> int
    val hash : int -> t -> int
    val equal : t -> t -> bool
    val refresh : unit -> unit
    val force : t -> data
    val const : data -> t
    val update_const : t -> data -> unit
    val thunk : (unit -> data) -> t
    val update_thunk : t -> (unit -> data) -> unit

    include MemoN.S with type data := data and type t := t
  end
end =
  AType

(** Module type for Adapton thunks. *)
module type AType = sig
  type atype
  type 'a thunk

  val is_incremental : bool
  val is_lazy : bool
  val id : 'a thunk -> int
  val hash : int -> 'a thunk -> int
  val equal : 'a thunk -> 'a thunk -> bool
  val refresh : unit -> unit
  val force : 'a thunk -> 'a

  module Make (R : Hashtbl.SeededHashedType) :
    AType.S with type atype = atype and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk

  val tweak_gc : unit -> unit
end

(** {2 Adapton lists} *)

(** Output module types of modules for Adapton lists. *)
module rec AListType : sig
  (** Module type for Adapton lists for a specific type containing basic types and operations. *)
  module type BasicS = sig
    type atype
    type 'a thunk
    type data

    module AData : AType.S with type atype = atype and type 'a thunk = 'a thunk and type data = data

    type t
    type t' = [ `Cons of data * t | `Nil ]

    val is_incremental : bool
    val is_lazy : bool
    val id : t -> int
    val hash : int -> t -> int
    val equal : t -> t -> bool
    val refresh : unit -> unit
    val force : t -> t'
    val to_list : t -> data list
    val to_ids : t -> int list
    val take : int -> t -> data list
    val hd : t -> data
    val tl : t -> t
    val const : t' -> t
    val update_const : t -> t' -> unit
    val thunk : (unit -> t') -> t
    val update_thunk : t -> (unit -> t') -> unit

    include MemoN.S with type data := t' and type t := t

    val of_list : data list -> t
    val push : data -> t -> unit
    val pop : t -> data
    val insert : int -> data -> t -> unit
    val remove : int -> t -> data
    val memo_append : t -> t -> t
    val memo_filter : (data -> bool) -> t -> t

    val memo_filter_with_key :
      (module Hashtbl.SeededHashedType with type t = 'a) -> ('a -> data -> bool) -> 'a -> t -> t

    val memo_filter_map :
      (module AListType.BasicS with type atype = atype and type data = 'a and type t = 'b) ->
      ('a -> data option) ->
      'b ->
      t

    val memo_map :
      (module AListType.BasicS with type atype = atype and type data = 'a and type t = 'b) -> ('a -> data) -> 'b -> t

    val memo_map_with_key :
      (module Hashtbl.SeededHashedType with type t = 'a) ->
      (module AListType.BasicS with type atype = atype and type data = 'b and type t = 'c) ->
      ('a -> 'b -> data) ->
      'a ->
      'c ->
      t

    val memo_scan :
      (module AListType.BasicS with type atype = atype and type data = 'a and type t = 'b) ->
      ('a -> data -> data) ->
      'b ->
      data ->
      t

    val memo_tfold : (data -> data -> data) -> t -> AData.t
  end

  (** Module type for Adapton lists for a specific type. *)
  module type S = sig
    include BasicS

    val memo_quicksort : (data -> data -> int) -> t -> t
    val memo_mergesort : (data -> data -> int) -> t -> t
  end
end =
  AListType

(** Module type for Adapton lists. *)
module type AListType = sig
  type atype
  type 'a thunk
  type 'a alist
  type 'a alist' = [ `Cons of 'a * 'a alist | `Nil ]

  val is_incremental : bool
  val is_lazy : bool
  val id : 'a alist -> int
  val hash : int -> 'a alist -> int
  val equal : 'a alist -> 'a alist -> bool
  val refresh : unit -> unit
  val force : 'a alist -> 'a alist'
  val to_list : 'a alist -> 'a list
  val to_ids : 'a alist -> int list
  val take : int -> 'a alist -> 'a list
  val hd : 'a alist -> 'a
  val tl : 'a alist -> 'a alist

  module type BasicS = AListType.BasicS
  module type S = AListType.S

  module MakeBasic (R : Hashtbl.SeededHashedType) :
    BasicS
      with type atype = atype
       and type 'a thunk = 'a thunk
       and type data = R.t
       and type t = R.t alist
       and type t' = R.t alist'

  module Make (R : Hashtbl.SeededHashedType) :
    S
      with type atype = atype
       and type 'a thunk = 'a thunk
       and type data = R.t
       and type t = R.t alist
       and type t' = R.t alist'
end

(** {2 Adapton array mapped tries} *)

(** Module type for Adapton lists. *)
module rec AArrayMappedTrieType : sig
  module type S = sig
    type atype
    type 'a thunk
    type data
    type t

    val is_incremental : bool
    val is_lazy : bool
    val hash : int -> t -> int
    val equal : t -> t -> bool
    val refresh : unit -> unit
    val get : t -> int -> data option
    val empty : t
    val memo_add : t -> int -> data -> t
  end
end =
  AArrayMappedTrieType

(** Module type for Adapton lists. *)
module type AArrayMappedTrieType = sig
  type atype
  type 'a thunk
  type 'a aamt

  val is_incremental : bool
  val is_lazy : bool
  val hash : int -> 'a aamt -> int
  val equal : 'a aamt -> 'a aamt -> bool
  val refresh : unit -> unit
  val get : 'a aamt -> int -> 'a option

  module type S = AArrayMappedTrieType.S

  module Make (R : Hashtbl.SeededHashedType) :
    S with type atype = atype and type 'a thunk = 'a thunk and type data = R.t and type t = R.t aamt
end
