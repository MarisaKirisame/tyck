(** Adapton thunks, alternative APIs, and applications. *)

include AdaptonZoo.Adapton
(** Adapton with a default functor-based API. *)

module PolyAPI = AdaptonUtil.PolyAPI.Make (AdaptonZoo.Adapton)
(** Adapton with a alternative polymorphic API. *)

module BasicAPI = AdaptonUtil.BasicAPI.Make (AdaptonZoo.Adapton)
(** Adapton with a alternative basic polymorphic API. *)

module AList = AdaptonUtil.AList.Make (AdaptonZoo.Adapton)
(** Adapton incremental lists. *)
