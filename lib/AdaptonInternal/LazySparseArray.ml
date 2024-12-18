(** Fixed-size lazy sparse arrays. *)

module T : sig
  type 'a t = private {
    hash : int;
    mutable thunk : int -> 'a option;
    mutable initialized : int;
    mutable nonnull : int;
    mutable array : 'a array;
  }

  val size : int
  val key_bits : int
  val hash : int -> 'a t -> int
  val equal : 'a t -> 'a t -> bool
  val make : (int -> 'a option) -> 'a t
  val get : 'a t -> int -> 'a option
end = struct
  type 'a t = {
    hash : int;
    mutable thunk : int -> 'a option;
    mutable initialized : int;
    mutable nonnull : int;
    mutable array : 'a array;
  }
  (** Type of lazy sparse arrays containing ['a]. *)

  (** Size of lazy sparse arrays. *)
  let size = if Sys.word_size == 32 then 16 else 32

  (** Key-width in bits of lazy sparse arrays. *)
  let key_bits = if size == 16 then 4 else 5

  (**/**) (* helper functions/values *)

  let popcount16 =
    let map16 =
      let map4 = [| 0; 1; 1; 2; 1; 2; 2; 3; 1; 2; 2; 3; 2; 3; 3; 4 |] in
      Array.init 0x10000 (fun x ->
          map4.(x land 0xF) + map4.((x lsr 4) land 0xF) + map4.((x lsr 8) land 0xF) + map4.((x lsr 12) land 0xF))
    in
    fun x -> map16.(x)

  let popcount32 x = popcount16 (x land 0xFFFF) + popcount16 (x lsr 16)
  let popcount = if Sys.word_size == 32 then popcount16 else popcount32
  let full_bitmask = (1 lsl size) - 1
  let null_thunk _ : 'a option = failwith "null thunk"

  (**/**)

  (** Create a new lazy sparse array. *)
  let make thunk = { hash = Hashtbl.hash thunk; thunk; initialized = 0; nonnull = 0; array = [||] }

  (** Compute the hash value of a lazy sparse array. *)
  let hash seed xs = Hashtbl.seeded_hash seed xs.hash

  (** Compute whether two lazy sparse arrays are equal. *)
  let equal = ( == )

  (** Return the element [i] of a lazy sparse array. *)
  let get xs k =
    if k < 0 || k >= size then invalid_arg "index out of bounds";
    let mask = 1 lsl k in
    let slot = popcount (xs.nonnull land (mask - 1)) in
    if xs.initialized land mask == 0 then (
      let value =
        match xs.thunk k with
        | None -> None
        | Some value as opt_value ->
            xs.nonnull <- xs.nonnull lor mask;
            xs.array <-
              Array.init
                (Array.length xs.array + 1)
                (fun k -> if k < slot then xs.array.(k) else if k == slot then value else xs.array.(k - 1));
            opt_value
      in
      xs.initialized <- xs.initialized lor mask;
      if xs.initialized == full_bitmask then xs.thunk <- null_thunk;
      value)
    else if xs.nonnull land mask == 0 then None
    else Some xs.array.(slot)
end

include T
