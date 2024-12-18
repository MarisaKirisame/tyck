(** Convenience modules for built-in and other useful types. *)

module Int = struct
  type t = int

  let hash = Hashtbl.seeded_hash
  let equal = ( == )
end

module Char = struct
  type t = char

  let hash = Hashtbl.seeded_hash
  let equal = ( == )
end

module String = struct
  type t = string

  let hash = Hashtbl.seeded_hash
  let equal = ( = )
end

module Float = struct
  type t = float

  let hash = Hashtbl.seeded_hash
  let equal = ( = )
end

module Bool = struct
  type t = bool

  let hash = Hashtbl.seeded_hash
  let equal = ( == )
end

module Int32 = struct
  type t = int32

  let hash = Hashtbl.seeded_hash
  let equal = ( = )
end

module Int64 = struct
  type t = int64

  let hash = Hashtbl.seeded_hash
  let equal = ( = )
end

module Nativeint = struct
  type t = nativeint

  let hash = Hashtbl.seeded_hash
  let equal = ( = )
end

module Option (A : Hashtbl.SeededHashedType) = struct
  type t = A.t option

  let hash seed = function
    | Some a -> A.hash (Hashtbl.seeded_hash seed `Some) a
    | None -> Hashtbl.seeded_hash seed `None

  let equal x x' = x == x' || match (x, x') with Some a, Some a' -> A.equal a a' | _ -> false
end

module Tuple2 (A : Hashtbl.SeededHashedType) (B : Hashtbl.SeededHashedType) = struct
  type t = A.t * B.t

  let hash seed (a, b) = B.hash (A.hash seed a) b
  let equal ((a, b) as x) ((a', b') as x') = x == x' || (A.equal a a' && B.equal b b')
end

module Tuple3 (A : Hashtbl.SeededHashedType) (B : Hashtbl.SeededHashedType) (C : Hashtbl.SeededHashedType) = struct
  type t = A.t * B.t * C.t

  let hash seed (a, b, c) = C.hash (B.hash (A.hash seed a) b) c
  let equal ((a, b, c) as x) ((a', b', c') as x') = x == x' || (A.equal a a' && B.equal b b' && C.equal c c')
end

module Tuple4
    (A : Hashtbl.SeededHashedType)
    (B : Hashtbl.SeededHashedType)
    (C : Hashtbl.SeededHashedType)
    (D : Hashtbl.SeededHashedType) =
struct
  type t = A.t * B.t * C.t * D.t

  let hash seed (a, b, c, d) = D.hash (C.hash (B.hash (A.hash seed a) b) c) d

  let equal ((a, b, c, d) as x) ((a', b', c', d') as x') =
    x == x' || (A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d')
end

module Unit = struct
  type t = unit

  let hash seed () = seed
  let equal = ( == )
end

(** Counter. *)
module Counter = struct
  type t = int ref

  let make x : t = ref x

  let next c =
    let x = !c in
    incr c;
    x
end

(** Random number stream. *)
module Seeds = struct
  type t = Seeds of int * t Lazy.t

  let make ?seeds:seeds_opt () =
    let rng = match seeds_opt with Some seeds -> Random.State.make seeds | None -> Random.State.make_self_init () in
    let rec seeds () = Seeds (Random.State.bits rng, Lazy.from_fun seeds) in
    seeds ()

  let pop (Seeds (s, (lazy seeds))) = (s, seeds)
  let hash seed (Seeds (s, _)) = Hashtbl.seeded_hash seed s
  let equal = ( == )
end

(** Infer and make Function modules. *)
let makeFunction (type a b) () : (module Hashtbl.SeededHashedType with type t = a -> b) =
  (module struct
    type t = a -> b

    let equal = ( == )
    let hash = Hashtbl.seeded_hash
  end)
