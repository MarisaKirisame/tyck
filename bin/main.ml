(*I tried using janestreet's incremental library, but sadly it over-track dependency.*)
module Inc = Tyck.AdaptonUtil.PolyAPI.Make (Tyck.AdaptonZoo.EagerTotalOrder)
open Inc

let _ = Printf.printf "\n"

let time f s =
  let t = Sys.time () in
  (*let nnr = State.num_nodes_recomputed State.t in*)
  let fx = f () in
  Printf.printf "%s Time: %fms\n" s (1000. *. (Sys.time () -. t));
  (*Printf.printf "%s Node: %d\n" s (State.num_nodes_recomputed State.t - nnr);*)
  fx

let rec church n f x = if n <= 0 then x else church (n - 1) f (f x)
let size = 10000

(*let _ = State.set_max_height_allowed State.t (size * 10)*)

(*a non-incremental version*)
module Demo0 = struct
  type expr = X | Lit of int | Pair of expr * expr | Zro of expr | Fst of expr
  type ty = Int | Prod of ty * ty | TypeError

  let rec tyck (e : expr) (x_type : ty) : ty =
    let recur x = tyck x x_type in
    match e with
    | X -> x_type
    | Lit _ -> Int
    | Pair (x, y) -> Prod (recur x, recur y)
    | Zro x -> ( match recur x with Prod (x, _) -> x | _ -> TypeError)
    | Fst y -> ( match recur y with Prod (_, y) -> y | _ -> TypeError)

  let expr = church size (fun x -> Zro x) (church size (fun x -> Pair (x, Lit 0)) X)
  let _ = time (fun _ -> tyck expr Int) "Demo0"
end

(*an incremental version that doesnt really work - changing x_type recompute everything*)
module Demo1 = struct
  type expr = X | Lit of int | Pair of expr * expr | Zro of expr | Fst of expr
  type ty = Int | Prod of ty * ty | TypeError

  let rec tyck (e : expr) (x_type : ty thunk) : ty thunk =
    let recur x = tyck x x_type in
    match e with
    | X -> x_type
    | Lit _ -> const Int
    | Pair (x, y) -> map2 (fun x y -> Prod (x, y)) (recur x) (recur y)
    | Zro x -> map (fun x -> match x with Prod (x, _) -> x | _ -> TypeError) (recur x)
    | Fst y -> map (fun y -> match y with Prod (_, y) -> y | _ -> TypeError) (recur y)

  let expr = church size (fun x -> Zro x) (church size (fun x -> Pair (x, Lit 0)) X)
  let x_type = const Int
  let expr_type = tyck expr x_type

  (*there is zero saving, and due to overhead it is slower then recomputing from scratch.*)
  let _ =
    time
      (fun _ ->
        update_const x_type (Prod (Int, Int));
        refresh ())
      "Demo1"
end

(*The key problem is that Incr.t must apply deep into the adt,
  which provide better granularity.
  The recursive structure allow *update-for-free*: a subterm can be updated without any cost,
  merely by sharing the same Incr.t*)
module Demo2 = struct
  type expr = X | Lit of int | Pair of expr * expr | Zro of expr | Fst of expr

  type pre_ty = Int | Prod of ty * ty | TypeError
  and ty = pre_ty thunk

  let rec tyck (e : expr) (x_type : ty) : ty =
    let recur x = tyck x x_type in
    match e with
    | X -> x_type
    | Lit _ -> const Int
    | Pair (x, y) -> const (Prod (recur x, recur y))
    | Zro x -> bind (recur x) (fun x -> match x with Prod (x, _) -> x | _ -> const TypeError)
    | Fst y -> bind (recur y) (fun y -> match y with Prod (_, y) -> y | _ -> const TypeError)

  let expr = church size (fun x -> Zro x) (church size (fun x -> Pair (x, Lit 0)) X)
  let x_type = const Int
  let expr_type = tyck expr x_type

  (*Now it is free.*)
  let _ =
    time
      (fun _ ->
        update_const x_type (Prod (const Int, const Int));
        refresh ())
      "Demo2"
end

(*Demo2 does not support changing the AST which is a critical feature.
  Let's try to fix that by scattering Incr.t deep inside the expr AST as well.
  Sadly, now update is not free - the bind per recursion cause no Incr.t to be the same, thus no propagation sharing.*)
module Demo3 = struct
  type pre_expr = X | Lit of int | Pair of expr * expr | Zro of expr | Fst of expr
  and expr = pre_expr thunk

  type pre_ty = Int | Prod of ty * ty | TypeError
  and ty = pre_ty thunk

  let rec tyck (e : expr) (x_type : ty) : ty =
    let recur x = tyck x x_type in
    bind e (fun e ->
        match e with
        | X -> x_type
        | Lit _ -> const Int
        | Pair (x, y) -> const (Prod (recur x, recur y))
        | Zro x -> bind (recur x) (fun x -> match x with Prod (x, _) -> x | _ -> const TypeError)
        | Fst y -> bind (recur y) (fun y -> match y with Prod (_, y) -> y | _ -> const TypeError))

  let expr = church size (fun x -> const (Zro x)) (church size (fun x -> const (Pair (x, const (Lit 0)))) (const X))
  let x_type = const Int
  let expr_type = tyck expr x_type

  let _ =
    time
      (fun _ ->
        update_const x_type (Prod (const Int, const Int));
        refresh ())
      "Demo3"
end

(*The trick to retain propagation sharing is via staging.
  In particular we use a Incr.t of ty (which is already incrementalized) during tyck.
  To maintain clear stage separation, this time we should not apply it deep into the pre_ty adt.*)
module Demo45 = struct
  type pre_expr = X | Lit of int | Pair of expr * expr | Zro of expr | Fst of expr
  and expr = pre_expr thunk

  type pre_ty = Int | Prod of ty * ty | TypeError
  and ty = pre_ty thunk

  (*Unfortunately, this code make Demo5 run in O(n^2) instead of O(1).
    Note that this is slower than recomputing from scratch which is O(n).
    The problem is that the AST edit propagate outward, causing a recursive recomputation.
    Worse yet, the computation create new value, forcing more propagation*)
  let rec tyck (e : expr) (x_type : ty thunk) : ty thunk =
    let recur x = tyck x x_type in
    bind e (fun e ->
        match e with
        | X -> x_type
        | Lit _ -> const (const Int)
        (*Note how the const below can be inserted outside map2 and inside map2.
          This decide whether the outer Incr change should effect the inner or outer Incr.
          To maintain stage separation it should effect the outer Incr, so the inside is constant*)
        | Pair (x, y) -> map2 (fun x y -> const (Prod (x, y))) (recur x) (recur y)
        (*This is where free propagation happens: note how an incremental is extracted from Prod and use as-is, without any change.*)
        | Zro x -> map (fun x -> bind x (fun x -> match x with Prod (x, _) -> x | _ -> const TypeError)) (recur x)
        | Fst y -> map (fun y -> bind y (fun y -> match y with Prod (_, y) -> y | _ -> const TypeError)) (recur y))

  (*The fix is to memo the input, avoiding needless recomputation.
    Alas, it is O(n) and not O(1) because we are returning new Incr.t,
    suggesting that it need further propagation.
    There are known solution - 
    writing the incremental code manually, or using nominal adapton to suggest nothing changed.
    Irregardless, this is a problem we need to deal with anyway,
    and is orthogonal to free propagation, so I wont delve deeper.*)
  let rec tyck (e : expr) (x_type : ty thunk) : ty thunk =
    memo2
      (fun recur e x_type ->
        let recur x = recur x x_type in
        bind e (fun e ->
            match e with
            | X -> x_type
            | Lit _ -> const (const Int)
            | Pair (x, y) -> map2 (fun x y -> const (Prod (x, y))) (recur x) (recur y)
            | Zro x -> map (fun x -> bind x (fun x -> match x with Prod (x, _) -> x | _ -> const TypeError)) (recur x)
            | Fst y -> map (fun y -> bind y (fun y -> match y with Prod (_, y) -> y | _ -> const TypeError)) (recur y)))            
            
      e x_type

  let expr_base = const X
  let expr = church size (fun x -> const (Zro x)) (church size (fun x -> const (Pair (x, const (Lit 0)))) expr_base)
  let x_type = const Int
  let expr_type = tyck expr (const x_type)

  let _ =
    time
      (fun _ ->
        update_const x_type (Prod (const Int, const Int));
        refresh ())
      "Demo4"

  let _ =
    time
      (fun _ ->
        update_const expr_base (Pair (const X, const X));
        refresh ())
      "Demo5"
end

(*Of course, we dont really modify ty.
  What happens is we modify expr which modify ty.
  Does everything still work? Let's find out.*)
module Demo67 = struct
  type pre_expr = LetX of expr * expr | X | Lit of int | Pair of expr * expr | Zro of expr | Fst of expr
  and expr = pre_expr thunk

  type pre_ty = Int | Prod of ty * ty | TypeError
  and ty = pre_ty thunk

  let rec tyck (e : expr) (x_type : ty thunk) : ty thunk =
    let recur x = tyck x x_type in
    bind e (fun e ->
        match e with
        (*Convert the expr change to a ty change. Note how going the other way is impossible.
          This line of code make me high.*)
        | LetX (x, i) -> tyck i (const (join (recur x)))
        | X -> x_type
        | Lit _ -> const (const Int)
        | Pair (x, y) -> map2 (fun x y -> const (Prod (x, y))) (recur x) (recur y)
        | Zro x -> map (fun x -> bind x (fun x -> match x with Prod (x, _) -> x | _ -> const TypeError)) (recur x)
        | Fst y -> map (fun y -> bind y (fun y -> match y with Prod (_, y) -> y | _ -> const TypeError)) (recur y))

  let expr_base = const X
  let let_rhs = const X

  let expr =
    const
      (LetX
         ( let_rhs,
           church size (fun x -> const (Zro x)) (church size (fun x -> const (Pair (x, const (Lit 0)))) expr_base) ))

  let x_type = const Int
  let expr_type = tyck expr (const x_type)

  let _ =
    time
      (fun _ ->
        update_const x_type (Prod (const Int, const Int));
        refresh ())
      "Demo6"

  let _ =
    time
      (fun _ ->
        update_const let_rhs (Pair (const X, const X));
        refresh ())
      "Demo7"

  let expr_base = const X
  let let_rhs = const X

  let expr =
    church size
      (fun x -> const (Zro x))
      (church size (fun x -> const (Pair (x, const (Lit 0)))) (const (LetX (let_rhs, expr_base))))

  let x_type = const Int
  let expr_type = tyck expr (const x_type)

  let _ =
    time
      (fun _ ->
        update_const x_type (Prod (const Int, const Int));
        refresh ())
      "Demo8"

  let _ =
    time
      (fun _ ->
        update_const let_rhs (Pair (const X, const X));
        refresh ())
      "Demo9"
end
