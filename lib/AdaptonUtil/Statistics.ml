(** Adapton performance statistics and measurement function. *)

module Counts = struct
  let create = ref 0
  let hit = ref 0
  let miss = ref 0
  let update = ref 0
  let dirty = ref 0
  let clean = ref 0
  let evaluate = ref 0
end

let word_size = Sys.word_size / 8
let word_bytes words = word_size * words
let word_megabytes words = float_of_int (word_bytes words) /. 1048576.
let get_time = Unix.gettimeofday

let get_heap_stack () =
  Gc.(
    let s = quick_stat () in
    (s.heap_words, s.stack_size))

(**/**) (* helper values/functions *)

let heap_stacks = ref []
let top_stack = ref 0

let _ =
  Gc.create_alarm (fun () ->
      let heap, stack =
        Gc.(
          let s = quick_stat () in
          (s.heap_words, s.stack_size))
      in
      List.iter
        (fun (h, s) ->
          h := max heap !h;
          s := max stack !s)
        !heap_stacks;
      top_stack := max stack !top_stack)

(**/**)

let get_top_heap_stack () = (Gc.((quick_stat ()).top_heap_words), !top_stack)

type t = {
  time : float;  (** Elapsed time in seconds. *)
  heap : int;  (** Max heap delta in bytes. *)
  stack : int;  (** Max stack delta in bytes. *)
  create : int;  (** Thunks created. *)
  hit : int;  (** Thunks memo-hit. *)
  miss : int;  (** Thunks memo-missed. *)
  update : int;  (** Thunks updated. *)
  evaluate : int;  (** Thunks re-evaluated. *)
  dirty : int;
      (** For {!Adapton}, dependencies to be checked; for {!EagerTotalOrder}, thunks scheduled for re-evaluation. *)
  clean : int;
      (** For {!Adapton}, dependencies checked clean; for {!EagerTotalOrder}, thunks unscheduled for re-evaluation (due to invalidation). *)
}

let add s s' =
  {
    time = s.time +. s'.time;
    heap = s.heap + s'.heap;
    stack = s.stack + s'.stack;
    create = s.create + s'.create;
    hit = s.hit + s'.hit;
    miss = s.miss + s'.miss;
    update = s.update + s'.update;
    evaluate = s.evaluate + s'.evaluate;
    dirty = s.dirty + s'.dirty;
    clean = s.clean + s'.clean;
  }

let measure f =
  let heap' = ref 0 in
  let stack' = ref 0 in
  let create = !Counts.create in
  let hit = !Counts.hit in
  let miss = !Counts.miss in
  let update = !Counts.update in
  let dirty = !Counts.dirty in
  let clean = !Counts.clean in
  let evaluate = !Counts.evaluate in
  let old_heap_stacks = !heap_stacks in
  heap_stacks := (heap', stack') :: old_heap_stacks;
  let heap, stack = get_heap_stack () in
  heap' := max heap !heap';
  stack' := max stack !stack';
  let time = get_time () in
  let x = f () in
  let time = get_time () -. time in
  heap_stacks := old_heap_stacks;
  let measurement =
    {
      time;
      heap = word_bytes (!heap' - heap);
      stack = word_bytes (!stack' - stack);
      create = !Counts.create - create;
      hit = !Counts.hit - hit;
      miss = !Counts.miss - miss;
      update = !Counts.update - update;
      evaluate = !Counts.evaluate - evaluate;
      dirty = !Counts.dirty - dirty;
      clean = !Counts.clean - clean;
    }
  in
  (x, measurement)
