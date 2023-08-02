open Ocamlock
open Taslock

module type WORK = sig
  type t

  val create : int -> t
  val incr : int ref -> t -> unit
end

module Work_builder (Lock : LOCK) : WORK = struct
  type t = Lock.t

  let create _ = Lock.create ()

  let incr counter lock =
    Lock.lock lock;
    incr counter;
    Lock.unlock lock
end

module Work_alock : WORK = struct
  type t = Alock.t

  let create size = Alock.create size

  let incr counter lock =
    let rlock = Alock.lock lock in
    incr counter;
    Alock.unlock rlock
end

module Work_alock_padded64 : WORK = struct
  type t = Alock.t

  let create size = Alock.create ~padding:8 size

  let incr counter lock =
    let rlock = Alock.lock lock in
    incr counter;
    Alock.unlock rlock
end

module Work_alock_padded128 : WORK = struct
  type t = Alock.t

  let create size = Alock.create ~padding:8 size

  let incr counter lock =
    let rlock = Alock.lock_relax lock in
    incr counter;
    Alock.unlock rlock
end

module Work_CLH : WORK = struct
  type t = CLH_queue_lock2.t

  let create _ = CLH_queue_lock2.create ()

  let incr counter lock =
    let rlock = CLH_queue_lock2.lock lock in
    incr counter;
    CLH_queue_lock2.unlock rlock
end

module Work_TAS : WORK = Work_builder ((TTASlock : LOCK))
module Work_Mutex : WORK = Work_builder ((Mutex : LOCK))
module Work_CLH_DLS : WORK = Work_builder ((CLH_queue_lock : LOCK))

let all_locks : (string * (module WORK)) list =
  [
    ("Mutex", (module Work_TAS));
    ("TTAS-lock", (module Work_TAS));
    ("Alock-padded64", (module Work_alock_padded64));
    ("CLH-DLS", (module Work_CLH_DLS));
    ("CLH", (module Work_CLH));
  ]
