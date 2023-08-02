open Ocamlock
open Taslock

module type WORK = sig
  type t
  type rt

  val create : int -> t
  val register : t -> rt
  val incr : int ref -> rt -> unit
end

module Work_builder (Lock : LOCK) : WORK = struct
  type t = Lock.t
  type rt = t

  let create _ = Lock.create ()
  let register t = t

  let incr counter lock =
    Lock.lock lock;
    incr counter;
    Lock.unlock lock
end

module Work_alock : WORK = struct
  type t = Alock.t

  let create size = Alock.create size

  type rt = t

  let register t = t

  let incr counter lock =
    let rlock = Alock.lock lock in
    incr counter;
    Alock.unlock rlock
end

module Work_alock_padded64 : WORK = struct
  type t = Alock.t

  let create size = Alock.create ~padding:8 size

  type rt = t

  let register t = t

  let incr counter lock =
    let rlock = Alock.lock lock in
    incr counter;
    Alock.unlock rlock
end

module Work_alock_padded128 : WORK = struct
  type t = Alock.t

  let create size = Alock.create ~padding:8 size

  type rt = t

  let register t = t

  let incr counter lock =
    let rlock = Alock.lock_relax lock in
    incr counter;
    Alock.unlock rlock
end

module Work_CLH : WORK = struct
  type t = CLH_queue_lock.t

  let create _ = CLH_queue_lock.create ()

  type rt = t

  let register t = t

  let incr counter lock =
    let rlock = CLH_queue_lock.lock lock in
    incr counter;
    CLH_queue_lock.unlock rlock
end

module Work_CLH_noalloc : WORK = struct
  type t = CLH_queue_lock_noalloc.t
  type rt = CLH_queue_lock_noalloc.rt

  let create _ = CLH_queue_lock.create ()
  let register = CLH_queue_lock_noalloc.register

  let incr counter rlock =
    CLH_queue_lock_noalloc.lock rlock;
    incr counter;
    CLH_queue_lock_noalloc.unlock rlock
end

module Work_MCS : WORK = struct
  type t = MCS_lock.t
  type rt = MCS_lock.rt

  let create _ = MCS_lock.create ()
  let register = MCS_lock.register

  let incr counter rlock =
    MCS_lock.lock rlock;
    incr counter;
    MCS_lock.unlock rlock
end

module Work_TAS : WORK = Work_builder ((TTASlock : LOCK))
module Work_Mutex : WORK = Work_builder ((Mutex : LOCK))
module Work_CLH_DLS : WORK = Work_builder ((CLH_queue_lock_DLS : LOCK))

let all_locks : (string * (module WORK)) list =
  [
    ("Mutex", (module Work_TAS));
    ("TTAS-lock", (module Work_TAS));
    ("Alock-padded64", (module Work_alock_padded64));
    (*("CLH-DLS", (module Work_CLH_DLS));
      ("CLH", (module Work_CLH));*)
    ("CLH-noalloc", (module Work_CLH_noalloc));
    ("MCS-lock", (module Work_MCS));
  ]
