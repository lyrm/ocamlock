(* TASlock, TTASlock implementations from The Art of Multiprocessor Programming -
   M.Herlihy, N. Shavit, V. Luchangco, M. Spear *)

module type LOCK = sig
  type t

  val create : unit -> t
  val lock : t -> unit
  val unlock : t -> unit
end

module TTASlock : LOCK = struct
  type t = bool Atomic.t

  let create () = Atomic.make false

  let rec lock_ ?(backoff = Backoff.default) t =
    if Atomic.get t then (
      Domain.cpu_relax ();
      lock_ ~backoff t)
    else if not (Atomic.compare_and_set t false true) then
      lock_ ~backoff:(Backoff.once backoff) t

  let lock t = lock_ t
  let unlock t = Atomic.set t false
end
