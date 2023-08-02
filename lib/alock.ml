(* TODO : add something to check that no more than 'capacity'
   domains are acessing the lock at the same time.
   For example :

   - a domain put some id (random ?) in its flag and stop spinning
   when the value is something else (-1 ?)

   - if a domain tries to acquire a flag with something else then the
   default value -> to many domains

   - when releasing put (-1) in the cell
*)

type t = { flags : bool array; tail : int Atomic.t; mask : int; padding : int }

let create ?(padding = 1) size : t =
  let flags = Array.make (size * padding) false in
  flags.(0) <- true;
  { flags; tail = Atomic.make 0; mask = size - 1; padding }

type rt = t * int

let lock ({ flags; tail; padding; mask } as t) =
  let slot = Atomic.fetch_and_add tail padding land mask in
  while not flags.(slot) do
    ()
  done;
  (t, slot)

let lock_relax ({ flags; tail; padding; mask } as t) =
  let slot = Atomic.fetch_and_add tail padding land mask in
  while not flags.(slot) do
    Domain.cpu_relax ()
  done;
  (t, slot)

let unlock ({ flags; padding; mask; _ }, slot) =
  flags.(slot) <- false;
  flags.((slot + padding) land mask) <- true
