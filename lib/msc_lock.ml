type node = { locked : bool Atomic.t; mutable next : qnode }
and qnode = Nil | Next of node

type t = qnode Atomic.t
type rt = t * qnode

let create () = Atomic.make @@ Nil

let register (t : t) : rt =
  let mynode = { locked = Atomic.make false; next = Nil } in
  (t, Next mynode)

let lock ((tail, mynode) : rt) =
  let mynode_val = match mynode with Nil -> assert false | Next v -> v in
  let pred = Atomic.exchange tail mynode in
  match pred with
  | Nil -> ()
  | Next pred_node ->
      Atomic.set mynode_val.locked true;
      pred_node.next <- mynode;
      while Atomic.get mynode_val.locked do
        Domain.cpu_relax ()
      done

exception Return

let unlock ((tail, mynode) : rt) =
  let mynode_val = match mynode with Nil -> assert false | Next v -> v in
  let rec loop () =
    match mynode_val.next with
    | Nil ->
        (* no one as try to acquire the lock *)
        (* we try to reset [tail] to [Nil] (freeing the lock). If it fails,
           that means that another domain is trying to acquire the
           lock *)
        if not @@ Atomic.compare_and_set tail mynode Nil then (
          while mynode_val.next = Nil do
            (* Is the while loop needed ? or just calling [loop ()]
               enough. If line 20 is not finished when we reloop, then
               we redo the CAS for nothing. *)
            Domain.cpu_relax ()
          done;
          loop ())
        else ()
    | Next { locked; _ } ->
        mynode_val.next <- Nil;
        Atomic.set locked false
  in

  loop ()
