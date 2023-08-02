open Domain

module CLH = struct
  type t = node Atomic.t
  and node = int Atomic.t

  let create () : t = Atomic.make (Atomic.make 0)

  type rt = node * Domain.id

  let lock t =
    let qnode = Atomic.make 1 in
    let pred = Atomic.exchange t qnode in
    let rec loop () =
      if Atomic.get pred = 1 then (
        Domain.cpu_relax ();
        loop ())
    in
    loop ();
    (qnode, Domain.self ())

  exception WrongDomain

  let unlock (mynode, _id) = Atomic.decr mynode

  exception Lock_already_unlocked

  let unlock_exn ((mynode, id) : rt) =
    if Domain.self () <> id then raise WrongDomain
    else if Atomic.get mynode = 0 then raise Lock_already_unlocked
    else Atomic.decr mynode
end

module CLH_DLS = struct
  type t = {
    tail : node Atomic.t;
    myPred : node DLS.key;
    myNode : node DLS.key;
  }

  and node = bool Atomic.t

  let create () : t =
    {
      tail = Atomic.make (Atomic.make false);
      myPred = DLS.new_key (fun _ -> Atomic.make false);
      myNode = DLS.new_key (fun _ -> Atomic.make false);
    }

  let lock { tail; myPred; myNode } =
    let qnode = DLS.get myNode in
    Atomic.set qnode true;
    let pred = Atomic.exchange tail qnode in
    DLS.set myPred pred;
    while Atomic.get pred do
      Domain.cpu_relax ()
    done

  let unlock { myPred; myNode; _ } =
    let qnode = DLS.get myNode in
    Atomic.set qnode false;
    DLS.set myNode (DLS.get myPred)
end

module CLH_noalloc = struct
  type t = node Atomic.t
  and node = int Atomic.t

  type rt = node Atomic.t * node ref * node ref

  let create () : t = Atomic.make (Atomic.make 0)
  let register t : rt = (t, ref (Atomic.make 0), ref (Atomic.make 0))

  let lock ((tail, mypred, mynode) : rt) =
    let qnode = !mynode in
    Atomic.incr qnode;
    let pred = Atomic.exchange tail qnode in
    mypred := pred;
    while Atomic.get pred = 1 do
      Domain.cpu_relax ()
    done

  let unlock ((_, mypred, mynode) : rt) =
    Atomic.decr !mynode;
    mynode := !mypred
end
