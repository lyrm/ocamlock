open Domain

module CLH_DLS = struct
  type t = {
    tail : node Atomic.t;
    myPred : node DLS.key;
    myNode : node DLS.key;
  }

  and node = bool ref

  let create () : t =
    {
      tail = Atomic.make (ref false);
      myPred = DLS.new_key (fun _ -> ref false);
      myNode = DLS.new_key (fun _ -> ref false);
    }

  let lock { tail; myPred; myNode } =
    let qnode = DLS.get myNode in
    qnode := true;
    let pred = Atomic.exchange tail qnode in
    DLS.set myPred pred;
    while !pred do
      Domain.cpu_relax ()
    done

  let unlock { myPred; myNode; _ } =
    let qnode = DLS.get myNode in
    qnode := false;
    DLS.set myNode (DLS.get myPred)
end

module CLH = struct
  type t = node Atomic.t
  and node = bool ref

  let create () : t = Atomic.make (ref false)

  type rt = node * Domain.id

  let lock t =
    let qnode = ref true in
    let pred = Atomic.exchange t qnode in
    let rec loop () =
      if !pred then (
        Domain.cpu_relax ();
        loop ())
    in
    loop ();
    (qnode, Domain.self ())

  exception WrongDomain

  let unlock (mynode, id) =
    if Domain.self () <> id then raise WrongDomain else mynode := false

  exception Lock_already_unlocked

  let unlock_exn (mynode, id) =
    if Domain.self () <> id then raise WrongDomain
    else if !mynode then raise Lock_already_unlocked
    else mynode := false
end
