module DLA = Domain_local_await

module DLA_mutex_naive = struct
  type t = s Atomic.t
  and s = Free | Locked of (unit -> unit) list

  let create () = Atomic.make Free

  let lock t : unit =
    let rec loop ?dla () =
      match Atomic.get t with
      | Free ->
          if not @@ Atomic.compare_and_set t Free (Locked []) then loop ?dla ()
      | Locked others as t_val ->
          let dla =
            match dla with None -> DLA.prepare_for_await () | Some dla -> dla
          in
          if
            not
            @@ Atomic.compare_and_set t t_val (Locked (dla.release :: others))
          then loop ~dla ()
          else dla.await ()
    in
    loop ()

  let rec unlock t : unit =
    match Atomic.get t with
    | Free -> assert false
    | Locked [] as t_val ->
        if not @@ Atomic.compare_and_set t t_val Free then unlock t
    | Locked (next :: others) as t_val ->
        if not @@ Atomic.compare_and_set t t_val (Locked others) then unlock t
        else next ()
end

module DLA_mutex_CLH = struct
  type t = node_a Atomic.t
  and node_a = node Atomic.t
  and node = Free | Locked | Next of (unit -> unit)

  let create () : t = Atomic.make (Atomic.make Free)

  type rt = t * node_a ref * node_a ref * DLA.t

  let register (t : t) : rt =
    let dla = DLA.prepare_for_await () in
    (t, ref (Atomic.make Free), ref (Atomic.make Free), dla)

  let lock ((tail, mynode, mypred, dla) : rt) : unit =
    Format.printf "Lock @.";
    let qnode = !mynode in
    Atomic.set qnode Locked;
    let is_locked = Atomic.exchange tail qnode in
    mypred := is_locked;
    match Atomic.get is_locked with
    | Next _ -> assert false
    | Locked ->
        Format.printf "   L-Locked @.";
        Atomic.set is_locked (Next dla.release);
        dla.await ()
    | Free ->
        (* nobody as the lock*)
        Format.printf "  L-Free @.";
        ()

  let rec unlock ((_, mynode, mypred, _) as t : rt) : unit =
    Format.printf "Unlock @.";
    match Atomic.get !mynode with
    | Free -> assert false
    | Locked as node_val ->
        Format.printf "   U-Locked @.";
        if not @@ Atomic.compare_and_set !mynode node_val Free then unlock t
        else (
          mynode := !mypred;
          Atomic.set !mypred Free)
    | Next next_release as node_val ->
        Format.printf "   U-Next @.";
        if not @@ Atomic.compare_and_set !mynode node_val Free then assert false
        else next_release ();
        mynode := !mypred;
        Atomic.set !mypred Free
end
