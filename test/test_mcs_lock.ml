module Lock = Ocamlock.MCS_lock

let test ndomains nincr =
  let counter = ref 0 in
  let lock = Lock.create () in
  let barrier = Atomic.make ndomains in

  let some_random_work () =
    let a = ref 0 in
    for _i = 0 to 99 do
      a := !a + Random.int 2
    done;
    !a
  in

  let work rlock =
    Lock.lock rlock;
    let _random_add = some_random_work () in
    incr counter;
    Lock.unlock rlock
  in

  let domains =
    Array.init ndomains (fun _ ->
        Domain.spawn (fun () ->
            let rlock = Lock.register lock in
            Atomic.decr barrier;
            while Atomic.get barrier <> 0 do
              Domain.cpu_relax ()
            done;

            for _ = 1 to nincr do
              work rlock
            done))
  in
  Array.iter Domain.join domains;
  assert (!counter = ndomains * nincr);
  Format.printf "Success with %d count.@." !counter

let _ = test 4 10_000
