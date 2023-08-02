module Lock = Ocamlock.CLH_queue_lock
module Lock2 = Ocamlock.CLH_queue_lock2

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

  let work () =
    Lock.lock lock;
    let _random_add = some_random_work () in
    incr counter;
    Lock.unlock lock
  in

  let domains =
    Array.init ndomains (fun _ ->
        Domain.spawn (fun () ->
            Atomic.decr barrier;
            while Atomic.get barrier <> 0 do
              Domain.cpu_relax ()
            done;

            for _ = 1 to nincr do
              work ()
            done))
  in
  Array.iter Domain.join domains;
  assert (!counter = ndomains * nincr);
  Format.printf "Success with %d count.@." !counter

let test2 ndomains nincr =
  let counter = ref 0 in
  let lock = Lock2.create () in
  let barrier = Atomic.make ndomains in

  let some_random_work () =
    let a = ref 0 in
    for _i = 0 to 99 do
      a := !a + Random.int 2
    done;
    !a
  in

  let work () =
    let al = Lock2.lock lock in
    let _random_add = some_random_work () in
    incr counter;
    Lock2.unlock al
  in

  let domains =
    Array.init ndomains (fun _ ->
        Domain.spawn (fun () ->
            Atomic.decr barrier;
            while Atomic.get barrier <> 0 do
              Domain.cpu_relax ()
            done;

            for _ = 1 to nincr do
              work ()
            done))
  in
  Array.iter Domain.join domains;
  assert (!counter = ndomains * nincr);
  Format.printf "Success with %d count.@." !counter

let _ =
  test 4 100_000;
  test2 4 100_000
