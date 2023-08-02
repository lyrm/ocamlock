module CLH_DLS = Ocamlock.CLH_queue_lock_DLS
module CLH = Ocamlock.CLH_queue_lock
module CLH_noalloc = Ocamlock.CLH_queue_lock_noalloc

let test_DLS ndomains nincr =
  let counter = ref 0 in
  let lock = CLH_DLS.create () in
  let barrier = Atomic.make ndomains in

  let some_random_work () =
    let a = ref 0 in
    for _i = 0 to 99 do
      a := !a + Random.int 2
    done;
    !a
  in

  let work () =
    CLH_DLS.lock lock;
    let _random_add = some_random_work () in
    incr counter;
    CLH_DLS.unlock lock
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

let test ndomains nincr =
  let counter = ref 0 in
  let lock = CLH.create () in
  let barrier = Atomic.make ndomains in

  let some_random_work () =
    let a = ref 0 in
    for _i = 0 to 99 do
      a := !a + Random.int 2
    done;
    !a
  in

  let work () =
    let al = CLH.lock lock in
    let _random_add = some_random_work () in
    incr counter;
    CLH.unlock al
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

let test_noalloc ndomains nincr =
  let counter = ref 0 in
  let lock = CLH_noalloc.create () in
  let barrier = Atomic.make ndomains in

  let some_random_work () =
    let a = ref 0 in
    for _i = 0 to 99 do
      a := !a + Random.int 2
    done;
    !a
  in

  let work rlock =
    CLH_noalloc.lock rlock;
    let _random_add = some_random_work () in
    incr counter;
    CLH_noalloc.unlock rlock
  in

  let domains =
    Array.init ndomains (fun _ ->
        Domain.spawn (fun () ->
            let rlock = CLH_noalloc.register lock in
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

let _ =
  test_DLS 4 100_000;
  test 4 100_000;
  test_noalloc 4 100_000
