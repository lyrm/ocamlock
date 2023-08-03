open Ocamlock

let test ndomains nincr =
  let counter = ref 0 in
  let lock = DLA_mutex_naive.create () in
  let barrier = Atomic.make ndomains in

  let some_random_work () =
    let a = ref 0 in
    for _i = 0 to 99 do
      a := !a + Random.int 2
    done;
    !a
  in

  let work () =
    DLA_mutex_naive.lock lock;
    let _random_add = some_random_work () in
    incr counter;
    DLA_mutex_naive.unlock lock
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

let test_CLH ndomains nincr =
  let counter = ref 0 in
  let lock = DLA_mutex_CLH.create () in
  let barrier = Atomic.make ndomains in

  let some_random_work () =
    let a = ref 0 in
    for _i = 0 to 99 do
      a := !a + Random.int 2
    done;
    !a
  in

  let critical_section id rt =
    DLA_mutex_CLH.lock rt;
    Format.printf "Begin - Domain %i@." id;
    let _random_add = some_random_work () in
    incr counter;
    Format.printf "End   - Domain %i@." id;
    DLA_mutex_CLH.unlock rt
  in

  let domains =
    Array.init ndomains (fun id ->
        Domain.spawn (fun () ->
            let rt = DLA_mutex_CLH.register lock in
            Atomic.decr barrier;
            while Atomic.get barrier <> 0 do
              Domain.cpu_relax ()
            done;

            for _ = 1 to nincr do
              critical_section id rt
            done))
  in
  Array.iter Domain.join domains;
  assert (!counter = ndomains * nincr);
  Format.printf "Success with %d count.@." !counter

let _ =
  test 8 10_000;
  test_CLH 2 5
