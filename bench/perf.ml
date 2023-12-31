module Work = All_locks_def.Work_MCS

let test ndomain nlock =
  let counter = ref 0 in
  let lock = Work.create ndomain in
  let barrier = Atomic.make ndomain in

  let domains =
    List.init ndomain (fun _ ->
        Domain.spawn (fun () ->
            let rlock = Work.register lock in
            Atomic.decr barrier;
            while Atomic.get barrier <> 0 do
              Domain.cpu_relax ()
            done;
            for _ = 0 to nlock - 1 do
              Work.incr counter rlock
            done))
  in
  List.iter Domain.join domains

let _ = test 4 100_000
(*;
  Gc.major ();
  Gc.print_stat stdout*)
