open Utils
open All_locks_def

let test (module Worker : WORK) ndomain nlock rounds =
  let orch = Orchestrator.init ~total_domains:ndomain ~rounds in
  let counter = ref 0 in
  let lock = Worker.create ndomain in

  let domains =
    List.init ndomain (fun _ ->
        Domain.spawn (fun () ->
            Orchestrator.worker orch (fun () ->
                for _ = 0 to nlock - 1 do
                  Worker.incr counter lock
                done)))
  in

  let res = Orchestrator.run orch in
  List.iter Domain.join domains;
  res

let main ?(gnuplot = false) () =
  let nlock, nround = (10_000, 100) in

  let res =
    List.map
      (fun (name, (module Worker : WORK)) ->
        ( name,
          (List.map (fun ndomain ->
               let data = test (module Worker) ndomain nlock nround in
               Gc.major ();
               let mean = mean ~cut_minmax:(nround / 20) data in
               (ndomain, mean)))
            [ 1; 2; 4; 6 ] ))
      All_locks_def.all_locks
  in
  print ~gnuplot res

let _ = main ~gnuplot:true ()

(*

dune exec ./bench/test_taslock.exe > bench.data

gnuplot -p -e 'plot for [col=2:4] "bench.data" using 1:col with lines title columnheader'

     *)
