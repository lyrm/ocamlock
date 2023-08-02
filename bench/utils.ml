let mean ?(cut_minmax = 0) data =
  let data = List.sort compare data in
  let length = List.length data in
  if cut_minmax * 2 > length then failwith "Not enougth data";
  let data =
    List.filteri (fun i _ -> i >= cut_minmax && i <= length - cut_minmax) data
  in
  let sum = List.fold_left (fun curr_sum b -> curr_sum +. b) 0. data in
  let n = Int.to_float (List.length data) in
  sum /. n

let print ~gnuplot res =
  if gnuplot then
    let n_domains = List.hd res |> snd |> List.map fst in
    let nlines = List.length n_domains in
    let names, lines =
      List.fold_left
        (fun (names, acc) (name, res) ->
          ( names ^ name ^ "\t",
            List.map2
              (fun tmp (_, mean) -> tmp ^ "\t" ^ Float.to_string mean)
              acc res ))
        ("ndomains\t", List.init nlines (fun _ -> ""))
        res
    in
    let lines =
      names
      :: List.map2 (fun line nd -> Int.to_string nd ^ line) lines n_domains
    in
    List.iter (Format.printf "%s@.") lines
  else
    List.iter
      (fun (name, means) ->
        Format.printf "%s : @." name;
        List.iter
          (fun (ndomain, mean) ->
            Format.printf "  for ndomain= %d : %f.6@." ndomain mean)
          means)
      res
