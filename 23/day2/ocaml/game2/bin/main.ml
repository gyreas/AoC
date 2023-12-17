open Game2
open Printf

let main _ =
  let sum_of_power_of_sets acc line =
    (* get the game *)
    let g = get_game line in
    match g.subsets with
    | [] -> 0
    | _ ->
      let r, g, b = minimum_colors g in
      let prod = r * g * b in
      prod + acc
  in
  let rec fold_lines f acc ch =
    match In_channel.input_line ch with
    | Some line -> fold_lines f (f acc line) ch
    | None -> acc
  in
  let file = open_in Sys.argv.(1) in
  let res = fold_lines sum_of_power_of_sets 0 file in
  printf "Sum: %d\n" res
;;

main ()
