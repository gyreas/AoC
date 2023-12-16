open Game1
open Printf

let main _ =
  let sum_id acc line =
    (* get the game *)
    let g = get_game line in
    (* sum if possible *)
    let sum = if game_is_possible g then get_game_id g else 0 in
    sum + acc
  in
  let rec fold_lines f acc ch =
    match In_channel.input_line ch with
    | Some line -> fold_lines f (f acc line) ch
    | None -> acc
  in
  let file = open_in Sys.argv.(1) in
  let res = fold_lines sum_id 0 file in
  printf "Sum: %d\n" res
;;

main ()
