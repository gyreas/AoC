open String;;

let isdigit c = match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' 
      -> true
  | _ -> false 
in
let isempty s = equal empty s 

in
(* The main work horse.
  (mutable) How it works.
  1. go through the string by chars
  2. if a char is a digit, use it as the first digit, d1 if it's not already set; use d2 otherwise.
  3. if d2 is not set, ie, a digit wasn't encountered, use the d1 for d2 as well, duplicating it.
  4. convert the concatenation of d1 to d2 (in that order), to an integer
  5. return
*)
let twodigits s =
  if isempty s then 0 
  else
    let d1 = ref "" in
    let d2 = ref "" in
    for i=0 to length(s) - 1 do
      let ch = s.[i] in
      if (isdigit ch) then
        if (isempty !d1) 
          then d1 := make 1 ch
        else   d2 := make 1 ch
    done;

    if (isempty !d2) then d2 := !d1
    else d2 := !d2;

  int_of_string (!d1 ^ !d2)

in
let parse_sum acc line = (twodigits line) + acc 

in
(* [fold_lines f acc ch] applies a folding operation on the lines in [ch]
    returning the result. Created because this version of OCaml doesn't have
    In_channel.fold_lines (added in v5.1).
*)
let rec fold_lines f acc ch = 
  match (In_channel.input_line ch) with
    | Some(line) -> fold_lines f (f acc line) ch;
    | None -> acc

in 
let file = open_in Sys.argv.(1) in
let res = fold_lines parse_sum 0 file in
  Printf.printf "Sum: %d\n" res;
