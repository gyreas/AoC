open List
open Printf
open Str
open String;;

(* TODO: Use an Array and match by index *)
let digitword = 
  [
    ("one"  , '1');
    ("two"  , '2');
    ("three", '3');
    ("four" , '4');
    ("five" , '5');
    ("six"  , '6');
    ("seven", '7');
    ("eight", '8');
    ("nine" , '9');
  ]

let to_match = {|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|}

let isdigit c = match c with
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' 
      -> true
  | _ -> false 

let isempty s = equal empty s 

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
    (* This was a for loop *)
    let re = regexp to_match in
    let rec trav d1 d2 s = match s with 
      | "" ->
          let d2 = if (isempty d2) then d1 else d2 in
          let d12 = d1 ^ d2
            in int_of_string (if (isempty d12) then "0" else d12)
      | subs ->  
          let ch = subs.[0] in
          let len = length subs in
          (* If we found a digit 0-9 *)

          if (isdigit ch) then begin
            if (isempty d1) then
            else 
          end

          (* Process it by word *)
          else
            if (string_match re subs 0) then 
              (* the digit word *)
              let digit = matched_string subs in
              let ch = assoc digit digitword in
              (* length of unmatched substring 
                 + 1 because we're accounting for string overlaps
              *)
              let l = len - length digit + 1 in
              (* move cursor forward to new position 
                 - 1 because we're considering string overlaps
              *)
              let e = match_end () - 1 in

              if (isempty d1) then
              else 
            else 

(* End of twodigits *)

let parse_sum acc line =  (twodigits line) + acc in

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
    printf "Sum: %d\n" res
