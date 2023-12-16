open String

type subset = int (* r *) * int (* g *) * int (* b *)

type game =
  { id : int
  ; subsets : subset list
  }

let empty_char = '\000'
(* Utility functions *)

(* [store_by_color color value [tuple]] Stores the given value with
   respect to the specified color, and return a tuple reflecting that.
*)
let store_by_color ~color ~value tup =
  match tup with
  | r, g, b ->
    (match color with
     | "red" -> r + value, g, b
     | "green" -> r, g + value, b
     | "blue" -> r, g, b + value
     | _ -> r, g, b)
;;

(*
   [parse_set line] parses the given line of string returning a game subset.
*)
let parse_set line : subset * string =
  (*
     [read_to_char str chr] is a utility function for reading upto a
     given char. It return a tuple whose first element is the substring from
     the start of [str] to just before [chr], and the second element is
     everything after [chr]. So, [chr] is omitted.
  *)
  let read_to_char (str : string) ~(chr : char) : string * string =
    let rec consume ~w ~ch s =
      if ch = chr
      then (
        let s = sub s 1 @@ (length s - 1) in
        trim w, s)
      else if ch = empty_char && s = ""
      then trim w, s
      else (
        let ch = make 1 s.[0] in
        let l = length s in
        let rem = sub s 1 (l - 1) in
        let nchar = if rem = "" then empty_char else rem.[0] in
        consume ~w:(w ^ ch) ~ch:nchar rem)
    in
    consume str ~w:"" ~ch:empty_char
  in
  (*
     [next_color str] returns the next color in [str] without checking
     for validity
  *)
  let next_color s =
    let rec consume ~w ~ch s =
      if ch = ';' || ch = ','
      then (
        let s = sub s 1 @@ (length s - 1) in
        trim w, s)
      else if ch = empty_char && s = ""
      then trim w, s
      else (
        let ch = make 1 s.[0] in
        let l = length s in
        let rem = sub s 1 (l - 1) in
        let nchar = if rem = "" then empty_char else rem.[0] in
        consume ~w:(w ^ ch) ~ch:nchar rem)
    in
    consume s ~w:"" ~ch:empty_char
  in
  (*
     [add_next_color str] Read the next color and return a tuple with the
     value of the color added *)
  let add_next_color s tp : (int * int * int) * string =
    let val_color, substr = next_color s in
    let val_color = split_on_char ' ' val_color in
    let value = int_of_string (List.nth val_color 0) in
    let color = trim (List.nth val_color 1) in
    let tp = store_by_color ~color ~value tp in
    tp, substr
  in
  let read_to_semicolon str = read_to_char ~chr:';' str in
  (*
     [fold_add_ tup str] is a speciliazed function recursively reads a color
     from [str] and add its value to [tup]. It's meant to consume until a semicolon
     is encountered.
  *)
  let rec fold_add_ tup str =
    match str with
    | "" -> tup
    | _ ->
      let tup, rest = add_next_color str tup in
      fold_add_ tup rest
  in
  let set_str, other_sets = read_to_semicolon line in
  let set_tup = fold_add_ (0, 0, 0) set_str in
  set_tup, other_sets
;;

(*
   [game_is_possible g] determines if a game is possible.
   A game is possible if all subsets are physically less than or equal
   to (12, 13, 14).
*)
let game_is_possible (g : game) : bool =
  let is_subset_possible ss =
    let r, g, b = ss in
    r <= 12 && g <= 13 && b <= 14
  in
  List.for_all is_subset_possible g.subsets
;;

(* TODO: use line [split_on_char] and some List functions for this *)
let parse_game_id line =
  let is_digit ch =
    match ch with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false
  in
  let rec trav buf ch str =
    if ch = ':'
    then (
      let l = length str - 1 in
      let str = sub str 1 l in
      buf, str)
    else (
      let ch = str.[0] in
      let l = length str - 1 in
      let substr = sub str 1 l in
      let next_char = if l = 0 then empty_char else substr.[0] in
      if ch != empty_char && is_digit ch
      then trav (buf ^ (make 1 ch)) next_char substr
      else trav buf next_char substr)
  in
  trav "" empty_char line
;;

(* [get_game_subsets line] reads all the subsets of a given game as a list *)
let get_game_subsets (line : string) : subset list =
  let rec get_subset acc str =
    if str = ""
    then acc
    else (
      let set, rest = parse_set str in
      get_subset (set :: acc) rest)
  in
  List.rev (get_subset [] line)
;;

let get_game line : game =
  let game_id, rest = parse_game_id line in
  let subsets = get_game_subsets rest in
  { id = int_of_string game_id; subsets }
;;

let get_game_id g = g.id
let get_subset_n g n = List.nth g.subsets n
