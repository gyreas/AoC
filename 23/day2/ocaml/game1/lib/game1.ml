open String

type game =
  { id : int
  ; colors : int (*r*) * int (*g*) * int (*b*)
  }

(* [store_by_color color value [tuple]] Stores the given value with
   respect to the specified color, and return a tuple reflecting that.
*)
let store_by_color color value tup =
  match tup with
  | r, g, b ->
    (match color with
     | "red" -> r + value, g, b
     | "green" -> r, g + value, b
     | "blue" -> r, g, b + value
     | _ -> r, g, b)
;;

let parse_colors str =
  let rec trav buf ch s tp =
    if String.length s = 0
    then (
      (* process the last color *)
      let buf = trim buf in
      let val_color = split_on_char ' ' buf in
      let value = List.nth val_color 0 in
      let color = List.nth val_color 1 in
      store_by_color color (int_of_string value) tp)
    else (
      let l = length s - 1 in
      let substr = String.sub s 1 l in
      let next_char = if length s = 1 then "" else make 1 s.[1] in
      if ch = ";" || ch = ","
      then (
        let buf = trim buf in
        let val_color = split_on_char ' ' buf in
        let value = List.nth val_color 0 in
        let color = List.nth val_color 1 in
        let tp = store_by_color color (int_of_string value) tp in
        trav "" next_char substr tp)
      else (
        let ch = make 1 s.[0] in
        trav (buf ^ ch) next_char substr tp))
  in
  trav "" "" (str ^ " ") (0, 0, 0)
;;

(* TODO: use line [split_on_char] and some List functions for this *)
let parse_game_id line =
  let is_digit ch =
    let ch = ch.[0] in
    match ch with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false
  in
  let rec trav buf ch str =
    if ch = ":"
    then (
      let l = length str - 1 in
      let str = sub str 1 l in
      buf, str)
    else (
      let ch = make 1 str.[0] in
      let l = length str - 1 in
      let substr = sub str 1 l in
      let next_char = if l = 0 then "" else make 1 substr.[0] in
      if length ch != 0 && is_digit ch
      then trav (buf ^ ch) next_char substr
      else trav buf next_char substr)
  in
  trav "" "" line
;;

let get_game line =
  let game_id, rest = parse_game_id line in
  let colors = parse_colors rest in
  { id = int_of_string game_id; colors }
;;

let get_game_id g = g.id

let game_is_possible g =
  let r, g, b = g.colors in
  r <= 12 && g <= 13 && b <= 14
;;
