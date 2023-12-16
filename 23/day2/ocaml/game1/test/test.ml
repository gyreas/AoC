open OUnit2
open Game1
open List
open Printf

(* TODO: Write better tests than this: inline, expect, property etc *)

(* a pretty printer for tuples because I can't look for one *)
let ppx_tuple tup =
  let x, y, z = tup in
  sprintf "(%d, %d, %d)" x y z
;;

(* a pretty printer for lists because I can't look for one *)
let ppx_list lst =
  let len = length lst in
  let rec out buf i l =
    match l with
    | [] -> buf ^ " ]"
    | e :: t ->
      let sl = ppx_tuple e in
      if i < len - 1 then out (buf ^ sl ^ ", ") (i + 1) t else out (buf ^ sl) (i + 1) t
  in
  out "[ " 0 lst
;;

(* a pretty printer for game type *)
let ppx_game g = sprintf "{ id = %d; subsets = %s }\n" g.id (ppx_list g.subsets)

let tests =
  "test suite for game1"
  >::: [ ("not_fooled_by_flimsy_whitespaces"
          >:: fun _ ->
          assert_equal
            1
            (get_game_id
               (get_game "      Game 1      : 3      blue   , 4 red   ,         2 green"))
         )
       ; ("can_recognize_game_id"
          >:: fun _ ->
          assert_equal
            1
            (get_game_id (get_game "Game 1: 3 blue, 4 red, 2 green"))
            ~printer:string_of_int)
       ; ("can_store_color_by_value"
          >:: fun _ ->
          assert_equal (12, 9, 5) (store_by_color ~color:"red" ~value:9 (3, 9, 5)))
       ; ("can_get_game_subset"
          >:: fun _ ->
          assert_equal
            ({ id = 1235; subsets = [ 4, 0, 3; 1, 2, 6; 0, 2, 0 ] } : game)
            (get_game "Game 1235: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
            ~printer:ppx_game)
       ; ("can_determine_possible_games"
          >:: fun _ ->
          let _ =
            assert (
              (* Not possible *)
              not
                (game_is_possible
                   (get_game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 22 green")))
          in
          let _ =
            assert (
              (* Possible *)
              game_is_possible
                (get_game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
          in
          let _ =
            assert (
              (* Possible *)
              game_is_possible
                (get_game
                   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"))
          in
          let _ =
            assert (
              (* Not possible *)
              not
                (game_is_possible
                   (get_game
                      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 \
                       green, 1 red")))
          in
          let _ =
            assert (
              (* Not possible *)
              not
                (game_is_possible
                   (get_game
                      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, \
                       14 red")))
          in
          let _ =
            assert (
              (* Possible *)
              game_is_possible
                (get_game "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))
          in
          ())
       ]
;;

(*
   Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green                    =>  possible
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue          =>  possible
   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red  =>  not possible
   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red  =>  not possible
   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green                    =>  possible

   Possible if and only if: Sum(red) <= 12 AND Sum(green) <= 13 AND Sum(blue) <= 14
*)

let _ = run_test_tt_main tests
