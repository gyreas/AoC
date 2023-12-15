open OUnit2
open Game1
open Printf

let ppx_tuple tup =
  let x, y, z = tup in
  sprintf "(%d, %d, %d)" x y z
;;

let ppx_game g = sprintf "{ id = %d; colors = %s }\n" g.id (ppx_tuple g.colors)

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
          >:: fun _ -> assert_equal (12, 9, 5) (store_by_color "red" 9 (3, 9, 5)))
       ; ("can_get_game"
          >:: fun _ ->
          assert_equal
            { id = 1235; colors = 5, 4, 9 }
            (get_game "Game 1235: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
            ~printer:ppx_game)
       ; ("can_determine_possible_games"
          >:: fun _ ->
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
          assert (
            (* Possible *)
            game_is_possible
              (get_game "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")))
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
