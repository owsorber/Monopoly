(* TODO: Write tests. *)
open OUnit2
open Player
open Board
open Game
open Input

(* General Helper Functions *)
let pp_string s = "\"" ^ s ^ "\""

(* Any Player Module Testing Helper Functions/Variables *)
let player1 = make_player "Kira"
let player2 = make_player "player2";;
update_balance player2 100;;
let roll1 = roll;;
let roll1sum = (fst roll1) + (snd roll1);;
move_player player2 roll1;;
let player3 = make_player "player3";;
move_player player3 (41,1);;

(* Player Module Tests *)
let player_tests = [
   ("get_player_id for player 1" >:: fun _ -> assert_equal (get_player_id player1) "Kira");
   ("get_blance for player 1 start of game" >:: fun _ -> assert_equal (get_balance player1) 1500);
   ("get_location for player 1 start of game" >:: fun _ -> assert_equal (get_location player1) 0);
   ("get_property_name_list for player 1 start of game" >:: fun _ -> assert_equal (get_property_name_list player1) []);
   ("get_blance for player 2 after adding" >:: fun _ -> assert_equal (get_balance player2) 1600);
   ("get_location for player 2 after 1 move of roll1 spaces" >:: fun _ -> assert_equal (get_location player2) (roll1sum));
   ("get_balance for player 3 after passing go" >:: fun _ -> assert_equal (get_balance player3) 1700);
   ("get_location for player 3 after passing go" >:: fun _ -> assert_equal (get_location player3) 2);
]

(* Any Board Module Testing Helper Functions/Variables *)
let space_from_location_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (space_from_location board loc)

let space_name_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (space_name board loc)
    ~printer:pp_string

let start_space_test name board expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (start_space board)
    ~printer:pp_string

let test_board = Yojson.Basic.from_file "board.json" |> init_board
let mediterranean_avenue_space =
  Property {name = "Mediterranean Avenue"; price = 60; house_price = 50; 
            color = "#955438"; rent = [|2; 10; 30; 90; 160; 250|]}
let income_tax_space = Tax {name = "Income Tax"; cost = 200}

(* Board Module Tests *)
let board_tests = [
  space_from_location_test "Mediterranean Avenue Property" test_board 1 
    mediterranean_avenue_space;
  space_from_location_test "Income Tax" test_board 4 income_tax_space;
  space_from_location_test "Chance" test_board 7 Chance;
  space_name_test "0 is Go" test_board 0 "Go";
  space_name_test "10 is Jail" test_board 10 "Jail";
  space_name_test "Last space is Boardwalk" test_board 39 "Boardwalk";
  start_space_test "Start on Go" test_board "Go"
]

(* Any Game Module Testing Helper Functions/Variables *)

(* Game Module Tests *)
let game_tests = []

(* Any Input Module Testing Helper Functions/Variables *)

(* Input Module Tests *)
let input_tests = []


(* Test Suite *)
let suite =
  "test suite for Monopoly"
  >::: List.flatten [ player_tests; board_tests; game_tests; input_tests]

let _ = run_test_tt_main suite