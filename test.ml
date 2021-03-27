(* TODO: Write tests. *)
open OUnit2
open Player
open Board
open Game
open Input

(* General Helper Functions *)

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

(* Board Module Tests *)
let board_tests = []

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