(* TODO: Write tests. *)
open OUnit2
open Player
open Board
open Game
open Input

(* General Helper Functions *)
let pp_string s = "\"" ^ s ^ "\""

(* Any Player Module Testing Helper Functions/Variables *)
let get_balance_test name player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_balance player)
    ~printer:string_of_int

let get_location_test name player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_location player)
    ~printer:string_of_int

let passes_go_test name roll player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (passes_go roll player)
    ~printer:string_of_bool

let player1 = make_player "Kira"

let player2 = make_player "player2"

let () = update_balance player2 100

let random_roll = roll ()

let random_roll_sum = fst random_roll + snd random_roll

let () = move_player random_roll player2

let player3 = make_player "player3"

let () = move_player (38, 0) player3

let roll3 = (1, 1)

let () = move_player roll3 player3

let player4 = make_player "player4"

let () = move_player (34, 0) player4

let () = update_balance player4 (-500)

let roll4 = (6, 3)

let player5 = make_player "player5"

(* Player Module Tests *)
let player_tests =
  [
    ( "get_player_id for player 1" >:: fun _ ->
      assert_equal (get_player_id player1) "Kira" );
    ( "get_property_name_list for player 1 start of game" >:: fun _ ->
      assert_equal (get_ownable_name_list player1) [] );
    get_balance_test "Player 1 starting balance" player1 1500;
    get_location_test "Player 1 starting location" player1 0;
    get_balance_test "Player 2 balance after positive transaction"
      player2 1600;
    get_location_test "Player 2 location after one roll" player2
      random_roll_sum;
    passes_go_test "Player 2 will not pass go with 1 & 1" (1, 1) player2
      false;
    get_balance_test "Player 3 balance after go passed" player3 1700;
    get_location_test "Player 3 location after hitting go" player3 0;
    get_balance_test "Player 4 balance after negative transaction"
      player4 1000;
    passes_go_test "Player 4 will pass go with roll4" roll4 player4 true;
    ( "Prevent negative balance" >:: fun _ ->
      assert_raises BalanceBelowZero (fun () ->
          update_balance player5 (-1501)) );
  ]

(* Any Board Module Testing Helper Functions/Variables *)
let space_from_location_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output (space_from_location board loc)

let space_name_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output (space_name board loc) ~printer:pp_string

let start_space_test name board expected_output =
  name >:: fun _ ->
  assert_equal expected_output (start_space board) ~printer:pp_string

let test_board = Yojson.Basic.from_file "board.json" |> init_board

let mediterranean_avenue_space =
  Property
    {
      name = "Mediterranean Avenue";
      price = 60;
      house_price = 50;
      color = "#955438";
      rent = [| 2; 10; 30; 90; 160; 250 |];
    }

let income_tax_space = Tax { name = "Income Tax"; cost = 200 }

(* Board Module Tests *)
let board_tests =
  [
    space_from_location_test "Mediterranean Avenue Property" test_board
      1 mediterranean_avenue_space;
    space_from_location_test "Income Tax" test_board 4 income_tax_space;
    space_from_location_test "Chance" test_board 7 Chance;
    space_name_test "0 is Go" test_board 0 "Go";
    space_name_test "10 is Jail" test_board 10 "Jail";
    space_name_test "Last space is Boardwalk" test_board 39 "Boardwalk";
    start_space_test "Start on Go" test_board "Go";
  ]

(* Any Game Module Testing Helper Functions/Variables *)

let test_game =
  init_game test_board [| player1; player2; player3; player4 |]

let test_game_two =
  init_game test_board [| player1; player2; player3; player4 |]

let test_game_three = init_game test_board [| player1; player2 |]

let () = next_player test_game_three (* curr player is now player2 *)

let board = get_board test_game

let cur_player = current_player test_game

let current_player_test name game expected_output =
  name >:: fun _ ->
  assert_equal expected_output (current_player game)
    ~printer:get_player_id

let next_player_help game =
  next_player game;
  game

let update_player_help player game =
  update_player player game;
  game

(* Game Module Tests *)
let game_tests =
  [
    space_from_location_test "Income Tax" board 4 income_tax_space;
    space_name_test "10 is Jail" board 10 "Jail";
    start_space_test "Start on Go" board "Go";
    ( "get_all_players in game" >:: fun _ ->
      assert_equal
        (get_all_players test_game)
        [| player1; player2; player3; player4 |] );
    current_player_test "Player 1 starts the game" test_game player1;
    current_player_test "Player 2 moves after Player 1"
      (next_player_help test_game_two)
      player2;
    current_player_test "Player 1 moves after Player 2 with 2 players"
      (next_player_help test_game_three)
      player1;
  ]

(* Any Input Module Testing Helper Functions/Variables *)

(* Input Module Tests *)
let input_tests = []

(* Test Suite *)
let suite =
  "test suite for Monopoly"
  >::: List.flatten
         [ player_tests; board_tests; game_tests; input_tests ]

let _ = run_test_tt_main suite
