(* TODO: Write tests. *)
open OUnit2
open Player
open Board
open Game
open Input

(* General Helper Functions *)

(* Any Player Module Testing Helper Functions/Variables *)

(* Player Module Tests *)
let player_tests = []

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