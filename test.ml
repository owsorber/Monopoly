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

let space_color_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (color board (space_from_location board loc))

let test_board = Yojson.Basic.from_file "board.json" |> init_board

let mediterranean_avenue_space : Board.space =
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
    space_name_test "10 is Quarantine" test_board 10 "Quarantine";
    space_name_test "Last space is Boardwalk" test_board 39 "Boardwalk";
    start_space_test "Start on Go" test_board "Go";
    space_color_test "Color of Mediterranean Avenue" test_board 1
      "#955438";
    space_color_test "Color of Mediterranean Avenue" test_board 1
      "#955438";
    space_color_test "Color of Pennsylvania Avenue" test_board 34
      "#1fb25a";
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
  name >:: fun _ -> assert_equal expected_output (current_player game)

let get_all_players_test name game expected_output =
  name >:: fun _ -> assert_equal expected_output (get_all_players game)

let next_player_help game =
  next_player game;
  game

let get_free_parking_test name game expected_output =
  name >:: fun _ -> assert_equal expected_output (get_free_parking game)

let do_tax_test name game player spc expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( Game.do_tax game player spc;
      Player.get_balance player )

let get_rent_test name game location_index dice expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_rent game location_index dice)

let get_ownable_status_test name game spc expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_ownable_status game spc)

let get_ownable_price_test name board own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_ownable_price board own_name)

(* not tested*)
let get_ownable_info_test name game board own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_ownable_info game board own_name)

let get_houses_test name game own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_houses game own_name)

(* don't need to test; used in constructing board *)
let make_ownable_owned_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( make_ownable_owned game player own_name;
      owner game own_name )

(* don't need to test, used in construct board*)
let make_ownable_mortgaged_test
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( make_ownable_owned game player own_name;
      is_mortgaged game own_name && owner game own_name = Some player )

let make_ownable_mortgaged_exn name game player own_name expected_output
    =
  name >:: fun _ ->
  assert_raises MortgageFailure (fun () ->
      make_ownable_mortgaged game player own_name)

let all_mortgagable_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (all_mortgagable game player)

let can_add_house_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (can_add_house game player own_name)

let can_add_house_property_exn name game player own_name expected_output
    =
  name >:: fun _ ->
  assert_raises NotPropertyName (fun () ->
      can_add_house game player own_name)

let can_add_house_monopoly_exn name game player own_name expected_output
    =
  name >:: fun _ ->
  assert_raises (CannotAddHouse "No Monopoly") (fun () ->
      can_add_house game player own_name)

let can_add_house_available_exn
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_raises (CannotAddHouse "No Houses Available") (fun () ->
      can_add_house game player own_name)

let can_add_house_four_houses_exn
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_raises (CannotAddHouse "4 Houses on Property") (fun () ->
      can_add_house game player own_name)

let can_add_house_even_build_exn
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_raises (CannotAddHouse "Even Build") (fun () ->
      can_add_house game player own_name)

let can_add_house_mortgaged_exn
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_raises (CannotAddHouse "Mortgaged Property on Color")
    (fun () -> can_add_house game player own_name)

let house_price_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (house_price game player own_name)

let all_can_buy_house_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (all_can_buy_house game player)

let print_arr arr = Array.fold_left (fun x y -> x ^ y) "" arr

let all_can_buy_hotel_test name game player expected_output =
  name >:: fun _ ->
  assert_equal ~printer:print_arr expected_output
    (all_can_buy_hotel game player)

let hotel_price_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (house_price game player own_name)

(* assumes a call to add_house will always add an house, thus
   incrementing number of houses by one. Property-based test *)
let add_house_test name game own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (let num_houses = get_houses game own_name in
     let num_houses_available = get_houses_available game in
     add_house game own_name true;
     let upd_houses = get_houses game own_name in
     let upd_houses_available = get_houses_available game in
     upd_houses = num_houses + 1
     && upd_houses_available = num_houses_available - 1)

(* add_hotel should always result in a property having 5 houses *)
let add_hotel_test name game own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (let num_houses_available = get_houses_available game in
     let num_hotels_available = get_hotels_available game in
     add_house game own_name false;
     let upd_houses = get_houses game own_name in
     let upd_houses_available = get_houses_available game in
     let upd_hotels_available = get_hotels_available game in
     add_house game own_name true;
     upd_houses = 5
     && upd_houses_available = num_houses_available + 4
     && upd_hotels_available = num_hotels_available - 1)

let add_house_exn name game own_name adding_house expected_output =
  name >:: fun _ ->
  assert_raises NotPropertyName (fun () ->
      add_house game own_name adding_house)

let can_add_hotel_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (can_add_hotel game player own_name)

let can_add_hotel_property_exn name game player own_name expected_output
    =
  name >:: fun _ ->
  assert_raises NotPropertyName (fun () ->
      can_add_hotel game player own_name)

let can_add_hotel_monopoly_exn name game player own_name expected_output
    =
  name >:: fun _ ->
  assert_raises (CannotAddHotel "No Monopoly") (fun () ->
      can_add_hotel game player own_name)

let can_add_hotel_four_houses_exn
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_raises (CannotAddHotel "Does Not Own Four Houses") (fun () ->
      can_add_hotel game player own_name)

let can_add_hotel_already_own_exn
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_raises (CannotAddHotel "Already Owns Hotel on Property")
    (fun () -> can_add_hotel game player own_name)

let can_add_hotel_no_hotels_exn
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_raises (CannotAddHotel "No Hotels Available") (fun () ->
      can_add_hotel game player own_name)

let is_available_test name game own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (is_available game own_name)

let is_available_exn name game own_name expected_output =
  name >:: fun _ ->
  assert_raises NotOwnableName (fun () -> is_available game own_name)

let is_mortgaged_test name game own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (is_mortgaged game own_name)

let is_mortgaged_exn name game own_name expected_output =
  name >:: fun _ ->
  assert_raises NotOwnableName (fun () -> is_mortgaged game own_name)

let owner_test name game own_name expected_output =
  name >:: fun _ -> assert_equal expected_output (owner game own_name)

let owner_exn name game own_name expected_output =
  name >:: fun _ ->
  assert_raises NotOwnableName (fun () -> owner game own_name)

let has_monopoly_test name game player color expected_output =
  name >:: fun _ ->
  assert_equal expected_output (has_monopoly game player color)

let has_houses_on_color_test name game player color expected_output =
  name >:: fun _ ->
  assert_equal expected_output (has_houses_on_color game player color)

(* NOTE: Not testing landing_on_space *)

let p1 = make_player "p1"

let p2 = make_player "p2"

let p3 = make_player "p3"

let p4 = make_player "p4"

let game_one = init_game test_board [| p1; p2; p3; p4 |]

let () = update_balance p1 100000

let () = update_balance p4 10000

(* buys a list of ownables for a player*)
let rec buy_ownable_lst game board player name_lst =
  match name_lst with
  | [] -> ()
  | h :: t ->
      buy_ownable player h (get_ownable_price board h);
      make_ownable_owned game player h;
      buy_ownable_lst game board player t

(* adds [num] houses to property [name] *)
let rec add_num_houses game name adding_house num =
  match num with
  | 0 -> ()
  | n ->
      add_house game name adding_house;
      add_num_houses game name adding_house (n - 1)

(* adds a list of houses (with num houses) to properties. Ex: name_lst =
   [A;B;C;D] with num_lst [1;2;2;1] adds 1 house to A and D, and 2
   houses to B and C. *)
let rec add_num_houses_lst game name_lst adding_house num_lst =
  match name_lst with
  | [] -> ()
  | h :: t -> (
      match num_lst with
      | [] -> ()
      | n :: t1 ->
          add_num_houses game h adding_house n;
          add_num_houses_lst game t adding_house t1 )

let () =
  buy_ownable_lst game_one test_board p1
    [
      "Connecticut Avenue";
      "Vermont Avenue";
      "Oriental Avenue";
      "Reading Railroad";
    ]

let () =
  add_num_houses_lst game_one
    [ "Connecticut Avenue"; "Vermont Avenue"; "Oriental Avenue" ]
    true [ 4; 3; 4 ]

let () = make_ownable_mortgaged game_one p1 "Reading Railroad"

let () =
  buy_ownable_lst game_one test_board p2
    [ "Electric Company"; "Water Works"; "Virginia Avenue" ]

let () =
  buy_ownable_lst game_one test_board p3
    [
      "Shortline";
      "B. & O. Railroad";
      "Kentucky Avenue";
      "Indiana Avenue";
      "Illinois Avenue";
    ]

let () =
  add_num_houses_lst game_one
    [ "Kentucky Avenue"; "Indiana Avenue"; "Illinois Avenue" ]
    true [ 4; 4; 4 ]

let () = add_house game_one "Kentucky Avenue" false

let () = add_house game_one "Illinois Avenue" false

let () =
  buy_ownable_lst game_one test_board p4
    [
      "Baltic Avenue";
      "Park Place";
      "Boardwalk";
      "Pennsylvania Avenue";
      "North Carolina Avenue";
      "Pacific Avenue";
      "Marvin Gardens";
      "Ventnor Avenue";
      "Atlantic Avenue";
    ]

let () = make_ownable_mortgaged game_one p4 "Pennsylvania Avenue"

let () =
  add_num_houses_lst game_one
    [
      "Park Place";
      "Boardwalk";
      "Marvin Gardens";
      "Ventnor Avenue";
      "Atlantic Avenue";
    ]
    true [ 2; 0; 5; 4; 4 ]

(* Game Module Tests *)
let game_tests =
  [
    space_from_location_test "Income Tax" board 4 income_tax_space;
    space_name_test "10 is Quarantine" board 10 "Quarantine";
    start_space_test "Start on Go" board "Go";
    get_all_players_test "get all four players in test game" test_game
      [| player1; player2; player3; player4 |];
    current_player_test "Player 1 starts the game" test_game player1;
    current_player_test "Player 2 moves after Player 1"
      (next_player_help test_game_two)
      player2;
    current_player_test "Player 1 moves after Player 2 with 2 players"
      (next_player_help test_game_three)
      player1;
    get_free_parking_test "A game starts with $0 free parking" game_one
      0;
    get_rent_test
      "Landing on owned Connecticut Avenue with 4 houses costs $8"
      game_one 9 (1, 2) 450;
    get_rent_test "Landing on a mortgaged railroad costs $0" game_one 5
      (2, 3) 0;
    get_rent_test
      "Landing on a railroad where the owner owns two costs $50"
      game_one 25 (2, 3) 50;
    get_rent_test
      "Landing on Park Place with 2 houses and owner monopoly costs \
       $500"
      game_one 37 (1, 2) 500;
    get_rent_test
      "Landing on Boardwalk with 0 houses and owner monopoly costs $100"
      game_one 39 (1, 2) 100;
    get_rent_test
      "Landing on a utility when both are owned by owner costs 10 * \
       dice roll"
      game_one 12 (5, 6) 110;
    get_ownable_status_test
      "Getting the status of an owned property with 4 houses" game_one
      (Property
         {
           name = "Connecticut Avenue";
           price = 120;
           house_price = 50;
           color = "#aae0fa";
           rent = [| 8; 40; 100; 300; 450; 600 |];
         })
      (Some (Property (P_Owned (p1, 4))));
    get_ownable_status_test
      "Getting the status of a property with no owner" game_one
      (Property
         {
           name = "St. Charles Place";
           price = 140;
           house_price = 100;
           color = "#d93a96";
           rent = [| 10; 50; 150; 450; 625; 750 |];
         })
      (Some (Property P_Available));
    get_ownable_status_test
      "Getting the status of a non ownable property" game_one Chance
      None;
    get_ownable_price_test "Price to buy Park Place is 350" test_board
      "Park Place" 350;
    get_ownable_price_test "Price to buy Shortline RR is 200" test_board
      "Shortline" 200;
    get_houses_test "No houses on Virginia Avenue" game_one
      "Virginia Avenue" 0;
    get_houses_test "Four houses on Oriental Avenue" game_one
      "Oriental Avenue" 4;
    make_ownable_mortgaged_exn "p1 does not own Shortline" game_one p1
      "Shortline" "exn";
    all_mortgagable_test "p1 cannot mortgage any properties" game_one p1
      [||];
    all_mortgagable_test
      "p3 can mortgage Shortline and B. & O. Railroad" game_one p3
      [| "B. & O. Railroad"; "Shortline" |];
    can_add_house_test "p1 can add a house to Vermont" game_one p1
      "Vermont Avenue" true;
    can_add_house_monopoly_exn "p3 does not have a monopoly on brown"
      game_one p3 "Mediterranean Avenue" "exn";
    can_add_house_four_houses_exn
      "Connecticut Avenue already has four houses" game_one p1
      "Connecticut Avenue" "exn";
    can_add_house_even_build_exn
      "Cannot build a house on Park Place due to even build rule"
      game_one p4 "Park Place" "exn";
    can_add_house_mortgaged_exn "Green property mortgaged" game_one p4
      "Pacific Avenue" "exn";
    house_price_test "A hotel on Connecticut Avenue costs $50" game_one
      p1 "Connecticut Avenue" 50;
    all_can_buy_house_test "p1 can put a house on Vermont Avenue"
      game_one p1 [| "Vermont Avenue" |];
    all_can_buy_hotel_test "p4 can put a hotel on Ventnor and Atlantic"
      game_one p4
      [| "Atlantic Avenue"; "Ventnor Avenue" |];
    can_add_hotel_test "" game_one p4 "Ventnor Avenue" true;
    can_add_hotel_property_exn "Water Works is not a property" game_one
      p4 "Water Works" "exn";
    can_add_hotel_monopoly_exn "p4 does not have a monopoly on orange"
      game_one p4 "New York Avenue" "exn";
    can_add_hotel_four_houses_exn
      "p1 does not have a full monopoly on light blue" game_one p1
      "Vermont Avenue" "exn";
    can_add_hotel_already_own_exn "Kentucky Avenue already has a hotel"
      game_one p3 "Kentucky Avenue" "exn";
    is_available_test "Kentucky Avenue is not available" game_one
      "Kentucky Avenue" false;
    is_available_exn "Chance is not ownable" game_one "Chance" false;
    is_mortgaged_test "Pennsylvania Avenue is mortgaged" game_one
      "Pennsylvania Avenue" true;
    is_mortgaged_exn "Chance is not ownable" game_one "Chance" false;
    owner_test "Shortline owned by p3" game_one "Shortline" (Some p3);
    owner_exn "Chance is not ownable" game_one "Chance" None;
    has_monopoly_test "p1 has a monopoly on #aae0fa" game_one p1
      "#aae0fa" true;
    has_houses_on_color_test "p1 has houses on #aae0fa" game_one p1
      "#aae0fa" true;
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
