(* TODO: Write tests. *)
open OUnit2
open Player
open Board
open Game
open Input
open Cards
open Stockmarket

(* General Helper Functions *)
let pp_string s = "\"" ^ s ^ "\""

let print_arr arr = Array.fold_left (fun x y -> x ^ y) "" arr

(* buys a list of ownables for a player*)
let buy_ownable_lst game board player name_lst =
  let rec modify_property_lst names =
    match names with
    | [] -> ()
    | h :: t ->
        buy_ownable player h (get_ownable_price board h);
        modify_property_lst t
  in
  modify_property_lst name_lst;
  make_own_lst_owned game player name_lst

(* adds [num] houses to property [name] *)
let rec add_num_houses game name num =
  match num with
  | 0 -> ()
  | n ->
      if n = 5 then add_house game name false
      else add_house game name true;
      add_num_houses game name (n - 1)

(* adds a list of houses (with num houses) to properties. Ex: name_lst =
   [A;B;C;D] with num_lst [1;2;2;1] adds 1 house to A and D, and 2
   houses to B and C. *)
let rec add_num_houses_lst game name_lst num_lst =
  match name_lst with
  | [] -> ()
  | h :: t -> (
      match num_lst with
      | [] -> ()
      | n :: t1 ->
          add_num_houses game h n;
          add_num_houses_lst game t t1 )

(* Any Player Module Testing Helper Functions/Variables *)

let test_board = Yojson.Basic.from_file "board.json" |> init_board

let get_player_id_test name player expected_output =
  name >:: fun _ -> assert_equal expected_output (get_player_id player)

let get_ownable_name_list_test name player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_ownable_name_list player)

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

let quarantine_test name player expected_output =
  name >:: fun _ -> assert_equal expected_output (quarantine player)

let update_below_zero_exn name player change expected_output =
  name >:: fun _ ->
  assert_raises BalanceBelowZero (fun () ->
      update_balance player change)

let projected_space_test name roll player board expected_output =
  name >:: fun _ ->
  assert_equal expected_output (projected_space roll player board)

let pay_test name p1 p2 amt =
  name >:: fun _ ->
  let p1_balance = get_balance p1 in
  let p2_balance = get_balance p2 in
  pay p1 p2 amt;
  assert (
    p1_balance - amt = get_balance p1
    && p2_balance + amt = get_balance p2 )

let swap_properties_test name p1 p2 swap expected1 expected2 =
  name >:: fun _ ->
  swap_properties p1 p2 swap;
  assert (
    get_ownable_name_list p1 = expected1
    && get_ownable_name_list p2 = expected2 )

let get_stocks_test name player expected_output =
  name >:: fun _ -> assert_equal expected_output (get_stocks player)

let sell_stocks_shares_exn name player stock num cost expected_output =
  name >:: fun _ ->
  assert_raises NotEnoughShares (fun () ->
      sell_stocks player stock num cost)

let player1 = make_player "Kira"

let () = buy_stocks player1 "Amazon" 5 100

let () = sell_stocks player1 "Amazon" 2 100

let player2 = make_player "player2"

let () = update_balance player2 100

let random_roll = roll ()

let random_roll_sum = sums random_roll

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

let () = go_to_quarantine_status player5

let () = decrement_day_quarantine player5

let player6 = make_player "quarantine"

let () = go_to_quarantine_status player6

let () = leave_quarantine player6

let () = move_player_to player6 15 true

let test_game_player = init_game test_board [| player5; player6 |]

let () =
  buy_ownable_lst test_game_player test_board player5
    [ "Mediterranean Avenue"; "Baltic Avenue"; "Water Works" ]

let () =
  buy_ownable_lst test_game_player test_board player6
    [ "Connecticut Avenue"; "St. Charles Place"; "Virginia Avenue" ]

(* Player Module Tests *)
let player_tests =
  [
    get_player_id_test "Player 1's id is Kira" player1 "Kira";
    get_ownable_name_list_test
      "Player 1 owns nothing at the start of a game" player1 [];
    get_balance_test "Player 1 starting balance" player1 1500;
    get_location_test "Player 1 starting location" player1 0;
    get_balance_test "Player 2 balance after positive transaction"
      player2 1600;
    get_location_test "Player 2 location after one roll" player2
      random_roll_sum;
    get_stocks_test
      "Player 1 has 3 stocks of Amazon after buying/selling" player1
      [| ("Amazon", 3) |];
    sell_stocks_shares_exn "Player 1 cannot sell 4 Amazon stocks"
      player1 "Amazon" 4 100 [||];
    passes_go_test "Player 2 will not pass go with 1 & 1" (1, 1) player2
      false;
    get_balance_test "Player 3 balance after go passed" player3 1700;
    get_location_test "Player 3 location after hitting go" player3 0;
    get_balance_test "Player 4 balance after negative transaction"
      player4 1000;
    passes_go_test "Player 4 will pass go with roll4" roll4 player4 true;
    update_below_zero_exn "Negative Balance is not allowed" player5
      (-99999) "";
    quarantine_test "Player 6 entered quarantine and immediately left"
      player6 Out;
    quarantine_test "Player 2 is not in quarantine" player2 Out;
    quarantine_test "Player 5 has been in quarantine for one day"
      player5 (In 2);
    pay_test "Player 1 pays Player 2 $200" player1 player2 200;
    projected_space_test
      "After rolling a 5, player1 is projected to be on Reading \
       Railroad"
      (2, 3) player1 test_board "Reading Railroad";
    swap_properties_test
      "Swaps Water Works and Baltic Avenue in player lists" player5
      player6
      [ "Baltic Avenue"; "Water Works" ]
      [ "Mediterranean Avenue" ]
      [
        "Water Works";
        "Baltic Avenue";
        "Virginia Avenue";
        "St. Charles Place";
        "Connecticut Avenue";
      ];
    get_location_test "Player 6 directly moved to location 15" player6
      15;
  ]

(* Any Board Module Testing Helper Functions/Variables *)
let space_from_location_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output (space_from_location board loc)

let space_name_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output (space_name board loc) ~printer:pp_string

let space_from_space_name_test name board loc_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (space_from_space_name board loc_name)

let length_test name board expected_output =
  name >:: fun _ -> assert_equal expected_output (length board)

let is_ownable_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (is_ownable board (space_from_location board loc))

let num_of_color_test name board col expected_output =
  name >:: fun _ ->
  assert_equal expected_output (num_of_color board col)

let num_of_color_exn name board col expected_output =
  name >:: fun _ ->
  assert_raises (BoardDoesNotHaveColor col) (fun () ->
      num_of_color board col)

let location_from_space_name_test name board loc_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (location_from_space_name board loc_name)

let start_space_test name board expected_output =
  name >:: fun _ ->
  assert_equal expected_output (start_space board) ~printer:pp_string

let space_color_test name board loc expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (color board (space_from_location board loc))

let mediterranean_avenue_space : Board.space =
  Property
    {
      name = "Mediterranean Avenue";
      price = 60;
      house_price = 50;
      color = "Brown";
      rent = [| 2; 10; 30; 90; 160; 250 |];
    }

let income_tax_space = Tax { name = "Income Tax"; cost = 200 }

(* Board Module Tests *)
let board_tests =
  [
    space_from_location_test "Mediterranean Avenue Property" test_board
      1 mediterranean_avenue_space;
    length_test "Monopoly board has 40 spaces" test_board 40;
    space_from_location_test "Income Tax" test_board 4 income_tax_space;
    space_from_location_test "Chance" test_board 7 Chance;
    space_name_test "0 is Go" test_board 0 "Go";
    space_name_test "10 is Quarantine" test_board 10 "Quarantine";
    space_name_test "Last space is Boardwalk" test_board 39 "Boardwalk";
    start_space_test "Start on Go" test_board "Go";
    space_color_test "Color of Mediterranean Avenue" test_board 1
      "Brown";
    space_color_test "Color of Pennsylvania Avenue" test_board 34
      "Green";
    space_from_space_name_test "Income Tax space has name Income Tax"
      test_board "Income Tax" (Some income_tax_space);
    is_ownable_test "Mediterranean Avenue (location 1) is ownable"
      test_board 1 true;
    is_ownable_test "Community Chest (location 2) is not ownable"
      test_board 2 false;
    num_of_color_test "There are 2 brown properties on the test board"
      test_board "Brown" 2;
    num_of_color_exn "There are no sky blue properties on the board"
      test_board "Sky Blue" 0;
    location_from_space_name_test "Reading Railroad is on location 5"
      test_board "Reading Railroad" 5;
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

let get_board_test name game expected_output =
  name >:: fun _ -> assert_equal expected_output (get_board game)

let get_properties_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_properties game player)

let get_net_worth_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (get_net_worth game player)
    ~printer:string_of_int

let get_houses_available_test name game expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (get_houses_available game)
    ~printer:string_of_int

let get_hotels_available_test name game expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (get_hotels_available game)
    ~printer:string_of_int

let current_player_test name game expected_output =
  name >:: fun _ -> assert_equal expected_output (current_player game)

let get_all_players_test name game expected_output =
  name >:: fun _ -> assert_equal expected_output (get_all_players game)

let next_player_help game =
  next_player game;
  game

let get_free_parking_test name game expected_output =
  name >:: fun _ -> assert_equal expected_output (get_free_parking game)

let do_free_parking_test name game player =
  name >:: fun _ ->
  let init_bal = get_balance player in
  let free_parking_amt = do_free_parking game player in
  assert (
    get_free_parking game = 0
    && get_balance player = init_bal + free_parking_amt )

let do_tax_test name game player spc =
  name >:: fun _ ->
  let init_bal = get_balance player in
  Game.do_tax game player spc;
  assert (get_balance player + 200 = init_bal)

let get_rent_test name game location_index dice expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_rent game location_index dice)

let get_ownable_status_test name game spc expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_ownable_status game spc)

let get_ownable_price_test name board own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_ownable_price board own_name)

let get_houses_test name game own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_houses game own_name)

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

let can_trade_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (can_trade game player own_name)

let all_can_trade_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (all_can_trade game player)
    ~printer:print_arr

let can_sell_house_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (can_sell_house game player own_name)

let can_sell_house_even_build_exn
    name
    game
    player
    own_name
    expected_output =
  name >:: fun _ ->
  assert_raises (CannotSellHouse "Even Build") (fun () ->
      can_sell_house game player own_name)

let can_sell_house_hotel_exn name game player own_name expected_output =
  name >:: fun _ ->
  assert_raises (CannotSellHouse "Hotel on Property") (fun () ->
      can_sell_house game player own_name)

let can_sell_house_nohouse_exn name game player own_name expected_output
    =
  name >:: fun _ ->
  assert_raises (CannotSellHouse "No houses on Property") (fun () ->
      can_sell_house game player own_name)

let all_can_sell_house_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (all_can_sell_house game player)
    ~printer:print_arr

let can_sell_hotel_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (can_sell_hotel game player own_name)

let can_sell_hotel_nohotel_exn name game player own_name expected_output
    =
  name >:: fun _ ->
  assert_raises (CannotSellHotel "No hotel on Property") (fun () ->
      can_sell_hotel game player own_name)

let all_can_sell_hotel_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (all_can_sell_hotel game player)

let house_price_test name game player own_name expected_output =
  name >:: fun _ ->
  assert_equal expected_output (house_price game player own_name)

let all_can_buy_house_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (all_can_buy_house game player)

let player_exists_test name game player expected_output =
  name >:: fun _ ->
  assert_equal expected_output (player_exists game player)

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

let goes_bankrupt_test name game player cost expected_output =
  name >:: fun _ ->
  assert_equal expected_output (goes_bankrupt game player cost)

let delete_player_test name game player =
  name >:: fun _ ->
  let player_before = player_exists game player in
  delete_player game player;
  assert (player_before <> player_exists game player)

let sell_house_test name game own_name =
  name >:: fun _ ->
  let houses_before = get_houses game own_name in
  let houses_available_before = get_houses_available game in
  sell_house game own_name true;
  assert (
    get_houses_available game = houses_available_before + 1
    && houses_before - 1 = get_houses game own_name )

let sell_all_test name game player =
  name >:: fun _ ->
  sell_all game player;
  assert (
    List.for_all
      (fun x -> is_mortgaged game x)
      (get_ownable_name_list player) )

let p1 = make_player "p1"

let p2 = make_player "p2"

let p3 = make_player "p3"

let p4 = make_player "p4"

let p5 = make_player "p5"

let game_one = init_game test_board [| p1; p2; p3; p4; p5 |]

let () = update_balance p1 100000

let () = update_balance p4 10000

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
    [ 4; 3; 4 ]

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
    [ 4; 4; 4 ]

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
    [ 1; 0; 5; 4; 4 ]

let p1_sell = make_player "p1_sell"

let p2_sell = make_player "p2_sell"

let test_sell_game = init_game test_board [| p1_sell; p2_sell |]

let () =
  buy_ownable_lst test_sell_game test_board p1_sell
    [ "Mediterranean Avenue"; "Baltic Avenue" ]

let () =
  buy_ownable_lst test_sell_game test_board p2_sell
    [ "Reading Railroad"; "Water Works"; "Kentucky Avenue" ]

let () = add_house test_sell_game "Kentucky Avenue" true

(* Game Module Tests *)
let game_tests =
  [
    get_board_test "Game uses test_board as a board" test_game
      test_board;
    get_all_players_test "get all four players in test game" test_game
      [| player1; player2; player3; player4 |];
    current_player_test "Player 1 starts the game" test_game player1;
    current_player_test "Player 2 moves after Player 1"
      (next_player_help test_game_two)
      player2;
    current_player_test "Player 1 moves after Player 2 with 2 players"
      (next_player_help test_game_three)
      player1;
    sell_house_test "p1_sell sells a house on Mediterranean Ave"
      test_sell_game "Mediterranean Avenue";
    do_tax_test "p1_sell lands on Income Tax" test_sell_game p1_sell
      income_tax_space;
    do_free_parking_test "p2_sell collects free parking after tax"
      test_sell_game p2_sell;
    sell_all_test "p2_sell mortgages all of their ownables"
      test_sell_game p2_sell;
    player_exists_test "p1 exists in game_one" game_one p1 true;
    player_exists_test "player6 does not exist in game_one" game_one
      player6 false;
    goes_bankrupt_test "p4 will go bankrupt if they have to pay $12000"
      game_one p4 12000 true;
    goes_bankrupt_test
      "p1 will not go bankrupt if they have to pay $100" game_one p1 100
      false;
    delete_player_test "Deleting p5" game_one p5;
    can_sell_hotel_test "p4 can sell a hotel on Marvin Gardens" game_one
      p4 "Marvin Gardens" true;
    can_sell_hotel_nohotel_exn
      "p2 does not have a hotel on Virginia Ave" game_one p2
      "Virginia Avenue" false;
    all_can_sell_hotel_test
      "p3 can sell hotels on Kentucky and Illinois Ave" game_one p3
      [| "Illinois Avenue"; "Kentucky Avenue" |];
    can_sell_house_test "Player 1 can sell a house on Connecticut Ave"
      game_one p1 "Connecticut Avenue" true;
    can_sell_house_even_build_exn
      "Player 4 cannot sell a house on Ventnor due to even build"
      game_one p4 "Ventnor Avenue" false;
    can_sell_house_hotel_exn "Player 4 has a hotel on Marvin Gardens"
      game_one p4 "Marvin Gardens" false;
    can_sell_house_nohouse_exn "Player 2 has no houses on Virginia Ave"
      game_one p2 "Virginia Avenue" false;
    all_can_sell_house_test "p4 can sell houses on Park Place" game_one
      p4 [| "Park Place" |];
    can_trade_test "Player 4 can trade Boardwalk" game_one p4
      "Boardwalk" true;
    can_trade_test "Player 4 cannot trade Ventnor Avenue" game_one p4
      "Ventnor Avenue" false;
    all_can_trade_test "Player 1 cannot trade any properties" game_one
      p1 [||];
    all_can_trade_test
      "Player 4 can trade Baltic, Boardwalk and N.C. Ave., and Pacific \
       Ave"
      game_one p4
      [|
        "Pacific Avenue";
        "North Carolina Avenue";
        "Boardwalk";
        "Baltic Avenue";
      |];
    get_net_worth_test "Player 4 has $11250 worth in assets" game_one p4
      11150;
    get_properties_test
      "p3 owns Kentucky, Indiana, and Illinois Avenues" game_one p3
      [ "Illinois Avenue"; "Indiana Avenue"; "Kentucky Avenue" ];
    get_houses_available_test "8 Houses available in game_one" game_one
      8;
    get_hotels_available_test "9 Hotels available in game_one" game_one
      9;
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
      "Landing on Park Place with 1 house and owner monopoly costs $500"
      game_one 37 (1, 2) 175;
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
           color = "Light Blue";
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
           color = "Pink";
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
    has_monopoly_test "p1 has a monopoly on Light Blue" game_one p1
      "Light Blue" true;
    has_houses_on_color_test "p1 has houses on Light Blue" game_one p1
      "Light Blue" true;
  ]

(* Any Stockmarket Module Testing Helper Functions/Variables *)

let market = Yojson.Basic.from_file "stocks.json" |> init_market

let test_market = Yojson.Basic.from_file "stocks.json" |> init_market

let () = update_market test_market

let value_of_test name market stock expected_output =
  name >:: fun _ ->
  assert_equal expected_output (value_of market stock)
    ~printer:string_of_int

let value_of_num_shares_test name market stock num expected_output =
  name >:: fun _ ->
  assert_equal expected_output (value_of_num_shares market stock num)

let percent_change_of_test name market stock expected_output =
  name >:: fun _ ->
  assert_equal expected_output (percent_change_of market stock)

let stock_array_test name market expected_output =
  name >:: fun _ ->
  assert_equal expected_output (stock_array market) ~printer:print_arr

(* Stockmarket Module Tests *)
let stockmarket_tests =
  [
    value_of_test "Initial value of Amazon is $1000" market "Amazon"
      1000;
    value_of_num_shares_test
      "Initial value of 10 shares of CamlCoin is $100" market "CamlCoin"
      10 100;
    percent_change_of_test "Initial percent change is 0.00" market
      "RPCC" 0.00;
    value_of_test "Updated value of GME after one turn" test_market
      "GME"
      (int_of_float
         (Float.round
            ( 1000.
            +. (1000. *. percent_change_of test_market "GME" /. 100.) )));
    stock_array_test "All available stocks" market
      [| "CamlCoin"; "Amazon"; "RPCC"; "GME"; "Snarly Hacker Co." |];
  ]

(* Test Suite *)
let suite =
  "test suite for Monopoly"
  >::: List.flatten
         [ player_tests; board_tests; game_tests; stockmarket_tests ]

let _ = run_test_tt_main suite
