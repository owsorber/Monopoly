open Printers

(**[take_action result p g phase] executes the action given by result
   [result], if one exists, on player [p] given game [g] during phase
   [phase]. *)
let take_action result p g phase =
  match result with
  | Input.Legal r ->
      Input.get_action r p;
      if phase = false then (
        Game.next_player g;
        red_print "Press enter to begin next player's turn. ";
        let _ = read_line () in
        true)
      else true
  | Input.Illegal ->
      red_print "Illegal move. Please enter a valid move.\n";
      false

let rec phase_2 b g p =
  let result = Input.turn p b g false in
  (* let _ = take_action result p g false in *)
  match result with
  | Input.Legal r ->
      Input.get_action r p;
      Input.get_end r
  | Input.Illegal ->
      red_print "Illegal move. Please enter a valid move. \n";
      phase_2 b g p

(**[double_turn b g p] handles taking another roll phase for player [p]
   given board [b] and game [g]. *)
let rec double_turn b g p =
  (*phase 2*)
  while phase_2 b g p = false do
    ()
  done;

  let result = Input.turn p b g true in
  let _ = take_action result p g true in
  match result with
  | Input.Legal r ->
      let double = Input.get_double r in
      if double then
        green_print
          "WOW! Doubles again?! Sadly that's it for rolling. \n\n"
      else ()
  | Input.Illegal ->
      red_print "Illegal move. Please enter a valid move. \n";
      double_turn b g p

let rec phase_1 b g p =
  let result = Input.turn p b g true in
  (* let _ = take_action result p g true in *)
  match result with
  | Input.Legal r ->
      Input.get_action r p;
      let double = Input.get_double r in
      if double then (
        green_print "Yay! You rolled doubles. You may roll again! \n";
        double_turn b g p)
      else ()
  | Input.Illegal ->
      red_print "Illegal move. Please enter a valid move. \n";
      phase_1 b g p

(**[turn_handler b g] repeatedly executes turns for each player,
   advanceing to the next player after each turn. Turns are executed on
   board [b] and game [g]. *)
let rec turn_handler b g =
  let p = Game.current_player g in
  print_horizontal_line ();
  (*phase 1*)
  phase_1 b g p;
  (*phase 2*)
  while phase_2 b g p = false do
    ()
  done;
  Game.next_player g;
  turn_handler b g

(** [get_player_count ()] prompts the user to enter in the number of
    players until a valid (positive integer) input is read. *)
let rec get_player_count () =
  red_print "Enter the number of players: \n> ";
  try
    let n = int_of_string (read_line ()) in
    if n < 1 then (
      print_string "Invalid input. Please enter a positive integer. \n";
      get_player_count ())
    else
      match n with
      | 1 ->
          cyan_print "It's lonely in here... have fun!\n";
          1
      | i ->
          if i >= 10 then
            cyan_print
              "Wow you have a lot of friends! I wish I had that many \
               friends...\n";
          i
  with _ ->
    print_string "Invalid input. Please enter a positive integer. \n";
    get_player_count ()

(** [play_game b] starts the game given board file f. *)
let rec play_game () =
  print_string
    "Please enter the name of the board file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | f -> (
      try
        let board = Board.init_board (Yojson.Basic.from_file f) in
        (*query number of players*)
        let n = get_player_count () in
        let players = Array.make n (Player.make_player "") in
        for i = 1 to n do
          players.(i - 1) <-
            Player.make_player
              (yellow_print
                 ("Enter Player " ^ string_of_int i ^ "'s name: ");
               read_line ())
        done;
        (* create board with number of players *)
        let game = Game.init_game board players in
        (* turn board game true *)
        turn_handler board game
      with Sys_error _ ->
        Stdlib.print_endline "board file not found";
        play_game ())

(** [main ()] prompts for the board to use, then starts it. *)
let main () =
  red_print "\n\nWelcome to Monopoly!\n";
  print_endline "";
  play_game ()

(* Execute the game engine. *)
let () = main ()
