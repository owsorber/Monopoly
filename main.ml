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

(**[double_turn b g p] handles taking another roll phase for player [p]
   given board [b] and game [g]. *)
let rec double_turn b g p =
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

(**[handle_double result b g p] executes a call to [double turn] if a
   double was rolled by player [p], given result [result], board [b],
   and game [g]. *)
let handle_double result b g p =
  match result with
  | Input.Legal r ->
      let double = Input.get_double r in
      if double then (
        (*redo phase 1*)
        green_print "Yay! You rolled doubles. You may roll again! \n";
        double_turn b g p)
      else ()
  | Input.Illegal -> ()
(*already handled*)

(**[turn b g phase] repeatedly executes turns for each player, beginning
   on phase [phase] advancing to the next player and phase after every
   turn. Turns are executed on board [b] and game [g]*)
let rec turn b g phase =
  (*do current player turn*)
  let current_player = Game.current_player g in
  let result = Input.turn current_player b g phase in
  let progress = take_action result current_player g phase in
  (*check for doubles*)
  handle_double result b g current_player;
  (*advance to next player in game*)
  match progress with
  | true -> turn b g (not phase)
  | false -> turn b g phase

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
        turn board game true
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
