let cyan_print = ANSITerminal.print_string [ ANSITerminal.cyan ]

let yellow_print = ANSITerminal.print_string [ ANSITerminal.yellow ]

let red_print = ANSITerminal.print_string [ ANSITerminal.red ]

let rec turn b g =
  (*do current player turn*)
  let current_player = Game.current_player g in
  let move = Input.turn current_player b g in
  let _ =
    match move with
    | Legal r ->
        Input.get_action r current_player;
        Game.next_player g;
        red_print "Press enter to begin next player's turn. ";
        let _ = read_line () in
        ()
    | Illegal -> red_print "Illegal move. Please enter a valid move.\n"
  in
  (*advance to next player in game*)
  turn b g

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
        turn board game
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
