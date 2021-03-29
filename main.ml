let rec turn b g =
  (*do current player turn*)
  let current_player = Game.current_player g in
  let move = Input.turn current_player b g in
  let _ = match move with
    | Legal r -> Input.get_action r current_player; Game.next_player g
    | Illegal -> ANSITerminal.print_string [ ANSITerminal.red ] 
      "Illegal move. Please enter a valid move.\n" in
  
  (*advance to next player in game*)
  turn b g

let yellow_print = ANSITerminal.print_string [ ANSITerminal.yellow ]

let red_print = ANSITerminal.print_string [ ANSITerminal.red ]

(** [play_game b] starts the game given board file f. *)
let rec play_game () =
  "Please enter the name of the board file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | f -> 
    try
      let board = Board.init_board (Yojson.Basic.from_file f) in
      (*query number of players*)
      ANSITerminal.print_string [ ANSITerminal.red ]
      "Enter the number of players: \n> ";
      let n = int_of_string (read_line ()) in 
      (*query player names*)
      let players = Array.make n (Player.make_player "") in
      for i = 1 to n do
        players.(i-1) <- Player.make_player
        (yellow_print ("Enter Player " ^ (string_of_int i) ^ "'s name: "); 
        read_line ())
      done;
      (* create board with number of players *)
      let game = Game.init_game board players in
      turn board game
    with Sys_error _ -> Stdlib.print_endline "board file not found"; play_game ()

(** [main ()] prompts for the board to use, then starts it. *)
let main () =
  red_print "\n\nWelcome to Monopoly!\n";
  print_endline "";
  play_game ()

(* Execute the game engine. *)
let () = main ()
