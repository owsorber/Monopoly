let rec turn b g = 
  (*do current player turn*)
  let current_player = Game.current_player p in
  let move = Input.turn current_player in
  match move with
    | Legal of t -> t.action current_player
    | Illegal -> ANSITerminal.print_string [ ANSITerminal.red ] 
      "Illegal move. Please enter a valid move.\n";
  
  (*advance to next player in game*)
  Game.next_player g;
  turn b g

(** [play_game b] starts the game given board file f. *)
let play_game f = 
  try
    let board = Board.init_board Yojson.Basic.from_file f in
    (*query number of players*)
    ANSITerminal.print_string [ ANSITerminal.red ]
    "Enter the number of players: \n>";
    let n = int_of_string (read_line ()) in 
    (*create board with number of players*)
    let game = Game.init_game board n in
    turn board game
  with Sys_error _ -> Stdlib.print_endline "board file not found"

(** [main ()] prompts for the board to use, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Monopoly!\n";
  print_endline
    "Please enter the name of the board file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
