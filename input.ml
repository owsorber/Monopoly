type t = {
  player_id : Player.player_id;
  action : Player.t -> unit;
}

type result =
  | Legal of t
  | Illegal

(** moves are the different possible moves a player can make, regardless of 
    current game conditions. *)
type moves = 
  | Roll 
  | Quit

(** [options p g] is the list containing all possible moves player [p] can 
    make given game state [g]. *)
let options p g = 
  let o = [Roll; Quit] in 
  o

(**[string_of_move m] is the string representation of move [m]. *)
let string_of_move m = 
  match m with
  | Roll -> "Roll"
  | Quit -> "Quit"

(** [options_printer] is the string representation of [o], a [moves]
    list. *)
let options_printer o =
  let pp_elts lst =
    let rec loop acc i= function
      | [] -> acc
      | h::t -> loop 
        (acc ^ (string_of_int i) ^ ": " ^ (string_of_move h) ^ " ") (i + 1) t 
  in loop "" 1 lst
  in "[" ^ pp_elts o ^ "]"

(**[string_of_list lst] is the string representation of list [lst]*)
let string_of_list lst= 
  let pp_elts lst =
    let rec loop acc = function
      | [] -> acc
      | h::t -> loop ( h ^ "\n " ^ acc) t
    in
    loop "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [input s o] is the desired move [s] given options [o]. *)
let input s o =
  let i = int_of_string s in
  List.nth o (i - 1)

let cyan_print = ANSITerminal.print_string [ ANSITerminal.cyan ]
let magenta_print = ANSITerminal.print_string [ ANSITerminal.magenta ]
let red_print = ANSITerminal.print_string [ ANSITerminal.red ]
let green_print = ANSITerminal.print_string [ ANSITerminal.green ]
let yellow_print = ANSITerminal.print_string [ ANSITerminal.yellow ]

(**[string_of_roll roll] returns the string represntation of roll [roll]. *)
let string_of_roll roll = 
  match roll with
  | (x, y) -> "First dice: " ^ string_of_int x ^ 
  ". Second dice: " ^ string_of_int y ^ "\n"  

let get_action turn =
  turn.action

(**[roll p b] returns a Legal result of the action representing a roll by player
    [p], given board [b]. *) 
let roll p b = let r = Player.roll () in
magenta_print (string_of_roll r);
if Player.passes_go r p then green_print "You passed go! You gained $200!\n";
let new_space = Player.projected_space r p b in 
magenta_print "You landed on: "; yellow_print new_space; print_endline "";
  Legal
  {
    player_id = Player.get_player_id p;
    action = Player.move_player (r)
  }

(**[print_player_info b p] prints appropriate info about player [p] given
    board state [b]. *)
let print_player_info b p = 
  let player_bal = string_of_int (Player.get_balance p) in
  let player_props = string_of_list (Player.get_property_name_list p) in
  let player_loc = Board.space_name b (Player.get_location p) in
  cyan_print "Current balance: "; green_print (player_bal ^ "\n");
  cyan_print "Current properties: "; green_print (player_props ^ "\n"); 
  cyan_print "Current location: "; green_print player_loc

(**[max players f] is the maximum element in [players] according to the 
    comparison function f applied to each player. *)
let max players f= 
  let rec max acc = function 
    | [] -> acc
    | h::t -> if (f h) > (f acc) then
      max h t else max acc t
    in max (List.nth players 0) players

(**[print_endgame b g] prints appropriate information about the game ending 
    give game [g] and board [b]. *)
let print_endgame b g = 
  let players = Array.to_list(Game.get_all_players g) in
  let max_money = max players Player.get_balance in
  let max_properties = 
    max players (fun p -> List.length(Player.get_property_name_list p)) in
  yellow_print "Player with the most money (ties excluded): " ;
  green_print (Player.get_player_id max_money);
  yellow_print " with "; 
  green_print ("$" ^ (max_money |> Player.get_balance |> string_of_int)); 
  print_endline "";
  yellow_print "Player with the most properties (ties excluded): ";
  green_print (Player.get_player_id max_properties);
  yellow_print " with ";
  green_print (max_properties |> Player.get_property_name_list 
    |> List.length |> string_of_int);
  yellow_print " properties.\n"

(**[graceful_shutdown b g] ends the game [g] given board [b]. *)
let graceful_shutdown b g = 
  red_print "Thanks for playing!\n";
  print_endgame b g;
  exit 0

(**[turn p b g] is the representation of a single response by the user for 
    player [p], given board [b] and game [g]. *)
let turn p b g =
  cyan_print ("\n" ^ (Player.get_player_id p) ^ "'s turn.\n");
  print_player_info b p;
  cyan_print "\n possible moves: ";
  Stdlib.print_endline (options_printer (options p g));
  cyan_print "\n input the number of the move you would like to make:";
  cyan_print "\n >";
  try
    match input (read_line ()) (options p g) with
      | Roll -> roll p b
      | Quit -> graceful_shutdown b g
  with 
  | _ -> Illegal
