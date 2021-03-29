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

let string_of_roll roll = 
  match roll with
  | (x, y) -> "First dice: " ^ string_of_int x ^ 
  ". Second dice: " ^ string_of_int y ^ "\n"
let get_action turn =
  turn.action

let roll p = let r = Player.roll () in
magenta_print (string_of_roll r);
  Legal
  {
    player_id = Player.get_player_id p;
    action = Player.move_player (r)
  }

let print_player_info b p = 
  let player_bal = string_of_int (Player.get_balance p) in
  let player_props = string_of_list (Player.get_property_name_list p) in
  let player_loc = Board.space_name b (Player.get_location p) in
  cyan_print "Current balance: "; green_print (player_bal ^ "\n");
  cyan_print "Current properties: "; green_print (player_props ^ "\n"); 
  cyan_print "Current loaction: "; green_print player_loc

let graceful_shutdown () = 
  red_print "Thanks for playing!\n";
  exit 0

let turn p b g =
  cyan_print ("\n" ^ (Player.get_player_id p) ^ "'s turn.\n");
  print_player_info b p;
  cyan_print "\n possible moves: ";
  Stdlib.print_endline (options_printer (options p g));
  cyan_print "\n input the number of the move you would like to make:";
  cyan_print "\n >";
  try
    match input (read_line ()) (options p g) with
      | Roll -> roll p
      | Quit -> graceful_shutdown ()
  with 
  | _ -> Illegal
