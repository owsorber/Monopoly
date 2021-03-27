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

(** [options p g] is the list containing all possible moves player [p] can 
    make given game state [g]. *)
let options p g = 
  let o = [Roll] in 
  o

let string_of_move m = 
  if m = Roll then "Roll" else "Invalid Move"

(** [options_printer] is the string representation of [o], a [moves] list. *)
let options_printer o = 
  let pp_elts lst =
    let rec loop acc = function
      | [] -> acc
      | h::t -> loop (acc ^ (string_of_move h)) t
  in loop "" lst
  in "[" ^ pp_elts o ^ "]"

(** [input s o] is the desired move [s] given options [o]. *)
let input s o =
  let i = int_of_string s in
  List.nth o (i - 1)
let cyan_print = ANSITerminal.print_string [ ANSITerminal.cyan ]

let turn p = 
  cyan_print ("\nPlayer " ^ (Player.get_id p) ^ "'s turn.\n");
  cyan_print "\n possible moves: ";
  Stdlib.print_endline (options_printer (options p g));
  cyan_print "\n input the number of the move you would like to make:";
  cyan_print "\n >";
  try
    match input (read_line ()) (options p g) with
      | Roll -> Legal of 
        {
          player_id = Player.get_player_id p;
          action = Player.move_player (p Player.roll)
        }
      | _ -> Illegal
  with 
  | _ -> Illegal