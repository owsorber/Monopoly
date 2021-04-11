open Printers

type t = {
  player_id : Player.player_id;
  action : Player.t -> unit;
  is_double : bool;
  is_end : bool;
}

type result =
  | Legal of t
  | Illegal

(** moves are the different possible moves a player can make, regardless
    of current game conditions. *)
type moves =
  | Roll
  | Buy
  | Mortgage
  | Trade
  | End
  | Quit
  | Faulty

(**[string_of_move m] is the string representation of move [m]. *)
let string_of_move m =
  match m with
  | Roll -> "Roll"
  | Buy -> "Buy"
  | Mortgage -> "Mortgage"
  | Trade -> "Trade"
  | End -> "End Turn"
  | Quit -> "Quit"
  | Faulty -> "Faulty"

(** [options_printer phase] is the string representation of the options
    a player can take during phase 1 if [phase] is true or phase 2 if
    [phase] is false. *)
let options_printer phase =
  match phase with
  | true ->
      "\n\
       Please choose one of the options below (case sensitive): \n\
      \ Roll \n\
      \ Quit"
  | false ->
      "\n\n\
       Please choose one of the options below (case sensitive): \n\
      \ Buy \n\
      \ Mortgage \n\
      \ Trade \n\
      \ End Turn \n\
      \ Quit"

(**[string_of_list lst] is the string representation of list [lst]*)
let string_of_list lst =
  let pp_elts lst =
    let rec loop acc = function
      | [] -> acc
      | h :: t -> loop (h ^ "\n " ^ acc) t
    in
    loop "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [input s o] is the desired move [s] given options [o]. *)
let input s =
  match s with
  | "Roll" | "r" -> Roll
  | "Buy" | "b" -> Buy
  | "Mortgage" | "m" -> Mortgage
  | "Trade" | "t" -> Trade
  | "End Turn" | "e" -> End
  | "Quit" | "q" -> Quit
  | _ -> Faulty

(**[string_of_roll roll] returns the string represntation of roll
   [roll]. *)
let string_of_roll roll =
  match roll with
  | x, y ->
      "First dice: " ^ string_of_int x ^ ". Second dice: "
      ^ string_of_int y ^ "\n"

let get_action turn = turn.action

let get_double t = t.is_double

let get_end t = t.is_end

(**[double_of_roll (a,b)] returns true if a and b are equal and false if
   not. *)
let double_of_roll (a, b) = if a = b then true else false

(**[roll p b] returns a Legal result of the action representing a roll
   by player [p], given board [b]. *)
let roll p b =
  let r = Player.roll () in
  magenta_print (string_of_roll r);
  if Player.passes_go r p then
    green_print "You passed go! You gained $200!\n";
  let new_space = Player.projected_space r p b in
  magenta_print "You landed on: ";
  yellow_print new_space;
  print_endline "\n";
  Legal
    {
      player_id = Player.get_player_id p;
      action = Player.move_player r;
      is_double = double_of_roll r;
      is_end = false;
    }

(**[end_turn p b] is the representative result of type t for player [p]
   on board [b] to end their turn. *)
let end_turn p b =
  Legal
    {
      player_id = Player.get_player_id p;
      action = (fun x -> ());
      is_double = false;
      is_end = true;
    }

let buy p b =
  let current_location = Player.get_location p in
  white_print "You are attempting to buy: ";
  yellow_print (Board.space_name b current_location);
  print_endline "\n";

  (* check ownable property *)
  (* let space = Board.space_from_location b current_location in *)
  (* if Board.is_ownable space then (* continue *) else Illegal *)

  (* check is_owned *)
  (* if space.is_owned then (* continue *) else Illegal*)

  (* check valid balance *)
  (* if Player.get_balance p > space.price then (* continue *) else
     Illegal *)
  Legal
    {
      player_id = Player.get_player_id p;
      action = (fun x -> ());
      (*Player.buy_property p property_name*)
      is_double = false;
      is_end = false;
    }

let mortgage p b =
  white_print "Please enter what property you would like to mortgage: ";
  yellow_print "Available properties: ";
  cyan_print (p |> Player.get_property_name_list |> string_of_list);
  print_string "\n> ";
  let property_name = read_line () in

  (*check ownable property*)
  (* if Board.is_ownable space then (* continue *) else Illegal *)

  (*check is owned by player*)
  (* if Player.owns space then (* continue *) else Illegal *)
  Legal
    {
      player_id = Player.get_player_id p;
      action = (fun x -> ());
      (*Player.mortgage_property p property_name*)
      is_double = false;
      is_end = false;
    }

let trade p b =
  white_print "Please enter which player you would like to trade with: ";
  print_string "> ";
  let trade_partner = read_line () in

  (*check is a player*)
  (* if Player.exists trade_partner then (* continue *) else Illegal *)
  Legal
    {
      player_id = Player.get_player_id p;
      action = (fun x -> ());
      (* trade somehow *)
      is_double = false;
      is_end = false;
    }

(**[print_player_info b p] prints appropriate info about player [p]
   given board state [b]. *)
let print_player_info b p =
  let player_bal = string_of_int (Player.get_balance p) in
  let player_props = string_of_list (Player.get_property_name_list p) in
  let player_loc = Board.space_name b (Player.get_location p) in
  cyan_print "Current balance: ";
  green_print (player_bal ^ "\n");
  cyan_print "Current properties: ";
  green_print (player_props ^ "\n");
  cyan_print "Current location: ";
  green_print player_loc

(**[max players f] is the maximum element in [players] according to the
   comparison function f applied to each player. *)
let max players f =
  let rec max acc = function
    | [] -> acc
    | h :: t -> if f h > f acc then max h t else max acc t
  in
  max (List.nth players 0) players

(**[print_endgame b g] prints appropriate information about the game
   ending give game [g] and board [b]. *)
let print_endgame b g =
  let players = Array.to_list (Game.get_all_players g) in
  let max_money = max players Player.get_balance in
  let max_properties =
    max players (fun p -> List.length (Player.get_property_name_list p))
  in
  yellow_print "Player with the most money (ties excluded): ";
  green_print (Player.get_player_id max_money);
  yellow_print " with ";
  green_print ("$" ^ (max_money |> Player.get_balance |> string_of_int));
  print_endline "";
  yellow_print "Player with the most properties (ties excluded): ";
  green_print (Player.get_player_id max_properties);
  yellow_print " with ";
  green_print
    (max_properties |> Player.get_property_name_list |> List.length
   |> string_of_int);
  yellow_print " properties.\n"

(**[graceful_shutdown b g] ends the game [g] given board [b]. *)
let graceful_shutdown b g =
  red_print "Thanks for playing!\n";
  print_endgame b g;
  exit 0

(**[turn_info b p phase] prints the information for player [p] on board
   [b] during phase [phase] of their turn. *)
let turn_info b p phase =
  print_player_info b p;
  cyan_print "\npossible moves: ";
  yellow_print (options_printer phase);
  cyan_print "\n>"

(**[turn p b g phase] is the representation of a single response by the
   user for player [p], given board [b] and game [g]. *)
let turn p b g phase =
  match phase with
  | true -> (
      cyan_print ("\n" ^ Player.get_player_id p ^ "'s turn.\n");
      let _ = turn_info b p phase in
      ();
      try
        match input (read_line ()) with
        | Roll -> roll p b
        | Quit -> graceful_shutdown b g
        | _ -> Illegal
      with _ -> Illegal)
  | false -> (
      let _ = turn_info b p phase in
      ();
      try
        match input (read_line ()) with
        | Buy -> buy p b
        | Mortgage -> mortgage p b
        | Trade -> Illegal
        | End -> end_turn p b
        | Quit -> graceful_shutdown b g
        | _ -> Illegal
      with _ -> Illegal)
