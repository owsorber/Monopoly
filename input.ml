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
  | BuyHouse
  | BuyHotel
  | Mortgage
  | Trade
  | End
  | Quit
  | Faulty

(**[string_of_move m] is the string representation of move [m]. *)
let string_of_move m =
  match m with
  | Roll -> "Roll"
  | Buy -> "Buy Current Space"
  | BuyHouse -> "Buy House"
  | BuyHotel -> "Buy Hotel"
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
      \ Buy Current Space\n\
      \ Buy House\n\
      \ Buy Hotel\n\
      \ Mortgage \n\
      \ Trade \n\
      \ End Turn \n\
      \ Quit"

(**[string_of_list lst] is the string representation of list [lst]*)
let string_of_list lst =
  let pp_elts lst =
    let rec loop acc = function
      | [] -> acc
      | h1 :: h2 :: t -> loop (h1 ^ ";  " ^ acc) (h2 :: t)
      | h :: t -> loop (h ^ acc) t
    in
    loop "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let property_info property g =
  let is_mortgaged = string_of_bool (Game.is_mortgaged g property) in
  let houses = Game.get_houses g property in
  let num_houses = string_of_int (if houses > 4 then 4 else houses) in
  let has_hotel = string_of_bool (houses > 4) in
  property ^ ". Houses: " ^ num_houses ^ ". Hotel: " ^ has_hotel
  ^ ". Mortgaged: " ^ is_mortgaged

let pp_propert_list g lst =
  let pp_elts lst =
    let rec loop acc = function
      | [] -> acc
      | h1 :: h2 :: t ->
          loop (acc ^ property_info h1 g ^ "\n") (h2 :: t)
      | h :: t -> loop (acc ^ property_info h g) t
    in
    loop "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(**[print_array a] prints the string representation of array [a]. *)
let print_array elem_printer a =
  Array.iteri
    (fun i elem ->
      yellow_print (string_of_int (i + 1));
      white_print (": " ^ elem_printer elem))
    a

(** [input s o] is the desired move [s] given options [o]. *)
let input s =
  match s with
  | "Roll" | "r" -> Roll
  | "Buy Current Space" | "b" -> Buy
  | "Buy House" -> BuyHouse
  | "Buy Hotel" -> BuyHotel
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

let landing p b g space r =
  try
    let current_location = Player.get_location p in
    let rent = Game.get_rent g current_location r in
    if rent > 0 then
      match Game.owner g space with
      | Some player ->
          magenta_print "You must pay ";
          red_print (string_of_int rent);
          magenta_print " to ";
          yellow_print (Player.get_player_id player);
          Player.update_balance p rent
      | None -> ()
    else ()
  with _ -> ()

(**[double_of_roll (a,b)] returns true if a and b are equal and false if
   not. *)
let double_of_roll (a, b) = if a = b then true else false

(**[roll p b] returns a Legal result of the action representing a roll
   by player [p], given board [b]. *)
let roll p b g =
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
      action =
        (fun player ->
          Player.move_player r player;
          landing player b g new_space r);
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

let buy p b g =
  let current_location = Player.get_location p in
  white_print "You are attempting to buy: ";
  yellow_print (Board.space_name b current_location);
  print_endline "\n";

  let legality = ref false in
  (* check ownable property *)
  let space = Board.space_from_location b current_location in
  if Board.is_ownable b space then
    match Game.get_ownable_status g space with
    | Some ownable_space -> (
        let ownable_space_name = Board.space_name b current_location in
        (* check is_owned *)
        try
          match Game.owner g ownable_space_name with
          | Some player ->
              red_print
                "INFO: The space you are currently on is already owned \
                 by: ";
              cyan_print (Player.get_player_id player);
              print_endline ""
          | None -> (
              (*check valid balance*)
              try
                let price =
                  Game.get_ownable_price b ownable_space_name
                in
                if Player.get_balance p > price then legality := true
                else
                  red_print
                    "INFO: You do not have enough money to purchase \
                     this space\n"
              with
              | Game.NotOwnableSpace -> red_print "not ownable space\n"
              | Game.NotOwnableName -> red_print "not ownable name\n"
              | Board.NameNotOnBoard s ->
                  red_print (s ^ " not on board\n")
              | _ -> red_print "somewhere else\n")
        with Game.NotOwnableName ->
          red_print
            "INFO: The space you are currently on cannot be bought\n")
    | None ->
        red_print
          "INFO: The space you are currently on cannot be bought\n"
  else
    red_print "INFO: The space you are currently on cannot be bought\n";

  if !legality then
    let ownable_space_name = Board.space_name b current_location in
    let price = Game.get_ownable_price b ownable_space_name in
    Legal
      {
        player_id = Player.get_player_id p;
        action =
          (fun p ->
            Player.buy_ownable p ownable_space_name price;
            Game.make_ownable_owned g p ownable_space_name);
        is_double = false;
        is_end = false;
      }
  else Illegal

let mortgage p b g =
  white_print
    "Please the number of the property you would like to mortgage: ";
  yellow_print "Possible properties to mortgage: ";
  let mortgagables = Game.all_mortgagable g p in
  print_array (fun x -> x) mortgagables;
  if Array.length mortgagables = 0 then green_print "None.\n";
  print_string "\n> ";
  let property_index = read_line () in
  (* check out of bounds *)
  try
    let property_name =
      mortgagables.(int_of_string property_index - 1)
    in
    let legality = ref false in

    (match Board.space_from_space_name b property_name with
    | Some space ->
        if Board.is_ownable b space then
          (*check is owned by player*)
          match Game.owner g property_name with
          | Some player ->
              if player = p then legality := true
              else
                red_print
                  "INFO: The property you are trying to mortgage is \
                   owned by: ";
              cyan_print (Player.get_player_id player);
              print_endline ""
          | None ->
              red_print
                "INFO: The property you are tyring to mortgage is not \
                 owned by any player\n"
        else
          red_print
            "INFO: The property you entered cannot be mortgaged\n"
    | None ->
        red_print "INFO: The property you entered cannot be mortgaged\n");
    if !legality then
      Legal
        {
          player_id = Player.get_player_id p;
          action =
            (* handle exceptions in the function *)
            (fun x -> Game.make_ownable_mortgaged g x property_name);
          is_double = false;
          is_end = false;
        }
    else Illegal
  with _ ->
    red_print "invalid input\n";
    Illegal

let buy_house p g =
  white_print
    "Please enter the number of the property you would like to buy a \
     house on: ";
  (*need Game.property_list_of_ownable_list*)
  let prop_array = Array.of_list (Player.get_ownable_name_list p) in
  print_array (fun x -> x) prop_array;
  print_string "\n> ";
  let property_index = read_line () in
  try
    let property_name = prop_array.(int_of_string property_index - 1) in
    try
      if Game.can_add_house g p property_name then
        Legal
          {
            player_id = Player.get_player_id p;
            action = (fun _ -> Game.add_house g property_name);
            is_double = false;
            is_end = false;
          }
      else (
        red_print "you cannot add a house on this property";
        Illegal)
    with Game.NotPropertyName ->
      red_print "The given name is not a valid property";
      Illegal
  with _ ->
    red_print "invalid input\n";
    Illegal

let buy_hotel p g =
  white_print
    "Please enter the number of the property you would like to buy a \
     hotel on: ";
  (*need Game.property_list_of_ownable_list*)
  let prop_array = Array.of_list (Player.get_ownable_name_list p) in
  print_array (fun x -> x) prop_array;
  print_string "\n> ";
  let property_index = read_line () in
  try
    let property_name = prop_array.(int_of_string property_index - 1) in
    try
      if Game.can_add_hotel g p property_name then
        Legal
          {
            player_id = Player.get_player_id p;
            action = (fun _ -> Game.add_hotel g property_name);
            is_double = false;
            is_end = false;
          }
      else (
        red_print "you cannot add a house on this property";
        Illegal)
    with Game.NotPropertyName ->
      red_print "The given name is not a valid property";
      Illegal
  with _ ->
    red_print "invalid input\n";
    Illegal

let trade p b =
  white_print "Please enter which player you would like to trade with: ";
  print_string "> ";
  let trade_partner = read_line () in

  (*check is a player*)
  (* if Game.player_exists trade_partner then (* continue *) else
     Illegal *)
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
let print_player_info b p g =
  let player_bal = string_of_int (Player.get_balance p) in
  let player_props =
    pp_propert_list g (Player.get_ownable_name_list p)
  in
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
    max players (fun p -> List.length (Player.get_ownable_name_list p))
  in
  magenta_print "Player with the most money (ties excluded): ";
  green_print (Player.get_player_id max_money);
  magenta_print " with ";
  green_print ("$" ^ (max_money |> Player.get_balance |> string_of_int));
  print_endline "";
  magenta_print "Player with the most properties (ties excluded): ";
  green_print (Player.get_player_id max_properties);
  magenta_print " with ";
  green_print
    (max_properties |> Player.get_ownable_name_list |> List.length
   |> string_of_int);
  magenta_print " properties.\n"

(**[graceful_shutdown b g] ends the game [g] given board [b]. *)
let graceful_shutdown b g =
  red_print "Thanks for playing!\n";
  print_endgame b g;
  exit 0

(**[turn_info b p phase] prints the information for player [p] on board
   [b] during phase [phase] of their turn. *)
let turn_info b p g phase =
  print_player_info b p g;
  cyan_print "\npossible moves: ";
  yellow_print (options_printer phase);
  cyan_print "\n>"

(**[turn p b g phase] is the representation of a single response by the
   user for player [p], given board [b] and game [g]. *)
let turn p b g phase =
  match phase with
  | true -> (
      cyan_print ("\n" ^ Player.get_player_id p ^ "'s turn.\n");
      let _ = turn_info b p g phase in
      ();
      try
        match input (read_line ()) with
        | Roll -> roll p b g
        | Quit -> graceful_shutdown b g
        | _ -> Illegal
      with _ -> Illegal)
  | false -> (
      let _ = turn_info b p g phase in
      ();
      try
        match input (read_line ()) with
        | Buy -> buy p b g
        | BuyHouse -> buy_house p g
        | BuyHotel -> buy_hotel p g
        | Mortgage -> mortgage p b g
        | Trade -> Illegal
        | End -> end_turn p b
        | Quit -> graceful_shutdown b g
        | _ -> Illegal
      with _ -> Illegal)
