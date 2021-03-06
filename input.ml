open Printers
open Graphics

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
  | SellHouse
  | SellHotel
  | Mortgage
  | UnMortgage
  | Trade
  | BuyStock
  | SellStock
  | End
  | Quit
  | Faulty

let read_line () =
  let return = ref "" in
  let c = ref 0 in
  while !c <> -35 do
    c := int_of_char (read_key ()) - 48;
    if !c <> -35 then
      if !c = -40 then
        if String.length !return > 0 then
          return := String.sub !return 0 (String.length !return - 1)
        else ()
      else return := !return ^ string_of_int !c
    else ();
    Gui.input_print !return cyan
  done;
  !return

(**[string_of_move m] is the string representation of move [m]. *)
let string_of_move m =
  match m with
  | Roll -> "Roll"
  | Buy -> "Buy Current Space"
  | Mortgage -> "Mortgage"
  | UnMortgage -> "Unmortgage"
  | BuyHouse -> "Buy House"
  | BuyHotel -> "Buy Hotel"
  | SellHouse -> "Sell House"
  | SellHotel -> "Sell Hotel"
  | BuyStock -> "Buy Stocks"
  | SellStock -> "Sell Stocks"
  | Trade -> "Trade"
  | End -> "End Turn"
  | Quit -> "Quit"
  | Faulty -> "Faulty"

let phase1_options = [| Roll; Quit |]

let phase2_options =
  [|
    Buy;
    BuyHouse;
    BuyHotel;
    SellHouse;
    SellHotel;
    BuyStock;
    SellStock;
    Mortgage;
    UnMortgage;
    Trade;
    End;
    Quit;
  |]

(**[print_array a] prints the string representation of array [a]. *)
let print_array elem_printer a =
  Array.iteri
    (fun i elem ->
      yellow_print (string_of_int (i + 1) ^ ": " ^ elem_printer elem))
    a

let rec phase2_string_of_move m p b g =
  let move_str = string_of_move m in
  let details =
    match m with Buy -> phase2_string_of_buy p b g | _ -> ""
  in
  move_str ^ details

and phase2_string_of_buy p b g =
  let current_location = Player.get_location p in
  let current_space = Board.space_from_location b current_location in
  let space_name = Board.space_name b current_location in
  if Board.is_ownable b current_space then
    if Game.is_available g space_name then
      " (Price: "
      ^ string_of_int
          (Game.get_ownable_price b
             (Board.space_name b current_location))
      ^ ")"
    else " (Illegal Action)"
  else " (Illegal Action)"

(** [options_printer phase] is the string representation of the options
    a player can take during phase 1 if [phase] is true or phase 2 if
    [phase] is false. *)
let options_printer phase p b g =
  yellow_print
    "Please enter the number of the action you would like to take:";
  match phase with
  | true -> print_array (fun x -> string_of_move x) phase1_options
  | false ->
      print_array
        (fun x -> phase2_string_of_move x p b g)
        phase2_options

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

let property_info property g b =
  (*pattern match against property to only print useful info*)
  match Board.space_from_space_name b property with
  | Some space -> (
      match space with
      | Property _ ->
          property
          ^ Game.get_ownable_info g b property
          ^ ". Color: " ^ Board.color b space
      | _ -> property ^ Game.get_ownable_info g b property)
  | None -> "impossible"

let pp_propert_list g b lst =
  let pp_elts lst =
    let rec loop = function
      | [] -> ()
      | h1 :: h2 :: t ->
          green_print (property_info h1 g b);
          loop (h2 :: t)
      | h :: t ->
          green_print (property_info h g b);
          loop t
    in
    loop lst
  in
  pp_elts lst

(**[string_of_roll roll] returns the string represntation of roll
   [roll]. *)
let string_of_roll roll =
  match roll with
  | x, y ->
      "First dice: " ^ string_of_int x ^ ". Second dice: "
      ^ string_of_int y ^ ""

let get_action turn = turn.action

let get_double t = t.is_double

let get_end t = t.is_end

(** [do_potential_bankruptcy g p c] bankrupts player [p] if the cost [c]
    they must pay is too high, or makes them sell all their assets if
    they can afford to pay the debt after selling all assets. If [p2] is
    a different player than [p], [p2] represents the player that debt is
    owed to. Otherwise, debt is owed to the bank. Requires: cost [c] is
    greater than player [p]'s balance, which should be checked before
    this function is called. *)
let do_potential_bankruptcy g p c p2 =
  let player_name = Player.get_player_id p in
  magenta_print
    ("The cost that " ^ player_name
   ^ " must pay is higher than their current balance.");
  if Game.goes_bankrupt g p c then (
    Game.delete_player g p;
    red_print
      (player_name
     ^ " doesn't have enough assets to pay off the debt! They went \
        bankrupt!"))
  else (
    Game.sell_all g p;
    if p == p2 then Player.update_balance p (-c) else Player.pay p p2 c;
    green_print
      ("However, " ^ player_name
     ^ " mortgaged all assets and successfully payed off the debt!"))

let rec landing p g space_name r cards (modRent, mult) =
  let b = Game.get_board g in
  let space = Board.space_from_space_name b space_name in
  let real_space =
    match space with Some s -> s | None -> failwith "Not a space"
  in
  match space with
  | Some (Property _) | Some (Railroad _) | Some (Utility _) ->
      ownable_landing p g space_name r modRent mult
  | Some (Tax _) -> tax_landing p g real_space
  | Some Chance -> chance_landing g b p cards r
  | Some CommunityChest -> community_chest_landing g b p cards r
  | Some Quarantine -> quarantine_landing p
  | Some FreeParking -> free_parking_landing g p
  | Some GoToQuarantine -> goto_quarantine_landing p
  | _ -> ()

and ownable_landing p g space_name r modRent mult =
  let current_location = Player.get_location p in
  let rent =
    if modRent <> 0 then modRent * mult
    else Game.get_rent g current_location r * mult
  in
  match Game.owner g space_name with
  | Some player ->
      if player <> p && rent > 0 then (
        magenta_print "You must pay ";
        red_print (string_of_int rent);
        magenta_print "to ";
        cyan_print (Player.get_player_id player);
        if rent > Player.get_balance p then
          do_potential_bankruptcy g p rent player
        else Player.pay p player rent)
      else ()
  | None -> ()

and tax_landing p g space =
  match space with
  | Board.Tax t -> (
      magenta_print "Oh no! You landed on ";
      yellow_print t.name;
      magenta_print "You must pay ";
      red_print (string_of_int t.cost);
      try Game.do_tax g p space
      with Game.MustCheckBankrupt ->
        do_potential_bankruptcy g p t.cost p)
  | _ -> failwith "Should be a Tax"

and chance_landing game board player cards r =
  let location = Player.get_location player in
  magenta_print "You landed on Chance! Drawing a card...";
  let card = Cards.draw_chance_card cards in
  magenta_print "Your card says:";
  cyan_print card.message;
  try
    let modRent, mult = Cards.do_card card player board game in
    if Player.get_location player <> location then
      landing player game
        (Board.space_name board (Player.get_location player))
        r cards (modRent, mult)
    else ()
  with Cards.MustCheckBankrupt c ->
    do_potential_bankruptcy game player c player

and community_chest_landing g b p cards r =
  let location = Player.get_location p in
  magenta_print "You landed on Community Chest! Drawing a card...";
  let card = Cards.draw_community_chest_card cards in
  magenta_print "Your card says:";
  cyan_print card.message;
  let modRent, mult = Cards.do_card card p b g in
  if Player.get_location p <> location then
    landing p g
      (Board.space_name b (Player.get_location p))
      r cards (modRent, mult)
  else ()

and quarantine_landing p =
  match Player.quarantine p with
  | In _ ->
      magenta_print
        "you stare out your window and notice a couple on a nice \
         stroll.";
      magenta_print
        "you can't remember the last time you felt the wind... or \
         anything really"
  | Out -> magenta_print "You're just here for a visit... for now"

and free_parking_landing g p =
  let received = Game.do_free_parking g p in
  magenta_print "You landed on Free Parking! You get to collect ";
  green_print (string_of_int received)

and goto_quarantine_landing p =
  Player.go_to_quarantine_status p;
  magenta_print
    "Oh no! You tested positive and need to go into quarantine!"

(**[double_of_roll (a,b)] returns true if a and b are equal and false if
   not. *)
let double_of_roll (a, b) = a = b

(**[roll p b] returns a Legal result of the action representing a roll
   by player [p], given board [b]. *)
let rec roll p b g cards =
  let r = Player.roll () in
  magenta_print (string_of_roll r);
  if Player.passes_go r p then
    green_print "You passed go! You gained $200!";
  match Player.quarantine p with
  | In i ->
      if double_of_roll r then roll_in_quarantine_doubles p b g r cards
      else roll_in_quarantine_no_doubles p b g r cards i
  | Out -> roll_out_of_quarantine p b g r cards

and roll_in_quarantine_doubles p b g r cards =
  green_print
    "Congrats! you rolled doubles and can leave quarantine! (you \
     tested negative)";
  let new_space = Player.projected_space r p b in
  Player.leave_quarantine p;
  Legal
    {
      player_id = Player.get_player_id p;
      action =
        (fun player ->
          Player.move_player r player;
          landing player g new_space r cards (0, 1));
      is_double = false;
      is_end = false;
    }

and roll_in_quarantine_no_doubles p b g r cards i =
  red_print
    ("You can't move yet, you're still in quarantine for "
   ^ string_of_int i ^ " more turns.");
  Legal
    {
      player_id = Player.get_player_id p;
      action = (fun x -> Player.decrement_day_quarantine x);
      is_double = double_of_roll r;
      is_end = false;
    }

and roll_out_of_quarantine p b g r cards =
  let new_space = Player.projected_space r p b in
  magenta_print "You landed on: ";
  yellow_print new_space;
  Legal
    {
      player_id = Player.get_player_id p;
      action =
        (fun player ->
          Player.move_player r player;
          landing player g new_space r cards (0, 1));
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
              cyan_print (Player.get_player_id player)
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
                     this space"
              with
              | Game.NotOwnableSpace -> red_print "not ownable space"
              | Game.NotOwnableName -> red_print "not ownable name"
              | Board.NameNotOnBoard s -> red_print (s ^ " not on board")
              | _ -> red_print "somewhere else")
        with Game.NotOwnableName ->
          red_print
            "INFO: The space you are currently on cannot be bought")
    | None ->
        red_print
          "INFO: The space you are currently on cannot be bought"
  else red_print "INFO: The space you are currently on cannot be bought";

  if !legality then
    let ownable_space_name = Board.space_name b current_location in
    let price = Game.get_ownable_price b ownable_space_name in
    (* Gui.play_sound "buy.wav"; *)
    Legal
      {
        player_id = Player.get_player_id p;
        action =
          (fun p ->
            Player.buy_ownable p ownable_space_name price;
            Game.make_ownable_owned g p ownable_space_name;
            yellow_print
              ("You successfully purchased " ^ ownable_space_name ^ "!"));
        is_double = false;
        is_end = false;
      }
  else Illegal

let mortgagable_details ownable b =
  let details =
    " (You gain: "
    ^ string_of_int (Game.get_ownable_price b ownable / 2)
    ^ ")"
  in
  ownable ^ details

let mortgage p b g =
  white_print
    "Please type the number of the property you would like to \
     mortgage: ";
  yellow_print "Possible properties to mortgage: ";
  let mortgagables = Game.all_mortgagable g p in
  print_array (fun x -> mortgagable_details x b) mortgagables;
  if Array.length mortgagables = 0 then (
    green_print "None.";
    Legal
      {
        player_id = Player.get_player_id p;
        action = (fun _ -> ());
        is_double = false;
        is_end = false;
      })
  else (
    (*if len is zero then end function*)
    white_print "> ";
    white_print "";
    (* check out of bounds *)
    try
      let property_index = int_of_string (read_line ()) in
      let property_name = mortgagables.(property_index - 1) in
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
                cyan_print (Player.get_player_id player)
            | None ->
                red_print
                  "INFO: The property you are tyring to mortgage is \
                   not owned by any player"
          else
            red_print
              "INFO: The property you entered cannot be mortgaged"
      | None ->
          red_print "INFO: The property you entered cannot be mortgaged");
      if !legality then
        Legal
          {
            player_id = Player.get_player_id p;
            action =
              (* handle exceptions in the function *)
              (fun x ->
                Player.update_balance x
                  (Game.get_ownable_price b property_name / 2);
                Game.make_ownable_mortgaged g x property_name);
            is_double = false;
            is_end = false;
          }
      else Illegal
    with _ ->
      red_print "invalid input";
      Illegal)

let rec find_mortgaged g acc = function
  | [] -> acc
  | h :: t ->
      if Game.is_mortgaged g h then
        find_mortgaged g (Array.append acc [| h |]) t
      else find_mortgaged g acc t

let unmortgage_details ownable b =
  let details =
    " (You must pay: "
    ^ string_of_int
        (int_of_float
           (1.1 *. float_of_int (Game.get_ownable_price b ownable / 2)))
    ^ ")"
  in
  ownable ^ details

let rec unmortgage p b g =
  white_print
    "Please the number of the property you would like to buy back from \
     the bank: ";
  yellow_print "Possible properties to buy back: ";
  let owned = Player.get_ownable_name_list p in
  let mortgaged = find_mortgaged g [||] owned in
  print_array (fun x -> unmortgage_details x b) mortgaged;
  if Array.length mortgaged = 0 then unmortgage_none p b g
  else (
    white_print "> ";
    white_print "";
    unmortgage_some p b g mortgaged)

and unmortgage_some p b g mortgaged =
  try
    let property_index = int_of_string (read_line ()) in
    let property_name = mortgaged.(property_index - 1) in
    Legal
      {
        player_id = Player.get_player_id p;
        action =
          (fun x ->
            Game.make_ownable_owned g x property_name;
            Player.update_balance x
              (int_of_float
                 (1.1
                 *. float_of_int
                      (-Game.get_ownable_price b property_name / 2))));
        is_double = false;
        is_end = false;
      }
  with _ ->
    red_print "Something went wrong.";
    Illegal

and unmortgage_none p b g =
  green_print "None.";
  Legal
    {
      player_id = Player.get_player_id p;
      action = (fun _ -> ());
      is_double = false;
      is_end = false;
    }

let house_details ownable p g buy =
  let descriptor = if buy then "Cost" else "Value" in
  let num =
    if buy then Game.house_price g p ownable
    else Game.house_price g p ownable / 2
  in
  let details = " (" ^ descriptor ^ ": " ^ string_of_int num ^ ")" in
  ownable ^ details

let buy_sell_house p g buy =
  let prop_array =
    if buy then Game.all_can_buy_house g p
    else Game.all_can_sell_house g p
  in
  if Array.length prop_array = 0 then (
    green_print "None.";
    Legal
      {
        player_id = Player.get_player_id p;
        action = (fun _ -> ());
        is_double = false;
        is_end = false;
      })
  else (
    if buy then
      white_print
        "Please enter the number of the property you would like to buy \
         a house on: "
    else
      white_print
        "Please enter the number of the property you would like to buy \
         a house on: ";
    print_array (fun x -> house_details x p g buy) prop_array;
    white_print "> ";
    white_print "";
    let property_index = read_line () in
    try
      let property_name =
        prop_array.(int_of_string property_index - 1)
      in
      try
        if buy && Game.can_add_house g p property_name then
          Legal
            {
              player_id = Player.get_player_id p;
              action =
                (fun _ ->
                  Game.add_house g property_name true;
                  Player.update_balance p
                    (-Game.house_price g p property_name));
              is_double = false;
              is_end = false;
            }
        else if (not buy) && Game.can_sell_house g p property_name then
          Legal
            {
              player_id = Player.get_player_id p;
              action =
                (fun _ ->
                  Game.sell_house g property_name true;
                  Player.update_balance p
                    (Game.house_price g p property_name / 2));
              is_double = false;
              is_end = false;
            }
        else (
          if buy then
            red_print "you cannot add a house on this property"
          else red_print "you cannot sell a house on this property";
          Illegal)
      with Game.NotPropertyName ->
        red_print "The given name is not a valid property";
        Illegal
    with _ ->
      red_print "invalid input";
      Illegal)

let hotel_details ownable p g buy = house_details ownable p g buy

let buy_sell_hotel p g buy =
  (*need Game.property_list_of_ownable_list*)
  let prop_array =
    if buy then Game.all_can_buy_hotel g p
    else Game.all_can_sell_hotel g p
  in
  if Array.length prop_array = 0 then (
    green_print "None.";
    Legal
      {
        player_id = Player.get_player_id p;
        action = (fun _ -> ());
        is_double = false;
        is_end = false;
      })
  else (
    if buy then
      white_print
        "Please enter the number of the property you would like to buy \
         a hotel on: "
    else
      white_print
        "Please enter the number of the property you would like to \
         sell a hotel on: ";
    (*use Game.all_can_buy_house to print only those that can add a
      house*)
    print_array (fun x -> hotel_details x p g buy) prop_array;
    white_print "> ";
    white_print "";
    try
      let property_index = int_of_string (read_line ()) in
      let property_name = prop_array.(property_index - 1) in
      try
        if buy && Game.can_add_hotel g p property_name then
          Legal
            {
              player_id = Player.get_player_id p;
              action =
                (fun _ ->
                  Game.add_house g property_name false;
                  Player.update_balance p
                    (-Game.house_price g p property_name));
              is_double = false;
              is_end = false;
            }
        else if (not buy) && Game.can_sell_hotel g p property_name then
          Legal
            {
              player_id = Player.get_player_id p;
              action =
                (fun _ ->
                  Game.sell_house g property_name false;
                  Player.update_balance p
                    (Game.house_price g p property_name / 2));
              is_double = false;
              is_end = false;
            }
        else (
          if buy then
            red_print "you cannot add a hotel on this property"
          else red_print "you cannot sell a hotel on this property";
          Illegal)
      with Game.NotPropertyName ->
        red_print "The given name is not a valid property";
        Illegal
    with _ ->
      red_print "invalid input";
      Illegal)

let player_id_arr players =
  Array.map (fun x -> Player.get_player_id x) players

(* NOTE: Duplicated from player.ml, TODO: refactor *)
let rec contains elt = function
  | [] -> false
  | h :: t -> if elt = h then true else contains elt t

let rec select_trade_props p acc prop_array cur_list trading finished =
  let phrase = if trading then "offer to " else "receive from " in
  if not finished then (
    magenta_print "Your current offer is:";
    print_array (fun x -> x) (Array.of_list cur_list);
    if List.length cur_list = 0 then cyan_print "None" else ();
    white_print
      ("Please enter the number of a property you would like to "
     ^ phrase ^ Player.get_player_id p);
    white_print "Press enter with no input when you are finished.";
    print_array (fun x -> x) prop_array;
    if Array.length prop_array = 0 then cyan_print "None" else ();
    white_print "> ";
    white_print "";
    let property_index = read_line () in
    try
      if property_index = "" then
        select_trade_props p acc prop_array cur_list trading true
      else
        try
          let property =
            prop_array.(int_of_string property_index - 1)
          in
          if not (contains property acc) then
            select_trade_props p (property :: acc) prop_array
              (property :: cur_list) trading false
          else (
            red_print
              "You have already added this property to your offer.";
            select_trade_props p acc prop_array cur_list trading false)
        with _ ->
          red_print "invalid input";
          select_trade_props p acc prop_array cur_list trading false
    with _ -> acc)
  else acc

(* p1 is offering, p2 is receiving *)
let rec enter_cash p1 p2 =
  white_print
    "Please enter the amount of cash you would like to trade (negative \
     if you want to receive):";
  white_print "> ";
  white_print "";
  try
    let amt = int_of_string (read_line ()) in
    if amt > Player.get_balance p1 then (
      red_print "You cannot offer this amount of money.";
      enter_cash p1 p2)
    else if -amt > Player.get_balance p2 then (
      red_print (Player.get_player_id p2 ^ " cannot pay this amount.");
      enter_cash p1 p2)
    else amt
  with _ ->
    red_print "Please enter an integer.";
    enter_cash p1 p2

let print_trade_details p1 p2 receive_arr trade_arr cash is_counter =
  if is_counter then
    magenta_print
      (Player.get_player_id p1 ^ ", " ^ Player.get_player_id p2
     ^ " has given you a counteroffer.")
  else
    magenta_print
      (Player.get_player_id p1 ^ ", " ^ Player.get_player_id p2
     ^ " has offered you a trade.");
  white_print "You have been offered:";
  print_array (fun x -> x) receive_arr;
  if Array.length receive_arr = 0 then cyan_print "No Properties"
  else ();
  white_print "In exchange for:";
  print_array (fun x -> x) trade_arr;
  if Array.length trade_arr = 0 then cyan_print "No Properties" else ();
  let transfer = if cash > 0 then "receive " else "pay " in
  white_print ("You will " ^ transfer ^ "$" ^ string_of_int (abs cash))

let rec ask_yes_no () =
  print_array (fun x -> x) [| "Yes"; "No" |];
  white_print "";
  let accept = int_of_string (read_line ()) in
  if accept = 1 || accept = 2 then accept = 1
  else (
    red_print "Please enter either 1 or 2.";
    ask_yes_no ())

let rec trade p g =
  white_print
    "Please enter the number of the player you would like to trade \
     with:";

  let trade_partners = Game.get_all_players g in
  print_array (fun x -> x) (player_id_arr trade_partners);
  white_print "> ";
  white_print "";

  let partner_index = read_line () in
  try
    let partner = trade_partners.(int_of_string partner_index - 1) in
    if partner = p then
      red_print "Trading with yourself? I guess it's allowed ..."
    else ();
    let can_receive_array = Game.all_can_trade g partner in
    let receive_array =
      Array.of_list
        (select_trade_props partner [] can_receive_array [] false false)
    in
    let can_offer_array = Game.all_can_trade g p in
    let trade_array =
      Array.of_list
        (select_trade_props partner [] can_offer_array [] true false)
    in
    let cash = enter_cash p partner in
    let p2, p1, p1_swap, p2_swap, cash_swap =
      trade_counteroffer partner p g trade_array receive_array cash
        false
    in
    Legal
      {
        player_id = Player.get_player_id p;
        action =
          (fun _ ->
            Player.swap_properties p1 p2 (Array.to_list p1_swap);
            Player.swap_properties p2 p1 (Array.to_list p2_swap);
            Game.make_own_lst_owned g p1 (Array.to_list p2_swap);
            Game.make_own_lst_owned g p2 (Array.to_list p1_swap);
            Player.pay p1 p2 cash_swap);
        is_double = false;
        is_end = false;
      }
  with _ -> Illegal

(* returns (p1, p2, prop_list_p1, prop_list_p2, cash), whether or not
   trade was successful and swapped properties *)

(* p1 is the player receiving the trade, p2 is the person offering the
   trade*)
and trade_counteroffer p1 p2 g trade_arr receive_arr cash counter =
  print_trade_details p1 p2 trade_arr receive_arr cash counter;
  green_print "Would you like to accept this offer?";
  let accept = ask_yes_no () in
  if not accept then (
    cyan_print "Would you like to make a counter-offer?";
    if ask_yes_no () then
      let trade_offer =
        Array.of_list
          (select_trade_props p2 []
             (Game.all_can_trade g p1)
             [] true false)
      in
      let rec_offer =
        Array.of_list
          (select_trade_props p2 []
             (Game.all_can_trade g p2)
             [] false false)
      in
      let cash_amt = enter_cash p2 p1 in
      trade_counteroffer p2 p1 g trade_offer rec_offer cash_amt true
    else (p1, p2, [||], [||], 0))
  else (
    green_print "Congratulations! You've made a trade.";
    (p1, p2, trade_arr, receive_arr, cash))

let stock_details stock p g market buy =
  let descriptor = if buy then "Cost" else "Value" in
  let num = Stockmarket.value_of market stock in
  let details = " (" ^ descriptor ^ ": " ^ string_of_int num ^ ")" in
  stock ^ details

(**[buy_sell_stock p g market buy] returns the result type for a player
   attempting to buy or sell stocks on the stock market [market]. The
   player is attempting to buy if [buy] is true. [buy_sell_stock] acts
   for player [p] in game [g].*)
let rec buy_sell_stock p g market buy =
  let stock_array =
    if buy then Stockmarket.stock_array market
    else Array.map (fun (stock, _) -> stock) (Player.get_stocks p)
  in
  if Array.length stock_array = 0 then buy_sell_stock_none p
  else (
    print_ask g p market stock_array buy;
    try
      let stock_index = int_of_string (read_line ()) in
      let stock_name = stock_array.(stock_index - 1) in
      print_ask_shares g p market buy stock_name;
      let num_stocks = int_of_string (read_line ()) in
      let total_value =
        Stockmarket.value_of_num_shares market stock_name num_stocks
      in
      if buy && Player.get_balance p > total_value then
        buy_stock_move p stock_name num_stocks total_value
      else if not buy then
        sell_stock_move p stock_name num_stocks total_value
      else (
        red_print "you do not have enough money";
        Illegal)
    with _ ->
      red_print "invalid input";
      Illegal)

and buy_sell_stock_none p =
  green_print "None.";
  Legal
    {
      player_id = Player.get_player_id p;
      action = (fun _ -> ());
      is_double = false;
      is_end = false;
    }

and print_ask g p market stock_array buy =
  if buy then
    white_print
      "Please enter the number of the stock you would like to buy: "
  else
    white_print
      "Please enter the number of the stock you would like to sell: ";
  print_array (fun x -> stock_details x p g market buy) stock_array;
  white_print "> ";
  white_print ""

and print_ask_shares g p market buy stock_name =
  if buy then (
    white_print
      ("Please enter the number of " ^ stock_name
     ^ " you would like to buy:");
    white_print "")
  else (
    white_print
      ("Please enter the number of " ^ stock_name
     ^ " you would like to sell:");
    white_print "")

and buy_stock_move p stock_name num_stocks total_value =
  Legal
    {
      player_id = Player.get_player_id p;
      action =
        (fun _ -> Player.buy_stocks p stock_name num_stocks total_value);
      is_double = false;
      is_end = false;
    }

and sell_stock_move p stock_name num_stocks total_value =
  Legal
    {
      player_id = Player.get_player_id p;
      action =
        (fun _ ->
          try Player.sell_stocks p stock_name num_stocks total_value
          with Player.NotEnoughShares ->
            red_print "you do not have enough shares");
      is_double = false;
      is_end = false;
    }

let stock_printer p =
  let stocks = Player.get_stocks p in
  Array.iter
    (fun (stock, number) ->
      if number > 0 then
        green_print (stock ^ ". Shares: " ^ string_of_int number))
    stocks

(**[print_player_info b p] prints appropriate info about player [p]
   given board state [b]. *)
let print_player_info b p g =
  let player_bal = string_of_int (Player.get_balance p) in
  let player_loc = Board.space_name b (Player.get_location p) in
  cyan_print "Current balance: ";
  green_print (player_bal ^ "");
  cyan_print "Current properties: ";
  pp_propert_list g b (Player.get_ownable_name_list p);
  cyan_print "Stock portfolio:";
  stock_printer p;
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
let rec print_endgame b g =
  let players = Array.to_list (Game.get_all_players g) in
  let max_money = max players Player.get_balance in
  let max_properties =
    max players (fun p -> List.length (Player.get_ownable_name_list p))
  in
  let max_stocks =
    max players (fun p ->
        Array.fold_left
          (fun acc (_, n) -> acc + n)
          0 (Player.get_stocks p))
  in
  print_final_info b g max_money max_properties max_stocks

and print_final_info b g max_money max_properties max_stocks =
  magenta_print "Player with the most money (ties excluded): ";
  green_print (Player.get_player_id max_money);
  magenta_print "with ";
  green_print ("$" ^ (max_money |> Player.get_balance |> string_of_int));
  magenta_print "Player with the most properties (ties excluded): ";
  green_print (Player.get_player_id max_properties);
  magenta_print "with ";
  green_print
    (max_properties |> Player.get_ownable_name_list |> List.length
   |> string_of_int);
  magenta_print "properties.";
  magenta_print "Player with diamond hands (ties excluded): ";
  green_print (Player.get_player_id max_stocks);
  magenta_print "with ";
  green_print
    (max_stocks |> Player.get_stocks
    |> Array.fold_left (fun acc (_, n) -> acc + n) 0
    |> string_of_int);
  magenta_print "total shares."

(**[graceful_shutdown b g] ends the game [g] given board [b]. *)
let graceful_shutdown b g =
  red_print "Thanks for playing!";
  print_endgame b g;
  (* Gui.play_sound "gameover.wav"; *)
  Unix.sleep 10;
  exit 0

(**[turn_info b p phase] prints the information for player [p] on board
   [b] during phase [phase] of their turn. *)
let turn_info b p g phase =
  print_player_info b p g;
  cyan_print "possible moves: ";
  options_printer phase p b g;
  cyan_print ">";
  cyan_print ""

let function_of_move m p b g cards market =
  match m with
  | Roll -> roll p b g cards
  | Buy -> buy p b g
  | BuyHouse -> buy_sell_house p g true
  | BuyHotel -> buy_sell_hotel p g true
  | SellHouse -> buy_sell_house p g false
  | SellHotel -> buy_sell_hotel p g false
  | BuyStock -> buy_sell_stock p g market true
  | SellStock -> buy_sell_stock p g market false
  | Mortgage -> mortgage p b g
  | UnMortgage -> unmortgage p b g
  | Trade -> trade p g
  | End -> end_turn p b
  | Quit -> graceful_shutdown b g
  | Faulty -> Illegal

let turn p b g phase cards market =
  if phase then (
    cyan_print ("" ^ Player.get_player_id p ^ "'s turn.");
    let _ = turn_info b p g phase in
    ();
    try
      let input_index = int_of_string (read_line ()) in
      let move = phase1_options.(input_index - 1) in
      function_of_move move p b g cards market
    with _ ->
      red_print "Please enter a valid index.";
      Illegal)
  else
    let _ = turn_info b p g phase in
    ();
    try
      let input_index = int_of_string (read_line ()) in
      let move = phase2_options.(input_index - 1) in
      function_of_move move p b g cards market
    with _ ->
      red_print "Please enter a valid index.";
      Illegal
