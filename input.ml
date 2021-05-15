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
  | SellHouse
  | SellHotel
  | Mortgage
  | UnMortgage
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
  | SellHouse -> "Sell House"
  | SellHotel -> "Sell Hotel"
  | Mortgage -> "Mortgage"
  | UnMortgage -> "Unmortgage"
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
      yellow_print (string_of_int (i + 1));
      white_print (": " ^ elem_printer elem))
    a

let phase2_string_of_move m p b g =
  let move_str = string_of_move m in
  let details =
    match m with
    | Buy ->
        let current_location = Player.get_location p in
        let current_space =
          Board.space_from_location b current_location
        in
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
    | _ -> ""
  in
  move_str ^ details

(** [options_printer phase] is the string representation of the options
    a player can take during phase 1 if [phase] is true or phase 2 if
    [phase] is false. *)
let options_printer phase p b g =
  yellow_print
    "Please enter the number of the action you would like to take:\n";
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
      | _ -> property ^ Game.get_ownable_info g b property )
  | None -> "impossible"

let pp_propert_list g b lst =
  let pp_elts lst =
    let rec loop acc = function
      | [] -> acc
      | h1 :: h2 :: t ->
          loop (acc ^ property_info h1 g b ^ "\n") (h2 :: t)
      | h :: t -> loop (acc ^ property_info h g b) t
    in
    loop "" lst
  in
  "[" ^ pp_elts lst ^ "]"

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

let rec landing p g space_name r cards (modRent, mult) =
  let b = Game.get_board g in
  match Board.space_from_space_name b space_name with
  | Some space -> (
      match space with
      | Property _ | Railroad _ | Utility _ ->
          let current_location = Player.get_location p in
          let rent =
            if modRent <> 0 then modRent * mult
            else Game.get_rent g current_location r * mult
          in

          if rent > 0 then
            match Game.owner g space_name with
            | Some player ->
                if player <> p then
                  if rent > Player.get_balance p then (
                    red_print
                      "You do not have enough money to pay for rent! \
                       You go bankrupt";
                    (* cyan_print (Player.get_player_id player); *)
                    print_endline "";
                    Game.delete_player g p )
                  else (
                    Player.pay p player rent;
                    magenta_print "You must pay ";
                    red_print (string_of_int rent);
                    magenta_print " to ";
                    cyan_print (Player.get_player_id player);
                    print_endline "" )
                else ()
            | None -> ()
          else ()
      | Tax t ->
          Game.do_tax g p space;
          magenta_print "Oh no! You landed on ";
          yellow_print t.name;
          magenta_print ". You must pay ";
          red_print (string_of_int t.cost);
          print_endline ""
      | Chance ->
          let location = Player.get_location p in
          magenta_print "You landed on Chance!\nDrawing a card...\n";
          let card = Cards.draw_chance_card cards in
          magenta_print "Your card says:\n";
          cyan_print card.message;
          print_endline "\n";
          let modRent, mult = Cards.do_card card p b g in
          if Player.get_location p <> location then
            landing p g
              (Board.space_name b (Player.get_location p))
              r cards (modRent, mult)
          else ()
      | CommunityChest ->
          let location = Player.get_location p in
          magenta_print
            "You landed on Community Chest!\nDrawing a card...\n";
          let card = Cards.draw_community_chest_card cards in
          magenta_print "Your card says:\n";
          cyan_print card.message;
          print_endline "\n";
          let modRent, mult = Cards.do_card card p b g in
          if Player.get_location p <> location then
            landing p g
              (Board.space_name b (Player.get_location p))
              r cards (modRent, mult)
          else ()
      | Quarantine -> (
          match Player.quarantine p with
          | In _ ->
              magenta_print
                "you stare out your window and notice a couple on a \
                 nice stroll. you can't remember the last time you \
                 felt the wind... or anything really\n"
          | Out ->
              magenta_print "You're just here for a visit... for now\n"
          )
      | FreeParking ->
          let received = Game.do_free_parking g p in
          magenta_print
            "You landed on Free Parking! You get to collect ";
          green_print (string_of_int received);
          print_endline ""
      | GoToQuarantine ->
          Player.go_to_quarantine_status p;
          magenta_print
            "Oh no! You tested positive and need to go into quarantine!\n"
      | Go -> () )
  | None -> ()

(**[double_of_roll (a,b)] returns true if a and b are equal and false if
   not. *)
let double_of_roll (a, b) = if a = b then true else false

(**[roll p b] returns a Legal result of the action representing a roll
   by player [p], given board [b]. *)
let roll p b g cards =
  let r = Player.roll () in
  magenta_print (string_of_roll r);
  if Player.passes_go r p then
    green_print "You passed go! You gained $200!\n";
  match Player.quarantine p with
  | In i ->
      if double_of_roll r then (
        green_print
          "congrats! you rolled doubles and can leave quarantine! (you \
           tested negative)\n";
        Player.leave_quarantine p;
        Legal
          {
            player_id = Player.get_player_id p;
            action =
              (fun player ->
                Player.move_player r player;
                landing player g
                  (Player.projected_space r p b)
                  r cards (0, 1));
            is_double = double_of_roll r;
            is_end = false;
          } )
      else (
        red_print
          ( "You can't move yet, you're still in quarantine for "
          ^ string_of_int i ^ " more turns.\n" );
        Legal
          {
            player_id = Player.get_player_id p;
            action = (fun x -> Player.decrement_day_quarantine x);
            is_double = double_of_roll r;
            is_end = false;
          } )
  | Out ->
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
              | _ -> red_print "somewhere else\n" )
        with Game.NotOwnableName ->
          red_print
            "INFO: The space you are currently on cannot be bought\n" )
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
            Game.make_ownable_owned g p ownable_space_name;
            yellow_print
              ( "You successfully purchased " ^ ownable_space_name
              ^ "!\n" ));
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
    "Please the number of the property you would like to mortgage: ";
  yellow_print "Possible properties to mortgage: \n";
  let mortgagables = Game.all_mortgagable g p in
  print_array (fun x -> mortgagable_details x b) mortgagables;
  if Array.length mortgagables = 0 then (
    green_print "None.\n";
    Legal
      {
        player_id = Player.get_player_id p;
        action = (fun _ -> ());
        is_double = false;
        is_end = false;
      } )
  else (
    (*if len is zero then end function*)
    print_string "\n> ";
    let property_index = read_line () in
    (* check out of bounds *)
    try
      let property_name =
        mortgagables.(int_of_string property_index - 1)
      in
      let legality = ref false in

      ( match Board.space_from_space_name b property_name with
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
                  "INFO: The property you are tyring to mortgage is \
                   not owned by any player\n"
          else
            red_print
              "INFO: The property you entered cannot be mortgaged\n"
      | None ->
          red_print
            "INFO: The property you entered cannot be mortgaged\n" );
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
      red_print "invalid input\n";
      Illegal )

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

let unmortgage p b g =
  white_print
    "Please the number of the property you would like to buy back from \
     the bank: ";
  yellow_print "Possible properties to buy back: \n";
  let owned = Player.get_ownable_name_list p in
  let mortgaged = find_mortgaged g [||] owned in
  print_array (fun x -> unmortgage_details x b) mortgaged;
  if Array.length mortgaged = 0 then (
    green_print "None.\n";
    Legal
      {
        player_id = Player.get_player_id p;
        action = (fun _ -> ());
        is_double = false;
        is_end = false;
      } )
  else (
    print_string "\n> ";
    let property_index = read_line () in
    (* check out of bounds *)
    try
      let property_name =
        mortgaged.(int_of_string property_index - 1)
      in
      Legal
        {
          player_id = Player.get_player_id p;
          action =
            (fun x ->
              Game.make_ownable_owned g x property_name;
              Player.update_balance x
                (int_of_float
                   ( 1.1
                   *. float_of_int
                        (-Game.get_ownable_price b property_name / 2) )));
          is_double = false;
          is_end = false;
        }
    with _ ->
      red_print "Something went wrong.\n";
      Illegal )

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
    green_print "None.\n";
    Legal
      {
        player_id = Player.get_player_id p;
        action = (fun _ -> ());
        is_double = false;
        is_end = false;
      } )
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
    print_string "\n> ";
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
          Illegal )
      with Game.NotPropertyName ->
        red_print "The given name is not a valid property";
        Illegal
    with _ ->
      red_print "invalid input\n";
      Illegal )

let hotel_details ownable p g buy = house_details ownable p g buy

let buy_sell_hotel p g buy =
  (*need Game.property_list_of_ownable_list*)
  let prop_array =
    if buy then Game.all_can_buy_hotel g p
    else Game.all_can_sell_hotel g p
  in
  if Array.length prop_array = 0 then (
    green_print "None.\n";
    Legal
      {
        player_id = Player.get_player_id p;
        action = (fun _ -> ());
        is_double = false;
        is_end = false;
      } )
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
    print_string "\n> ";
    let property_index = read_line () in
    try
      let property_name =
        prop_array.(int_of_string property_index - 1)
      in
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
          Illegal )
      with Game.NotPropertyName ->
        red_print "The given name is not a valid property";
        Illegal
    with _ ->
      red_print "invalid input\n";
      Illegal )

let player_id_arr players =
  Array.map (fun x -> Player.get_player_id x) players

(* NOTE: Duplicated from player.ml, TODO: refactor *)
let rec contains elt = function
  | [] -> false
  | h :: t -> if elt = h then true else contains elt t

let rec select_trade_props p acc prop_array cur_list trading finished =
  let phrase = if trading then "offer to " else "receive from " in
  if not finished then (
    magenta_print "\nYour current offer is: \n";
    print_array (fun x -> x) (Array.of_list cur_list);
    if List.length cur_list = 0 then cyan_print "None \n" else ();
    white_print
      ( "\nPlease enter the number of a property you would like to "
      ^ phrase ^ Player.get_player_id p
      ^ ". Press enter with no input when you are finished." );
    print_array (fun x -> x) prop_array;
    if Array.length prop_array = 0 then cyan_print "None \n" else ();
    print_string "> ";
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
              "You have already added this property to your offer. \n";
            select_trade_props p acc prop_array cur_list trading false )
        with _ ->
          red_print "invalid input \n";
          select_trade_props p acc prop_array cur_list trading false
    with _ -> acc )
  else acc

(* p1 is offering, p2 is receiving *)
let rec enter_cash p1 p2 =
  white_print
    "Please enter the amount of cash you would like to trade (negative \
     if you want to receive): \n";
  print_string "> ";
  try
    let amt = int_of_string (read_line ()) in
    if amt > Player.get_balance p1 then (
      red_print "You cannot offer this amount of money. \n";
      enter_cash p1 p2 )
    else if -amt > Player.get_balance p2 then (
      red_print (Player.get_player_id p2 ^ " cannot pay this amount. \n");
      enter_cash p1 p2 )
    else amt
  with _ ->
    red_print "Please enter an integer. \n";
    enter_cash p1 p2

let print_trade_details p1 p2 receive_arr trade_arr cash is_counter =
  if is_counter then
    magenta_print
      ( Player.get_player_id p1 ^ ", " ^ Player.get_player_id p2
      ^ " has given you a counteroffer. \n" )
  else
    magenta_print
      ( Player.get_player_id p1 ^ ", " ^ Player.get_player_id p2
      ^ " has offered you a trade. \n" );
  white_print "You have been offered:";
  print_array (fun x -> x) receive_arr;
  if Array.length receive_arr = 0 then cyan_print "No Properties \n"
  else ();
  white_print "In exchange for:";
  print_array (fun x -> x) trade_arr;
  if Array.length trade_arr = 0 then cyan_print "No Properties \n"
  else ();
  let transfer = if cash > 0 then "receive " else "pay " in
  white_print
    ("\nYou will " ^ transfer ^ "$" ^ string_of_int (abs cash) ^ "\n")

let rec ask_yes_no () =
  print_array (fun x -> x) [| "Yes"; "No" |];
  let accept = int_of_string (read_line ()) in
  if accept = 1 || accept = 2 then if accept = 1 then true else false
  else (
    red_print "Please enter either 1 or 2. \n";
    ask_yes_no () )

let rec trade p g =
  white_print
    "Please enter the number of the player you would like to trade \
     with: \n";
  let trade_partners = Game.get_all_players g in
  print_array (fun x -> x) (player_id_arr trade_partners);
  print_string "> ";

  let partner_index = read_line () in
  try
    let partner = trade_partners.(int_of_string partner_index - 1) in
    if partner = p then
      red_print "Trading with yourself? I guess it's allowed ... \n"
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
  green_print "Would you like to accept this offer? \n";
  let accept = ask_yes_no () in
  if not accept then (
    cyan_print "Would you like to make a counter-offer? \n";
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
    else (p1, p2, [||], [||], 0) )
  else (
    green_print "Congratulations! You've made a trade. \n";
    (p1, p2, trade_arr, receive_arr, cash) )

(**[print_player_info b p] prints appropriate info about player [p]
   given board state [b]. *)
let print_player_info b p g =
  let player_bal = string_of_int (Player.get_balance p) in
  let player_props =
    pp_propert_list g b (Player.get_ownable_name_list p)
  in
  let player_loc = Board.space_name b (Player.get_location p) in
  cyan_print "Current balance: ";
  green_print (player_bal ^ "\n");
  cyan_print "Current properties: \n";
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
    ( max_properties |> Player.get_ownable_name_list |> List.length
    |> string_of_int );
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
  options_printer phase p b g;
  cyan_print "\n>"

let function_of_move m p b g cards =
  match m with
  | Roll -> roll p b g cards
  | Buy -> buy p b g
  | BuyHouse -> buy_sell_house p g true
  | BuyHotel -> buy_sell_hotel p g true
  | SellHouse -> buy_sell_house p g false
  | SellHotel -> buy_sell_hotel p g false
  | Mortgage -> mortgage p b g
  | UnMortgage -> unmortgage p b g
  | Trade -> trade p g
  | End -> end_turn p b
  | Quit -> graceful_shutdown b g
  | Faulty -> Illegal

(**[turn p b g phase cards] is the representation of a single response
   by the user for player [p], given board [b] and game [g]. *)
let turn p b g phase cards =
  match phase with
  | true -> (
      cyan_print ("\n" ^ Player.get_player_id p ^ "'s turn.\n");
      let _ = turn_info b p g phase in
      ();
      try
        let input_index = int_of_string (read_line ()) in
        let move = phase1_options.(input_index - 1) in
        function_of_move move p b g cards
      with _ ->
        red_print "Please enter a valid index.\n";
        Illegal )
  | false -> (
      let _ = turn_info b p g phase in
      ();
      try
        let input_index = int_of_string (read_line ()) in
        let move = phase2_options.(input_index - 1) in
        function_of_move move p b g cards
      with _ ->
        red_print "Please enter a valid index.\n";
        Illegal )
