type num_houses = int

type ownable_name = string

type property_status =
  | P_Owned of (Player.t * num_houses)
  | P_Mortgaged of Player.t
  | P_Available

type rr_status =
  | RR_Owned of Player.t
  | RR_Mortgaged of Player.t
  | RR_Available

type util_status =
  | U_Owned of Player.t
  | U_Available

type ownable_status =
  | Property of property_status
  | Railroad of rr_status
  | Utility of util_status

exception NotOwnableSpace

exception NotOwnableName

exception MortgageFailure

exception NotPropertyName

exception CannotAddHouse of string

exception CannotAddHotel of string

type t = {
  board : Board.t;
  players : Player.t array;
  mutable cur_player : int;
  mutable free_parking : int;
  mutable houses_available : int;
  mutable hotels_available : int;
  mutable ownables : (ownable_name, ownable_status) Stdlib__hashtbl.t;
}

let get_board t = t.board

let current_player t = t.players.(t.cur_player)

let get_all_players t = t.players

let init_ownable (space : Board.space) =
  match space with
  | Board.Property p -> Property P_Available
  | Board.Railroad r -> Railroad RR_Available
  | Board.Utility u -> Utility U_Available
  | _ -> raise NotOwnableSpace

let init_hashtbl hashtbl b =
  let num_spaces = Board.length b in
  for i = 0 to num_spaces - 1 do
    let space = Board.space_from_location b i in
    let space_name = Board.space_name b i in
    if Board.is_ownable b space then
      Hashtbl.add hashtbl space_name (init_ownable space)
    else ()
  done

let init_game b all_players =
  let all_props = Hashtbl.create (Board.length b) in
  init_hashtbl all_props b;
  {
    board = b;
    players = all_players;
    cur_player = 0;
    free_parking = 0;
    houses_available = 32;
    hotels_available = 12;
    ownables = all_props;
  }

let next_player t =
  let player_amt = Array.length t.players in
  t.cur_player <- (t.cur_player + 1) mod player_amt

let get_free_parking t = t.free_parking

let get_houses_available t = t.houses_available

let get_hotels_available t = t.hotels_available

let do_free_parking g p =
  let free_parking_val = get_free_parking g in
  Player.update_balance p free_parking_val;
  g.free_parking <- 0

let do_tax g p s =
  match s with
  | Board.Tax t ->
      let tax_cost = t.cost in
      Player.update_balance p (-tax_cost);
      g.free_parking <- g.free_parking + tax_cost
  | _ -> failwith "Tried to complete a tax on a non-tax space."

let get_own_status t o =
  match Hashtbl.find_opt t.ownables o with
  | None -> raise NotOwnableName
  | Some status -> status

let rec get_properties_helper g acc ownables =
  match ownables with
  | [] -> acc
  | h :: t -> (
      match get_own_status g h with
      | Property status -> get_properties_helper g (h :: acc) t
      | _ -> get_properties_helper g acc t )

(* Gets all the properties of player [p] in game [g]. *)
let get_properties g p =
  let ownables = Player.get_ownable_name_list p in
  get_properties_helper g [] ownables |> List.rev

(* Returns the number of utilities a player owns. *)
let rec has_both_utilities_helper game acc ownables =
  match ownables with
  | [] -> acc
  | h :: t ->
      let new_acc =
        match get_own_status game h with
        | Utility u -> (
            match u with U_Owned _ -> acc + 1 | _ -> acc + 1 )
        | _ -> acc
      in
      has_both_utilities_helper game new_acc t

let has_both_utilities game player =
  let ownables = Player.get_ownable_name_list player in
  let num_utilities = has_both_utilities_helper game 0 ownables in
  if num_utilities = 2 then true else false

(* Returns the number of railroads a player owns. *)
let rec num_rrs_owned_helper game acc ownables =
  match ownables with
  | [] -> acc
  | h :: t ->
      let new_acc =
        match get_own_status game h with
        | Railroad r -> (
            match r with RR_Owned _ -> acc + 1 | _ -> acc )
        | _ -> acc
      in
      num_rrs_owned_helper game new_acc t

let num_rrs_owned game player =
  let ownables = Player.get_ownable_name_list player in
  num_rrs_owned_helper game 0 ownables

let get_rent g board_location roll =
  let board = get_board g in
  let space = Board.space_from_location board board_location in
  let o = Board.space_name board board_location in
  let o_status =
    try get_own_status g o
    with NotOwnableName ->
      failwith
        "Get rent supplied a board location that isn't an ownable."
  in
  match o_status with
  | Property status -> (
      match status with
      | P_Owned (player, houses) -> (
          match space with
          | Board.Property p -> p.rent.(houses)
          | _ -> failwith "Ownable Status has Incorrect Ownable Type" )
      | _ -> 0 )
  | Utility status -> (
      match status with
      | U_Owned player ->
          let dice_sum = fst roll + snd roll in
          let both_utilities = has_both_utilities g player in
          if both_utilities then 10 * dice_sum else 4 * dice_sum
      | _ -> 0 )
  | Railroad status -> (
      match status with
      | RR_Owned player -> 25 * num_rrs_owned g player
      | _ -> 0 )

let make_ownable_owned g p o =
  match get_own_status g o with
  | Property _ ->
      Hashtbl.replace g.ownables o (Property (P_Owned (p, 0)))
  | Utility _ -> Hashtbl.replace g.ownables o (Utility (U_Owned p))
  | Railroad _ -> Hashtbl.replace g.ownables o (Railroad (RR_Owned p))

let get_ownable_status g space =
  match space with
  | Board.Property p -> Some (get_own_status g p.name)
  | Utility u -> Some (get_own_status g u.name)
  | Railroad r -> Some (get_own_status g r.name)
  | _ -> None

let get_ownable_price board own =
  let space = Board.space_from_space_name board own in
  match space with
  | None -> raise NotOwnableName
  | Some s -> (
      match s with
      | Board.Property p -> p.price
      | Utility u -> u.price
      | Railroad r -> r.price
      | _ -> raise NotOwnableSpace )

let is_available t o =
  let own_status = get_own_status t o in
  match own_status with
  | Property p -> ( match p with P_Available -> true | _ -> false )
  | Railroad r -> ( match r with RR_Available -> true | _ -> false )
  | Utility u -> ( match u with U_Available -> true | _ -> false )

let is_mortgaged t o =
  let own_status = get_own_status t o in
  match own_status with
  | Property p -> ( match p with P_Mortgaged _ -> true | _ -> false )
  | Railroad r -> ( match r with RR_Mortgaged _ -> true | _ -> false )
  | _ -> false

let owner t o =
  let available = is_available t o in
  let own_status = get_own_status t o in
  match available with
  | false -> (
      match own_status with
      | Property p -> (
          match p with
          | P_Owned (player, houses) -> Some player
          | P_Mortgaged player -> Some player
          | P_Available -> None )
      | Railroad r -> (
          match r with
          | RR_Owned player -> Some player
          | RR_Mortgaged player -> Some player
          | RR_Available -> None )
      | Utility u -> (
          match u with U_Owned player -> Some player | _ -> None ) )
  | true -> None

(** Gets the property (space) from a ownable name*)
let get_property_from_space_name board name =
  match Board.space_from_space_name board name with
  | Some s -> s
  | None -> raise NotPropertyName

(** Returns how many properties of [color] col owned by the player*)
let color_owned g p col =
  let prop_list = get_properties g p in
  let rec amt_color lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        if
          Board.color g.board (get_property_from_space_name g.board h)
          = col
        then amt_color t (acc + 1)
        else amt_color t acc
  in
  amt_color prop_list 0

let has_monopoly t p col =
  Board.num_of_color t.board col = color_owned t p col

let get_houses g name =
  let status = get_own_status g name in
  match status with
  | Property p -> (
      match p with P_Owned (player, houses) -> houses | _ -> 0 )
  | Railroad r -> 0
  | Utility u -> 0

let get_ownable_info g board ownable_name =
  match Board.space_from_space_name board ownable_name with
  | Some space -> (
      match space with
      | Property _ ->
          let is_mortgaged =
            string_of_bool (is_mortgaged g ownable_name)
          in
          let houses = get_houses g ownable_name in
          let num_houses =
            string_of_int (if houses > 4 then 4 else houses)
          in
          let has_hotel = string_of_bool (houses > 4) in
          ". Houses: " ^ num_houses ^ ". Hotel: " ^ has_hotel
          ^ ". Mortgaged: " ^ is_mortgaged
      | Railroad _ | Utility _ ->
          let is_mortgaged =
            string_of_bool (is_mortgaged g ownable_name)
          in
          ". Mortgaged: " ^ is_mortgaged
      | _ -> "" )
  | None -> ""

(** Checks if the property with name [h] in game [g] has color [col] *)
let is_color g h col =
  if Board.color g.board (get_property_from_space_name g.board h) = col
  then true
  else false

let has_houses_on_color g p col =
  let prop_list = get_properties g p in
  let rec find_house_with_col lst =
    match lst with
    | [] -> false
    | h :: t ->
        let correct_color = is_color g h col in
        if correct_color && get_houses g h > 0 then true
        else find_house_with_col t
  in
  find_house_with_col prop_list

(** Checks for even build rule. Requires: player p has monopoly on col *)
let check_even_build g p prop_name col =
  let prop_list = get_properties g p in
  let rec get_house_list lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        let added_house = if h = prop_name then 1 else 0 in
        if
          Board.color g.board (get_property_from_space_name g.board h)
          = col
        then get_house_list t ((get_houses g h + added_house) :: acc)
        else get_house_list t acc
  in
  let house_list = get_house_list prop_list [] in
  let max_list = List.fold_left (fun x y -> max x y) 0 house_list in
  let min_list = List.fold_left (fun x y -> min x y) 5 house_list in
  max_list - min_list <= 1

(** Checks that a player can't add another house to a property with four
    houses *)
let check_less_houses g prop_name num = get_houses g prop_name < num

(* Requires: player p has a monopoly on col*)
let check_no_mortgaged g p col =
  let prop_list = get_properties g p in
  let rec mortgaged_col p =
    match p with
    | [] -> true
    | h :: t ->
        let correct_color = is_color g h col in
        let mortgaged = is_mortgaged g h in
        if mortgaged && correct_color then false else mortgaged_col t
  in
  mortgaged_col prop_list

let can_add_house t player property_name =
  let space = get_property_from_space_name t.board property_name in
  let cur_space_color = Board.color t.board space in
  let check1 =
    if has_monopoly t player cur_space_color then true
    else raise (CannotAddHouse "No Monopoly")
  in
  let check2 =
    if t.houses_available > 0 then true
    else raise (CannotAddHouse "No Houses Available")
  in
  let check3 =
    if check_less_houses t property_name 4 then true
    else raise (CannotAddHouse "4 Houses on Property")
  in
  let check4 =
    if check_even_build t player property_name cur_space_color then true
    else raise (CannotAddHouse "Even Build")
  in
  let check5 =
    if check_no_mortgaged t player cur_space_color then true
    else raise (CannotAddHouse "Mortgaged Property on Color")
  in
  check1 && check2 && check3 && check4 && check5

(* next_house is true if we want the price of the next house and false
   if we want the price of a hotel *)
let next_house_price_helper g p property_name next_house =
  let space =
    match Board.space_from_space_name g.board property_name with
    | Some s -> s
    | None -> raise NotOwnableSpace
  in
  match get_own_status g property_name with
  | Property status -> (
      match status with
      | P_Owned (player, houses) -> (
          match space with
          | Board.Property p ->
              if next_house then p.rent.(houses + 1) else p.rent.(5)
          | _ -> failwith "Ownable Status has Incorrect Ownable Type" )
      | _ -> raise (CannotAddHouse "Property Not Owned") )
  | _ -> raise NotPropertyName

let next_house_price g p property_name =
  next_house_price_helper g p property_name true

let hotel_price g p property_name =
  next_house_price_helper g p property_name false

(** Returns an updated property_status with an additional house*)
let new_property_house t property_name : property_status =
  let cur_own_status = get_own_status t property_name in
  let cur_prop_status =
    match cur_own_status with
    | Property p -> p
    | _ -> raise NotPropertyName
  in
  let upd_prop_status =
    match cur_prop_status with
    | P_Owned (a, b) -> (a, b + 1)
    | _ -> failwith "Impossible: Precondition Violation"
  in
  P_Owned upd_prop_status

let house_step t = t.houses_available <- t.houses_available - 1

let hotel_step t =
  t.houses_available <- t.houses_available + 4;
  t.hotels_available <- t.hotels_available - 1

let add_house t property_name adding_house =
  let property_space =
    Board.space_from_space_name t.board property_name
  in
  let property_check (space : Board.space option) =
    match space with
    | Some s -> (
        match s with Property p -> true | _ -> raise NotPropertyName )
    | _ -> raise NotPropertyName
  in
  if adding_house then house_step t else hotel_step t;
  if property_check property_space then
    Hashtbl.replace t.ownables property_name
      (Property (new_property_house t property_name))

(** requires player p has a monopoly on col *)
let has_full_monopoly g p col =
  let prop_list = get_properties g p in
  let rec check_four_houses p =
    match p with
    | [] -> true
    | h :: t ->
        let correct_color = is_color g h col in
        let correct_houses =
          if check_less_houses g h 4 then false else true
        in
        if correct_color then
          if correct_houses then check_four_houses t else false
        else check_four_houses t
  in
  check_four_houses prop_list

(** check if one hotel (five houses) on property before start, have four
    or more houses on each property in that col *)
let can_add_hotel t p name =
  let space = get_property_from_space_name t.board name in
  let cur_space_color = Board.color t.board space in
  let check1 =
    if has_monopoly t p cur_space_color then true
    else raise (CannotAddHotel "No Monopoly")
  in
  let check2 =
    if has_full_monopoly t p cur_space_color then true
    else raise (CannotAddHotel "Does Not Own Four Houses")
  in
  let check3 =
    if check_less_houses t name 5 then true
    else raise (CannotAddHotel "Already Owns Hotel on Property")
  in
  let check4 =
    if t.hotels_available > 0 then true
    else raise (CannotAddHotel "No Hotels Available")
  in
  check1 && check2 && check3 && check4

(* let add_hotel t name = failwith "Unimplemented" *)

let can_mortgage g p o =
  let board = get_board g in
  let space =
    match Board.space_from_space_name board o with
    | None -> raise NotOwnableName
    | Some s -> s
  in
  match get_own_status g o with
  | Property status -> (
      match status with
      | P_Owned (player, houses) ->
          let col = Board.color board space in
          player = p && houses = 0 && not (has_houses_on_color g p col)
      | _ -> false )
  | Utility status -> false
  | Railroad status -> (
      match status with RR_Owned player -> player = p | _ -> false )

(* Helper to compile together all ownables satisfying a certain
   condition, such as: can be mortgaged, can have a house bought on it,
   can have a hotel bought on it. *)
let rec all_ownables_helper g p cond acc ownables =
  match ownables with
  | [] -> acc
  | h :: t ->
      (* if an exception thrown, set to false *)
      let satisfies_condition = try cond g p h with _ -> false in
      if satisfies_condition then
        all_ownables_helper g p cond (h :: acc) t
      else all_ownables_helper g p cond acc t

let all_mortgagable g p =
  let ownables = Player.get_ownable_name_list p in
  all_ownables_helper g p can_mortgage [] ownables
  |> List.rev |> Array.of_list

let all_can_buy_house g p =
  let ownables = Player.get_ownable_name_list p in
  all_ownables_helper g p can_add_house [] ownables
  |> List.rev |> Array.of_list

let all_can_buy_hotel g p =
  let ownables = Player.get_ownable_name_list p in
  all_ownables_helper g p can_add_hotel [] ownables
  |> List.rev |> Array.of_list

let make_ownable_mortgaged g p o =
  let mortgagable = can_mortgage g p o in
  if mortgagable then
    match get_own_status g o with
    | Property _ ->
        Hashtbl.replace g.ownables o (Property (P_Mortgaged p))
    | Railroad _ ->
        Hashtbl.replace g.ownables o (Railroad (RR_Mortgaged p))
    | _ -> raise MortgageFailure
  else raise MortgageFailure

let landing_on_space g p b r space_name =
  match Board.space_from_space_name b space_name with
  | Some space -> (
      match space with
      | Property _ | Railroad _ | Utility _ ->
          let current_location = Player.get_location p in
          let rent = get_rent g current_location r in
          if rent > 0 then
            match owner g space_name with
            | Some player ->
                if player <> p then (
                  Player.pay p player rent;
                  "You must pay " ^ string_of_int rent ^ " to "
                  ^ Player.get_player_id player
                  ^ "\n" )
                else ""
            | None -> ""
          else ""
      | Tax t ->
          Player.update_balance p (-t.cost);
          g.free_parking <- g.free_parking + t.cost;
          "Oh no! You landed on " ^ t.name ^ ". You must pay "
          ^ string_of_int t.cost ^ "\n"
      | Chance -> "You landed on Chance!\nDrawing a card...\n"
      | CommunityChest ->
          "You landed on Community Chest!\nDrawing a card...\n"
      | Quarantine -> "You're just here for a visit... for now\n"
      | FreeParking ->
          Player.update_balance p g.free_parking;
          g.free_parking <- 0;
          "You landed on Free Parking! You get to collect "
          ^ string_of_int g.free_parking
          ^ "\n"
      | GoToQuarantine ->
          Player.go_to_quarantine_status p;
          "Oh no! You tested positive and need to go into quarantine!\n"
      | Go -> "" )
  | None -> "something went wrong"
