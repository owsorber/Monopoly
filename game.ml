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
  | U_Mortgaged of Player.t
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

exception CannotSellHouse of string

exception CannotSellHotel of string

exception MustCheckBankrupt

type t = {
  board : Board.t;
  mutable players : Player.t array;
  mutable curr_player : int;
  mutable free_parking : int;
  mutable houses_available : int;
  mutable hotels_available : int;
  mutable ownables : (ownable_name, ownable_status) Stdlib__hashtbl.t;
}

let get_board g = g.board

let current_player g = g.players.(g.curr_player)

let get_all_players g = g.players

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
    curr_player = 0;
    free_parking = 0;
    houses_available = 32;
    hotels_available = 12;
    ownables = all_props;
  }

let next_player g =
  let player_amt = Array.length g.players in
  g.curr_player <- (g.curr_player + 1) mod player_amt

let get_free_parking g = g.free_parking

let get_houses_available g = g.houses_available

let get_hotels_available g = g.hotels_available

let do_free_parking g p =
  let free_parking_val = get_free_parking g in
  Player.update_balance p free_parking_val;
  g.free_parking <- 0;
  free_parking_val

let get_own_status g o =
  match Hashtbl.find_opt g.ownables o with
  | None -> raise NotOwnableName
  | Some status -> status

let rec get_properties_helper g acc ownables =
  match ownables with
  | [] -> acc
  | h :: t -> (
      match get_own_status g h with
      | Property status -> get_properties_helper g (h :: acc) t
      | _ -> get_properties_helper g acc t)

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
        | Utility (U_Owned _) -> acc + 1
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
        | Railroad (RR_Owned _) -> acc + 1
        | _ -> acc
      in
      num_rrs_owned_helper game new_acc t

let num_rrs_owned game player =
  let ownables = Player.get_ownable_name_list player in
  num_rrs_owned_helper game 0 ownables

let make_ownable_owned g p o =
  match get_own_status g o with
  | Property _ ->
      Hashtbl.replace g.ownables o (Property (P_Owned (p, 0)))
  | Utility _ -> Hashtbl.replace g.ownables o (Utility (U_Owned p))
  | Railroad _ -> Hashtbl.replace g.ownables o (Railroad (RR_Owned p))

let rec make_own_lst_owned g p o_lst =
  match o_lst with
  | [] -> ()
  | h :: t ->
      make_ownable_owned g p h;
      make_own_lst_owned g p t

(* makes an ownable available, and happens when a player gets bankrupt *)
let make_ownable_available g p o =
  let space =
    match Board.space_from_space_name (get_board g) o with
    | Some s -> s
    | None -> failwith "This isn't a space name."
  in
  Hashtbl.replace g.ownables o (init_ownable space)

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
  | Some (Board.Property p) -> p.price
  | Some (Utility u) -> u.price
  | Some (Railroad r) -> r.price
  | _ -> raise NotOwnableSpace

let is_available g o =
  let own_status = get_own_status g o in
  match own_status with
  | Property P_Available | Railroad RR_Available | Utility U_Available
    ->
      true
  | _ -> false

let is_mortgaged g o =
  let own_status = get_own_status g o in
  match own_status with
  | Property (P_Mortgaged _)
  | Railroad (RR_Mortgaged _)
  | Utility (U_Mortgaged _) ->
      true
  | _ -> false

let owner g o =
  let available = is_available g o in
  let own_status = get_own_status g o in
  if available then None
  else
    match own_status with
    | Property (P_Owned (p, _))
    | Property (P_Mortgaged p)
    | Railroad (RR_Owned p)
    | Railroad (RR_Mortgaged p)
    | Utility (U_Owned p)
    | Utility (U_Mortgaged p) ->
        Some p
    | _ -> None

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

let has_monopoly g p col =
  Board.num_of_color g.board col = color_owned g p col

let get_houses g name =
  let status = get_own_status g name in
  match status with Property (P_Owned (_, houses)) -> houses | _ -> 0

let rec get_ownable_info g board ownable_name =
  match Board.space_from_space_name board ownable_name with
  | Some (Property _) -> get_property_info g board ownable_name
  | Some (Railroad _) | Some (Utility _) ->
      get_utility_rr_info g board ownable_name
  | _ -> ""

and get_property_info g board ownable_name =
  let is_mortgaged = string_of_bool (is_mortgaged g ownable_name) in
  let houses = get_houses g ownable_name in
  let num_houses = string_of_int (if houses > 4 then 4 else houses) in
  let has_hotel = string_of_bool (houses > 4) in
  ". Houses: " ^ num_houses ^ ". Hotel: " ^ has_hotel ^ ". Mortgaged: "
  ^ is_mortgaged

and get_utility_rr_info g board ownable_name =
  let is_mortgaged = string_of_bool (is_mortgaged g ownable_name) in
  ". Mortgaged: " ^ is_mortgaged

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

(** Checks for even build rule. [buying] is true iff even build is being
    checked for buying a property. Requires: player p has monopoly on
    col *)
let check_even_build g p prop_name col buying =
  let prop_list = get_properties g p in
  let house_acc = if buying then 1 else -1 in
  let rec get_house_list lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        let house_change = if h = prop_name then house_acc else 0 in
        if
          Board.color g.board (get_property_from_space_name g.board h)
          = col
        then get_house_list t ((get_houses g h + house_change) :: acc)
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

let house_price g p property_name =
  let cannot_add_exn = CannotAddHouse "Property Not Owned" in
  let property_house_price =
    match Board.space_from_space_name g.board property_name with
    | Some (Property prop) -> prop.house_price
    | _ -> raise NotPropertyName
  in
  match owner g property_name with
  | Some player ->
      if p = player then property_house_price else raise cannot_add_exn
  | _ -> raise cannot_add_exn

let can_afford g p name = Player.get_balance p > house_price g p name

let can_add_house g player property_name =
  let space = get_property_from_space_name g.board property_name in
  let col = Board.color g.board space in
  if not (has_monopoly g player col) then
    raise (CannotAddHouse "No Monopoly");
  if not (g.houses_available > 0) then
    raise (CannotAddHouse "No Houses Available");
  if not (check_less_houses g property_name 4) then
    raise (CannotAddHouse "4 Houses on Property");
  if not (check_even_build g player property_name col true) then
    raise (CannotAddHouse "Even Build");
  if not (check_no_mortgaged g player col) then
    raise (CannotAddHouse "Mortgaged Property on Color");
  if not (can_afford g player property_name) then
    raise (CannotAddHouse "Cannot afford House");
  true

(** Returns an updated property_status with an updated house amount *)
let new_property_house g property_name adding =
  let house_change = if adding then 1 else -1 in
  let cur_own_status = get_own_status g property_name in
  let cur_prop_status =
    match cur_own_status with
    | Property p -> p
    | _ -> raise NotPropertyName
  in
  let upd_prop_status =
    match cur_prop_status with
    | P_Owned (a, b) -> (a, b + house_change)
    | _ -> failwith "Impossible: Precondition Violation"
  in
  P_Owned upd_prop_status

let add_house_step g = g.houses_available <- g.houses_available - 1

let add_hotel_step g =
  g.houses_available <- g.houses_available + 4;
  g.hotels_available <- g.hotels_available - 1

let sell_house_step g = g.houses_available <- g.houses_available + 1

let sell_hotel_step g = g.hotels_available <- g.hotels_available + 1

let verify_property_space g name =
  match Board.space_from_space_name g.board name with
  | Some (Property _) -> ()
  | _ -> raise NotPropertyName

let add_house g property_name adding_house =
  verify_property_space g property_name;
  if adding_house then add_house_step g else add_hotel_step g;
  Hashtbl.replace g.ownables property_name
    (Property (new_property_house g property_name true))

let sell_house g property_name selling_house =
  verify_property_space g property_name;
  if selling_house then sell_house_step g else sell_hotel_step g;
  Hashtbl.replace g.ownables property_name
    (Property (new_property_house g property_name false))

(** requires player p has a monopoly on col *)
let has_full_monopoly g p col =
  let prop_list = get_properties g p in
  let rec check_four_houses p =
    match p with
    | [] -> true
    | h :: t ->
        let correct_color = is_color g h col in
        let incorrect_houses = check_less_houses g h 4 in
        if correct_color && incorrect_houses then false
        else check_four_houses t
  in
  check_four_houses prop_list

(** check if one hotel (five houses) on property before start, have four
    or more houses on each property in that col *)
let can_add_hotel g p name =
  let space = get_property_from_space_name g.board name in
  let col = Board.color g.board space in
  if not (has_monopoly g p col) then
    raise (CannotAddHotel "No Monopoly");
  if not (has_full_monopoly g p col) then
    raise (CannotAddHotel "Does Not Own Four Houses");
  if not (check_less_houses g name 5) then
    raise (CannotAddHotel "Already Owns Hotel on Property");
  if not (g.hotels_available > 0) then
    raise (CannotAddHotel "No Hotels Available");
  if not (can_afford g p name) then
    raise (CannotAddHotel "Cannot afford Hotel");
  true

(* checks that there is less than 4 and greater than 0 houses on that
   property, as well as even build *)
let can_sell_house g p name =
  let space = get_property_from_space_name g.board name in
  let col = Board.color g.board space in
  if not (check_even_build g p name col false) then
    raise (CannotSellHouse "Even Build");
  if not (check_less_houses g name 5) then
    raise (CannotSellHouse "Hotel on Property");
  if not (get_houses g name > 0) then
    raise (CannotSellHouse "No houses on Property");
  true

(* checks that a hotel is on property name, assumes player owns name *)
let can_sell_hotel g p name =
  if get_houses g name = 5 then true
  else raise (CannotSellHotel "No hotel on Property")

let can_mortgage g p o =
  let board = get_board g in
  let space =
    match Board.space_from_space_name board o with
    | None -> raise NotOwnableName
    | Some s -> s
  in
  (match get_own_status g o with
  | Property (P_Owned (player, houses)) ->
      let col = Board.color board space in
      player = p && houses = 0 && not (has_houses_on_color g p col)
  | Utility (U_Owned player) | Railroad (RR_Owned player) -> player = p
  | _ -> false)
  && get_ownable_price board o / 2 < Player.get_balance p

let can_trade g p name =
  match get_own_status g name with
  | Property (P_Owned (player, houses)) -> houses = 0
  | _ -> false

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

let all_can_sell_house g p =
  let ownables = Player.get_ownable_name_list p in
  all_ownables_helper g p can_sell_house [] ownables
  |> List.rev |> Array.of_list

let all_can_sell_hotel g p =
  let ownables = Player.get_ownable_name_list p in
  all_ownables_helper g p can_sell_hotel [] ownables
  |> List.rev |> Array.of_list

let all_can_trade g p =
  let ownables = Player.get_ownable_name_list p in
  all_ownables_helper g p can_trade [] ownables
  |> List.rev |> Array.of_list

let make_ownable_mortgaged g p o =
  let mortgagable = can_mortgage g p o in
  if mortgagable then
    match get_own_status g o with
    | Property _ ->
        Hashtbl.replace g.ownables o (Property (P_Mortgaged p))
    | Railroad _ ->
        Hashtbl.replace g.ownables o (Railroad (RR_Mortgaged p))
    | Utility _ ->
        Hashtbl.replace g.ownables o (Utility (U_Mortgaged p))
  else raise MortgageFailure

let rec get_rent g board_location roll =
  let board = get_board g in
  let space = Board.space_from_location board board_location in
  let o = Board.space_name board board_location in
  let o_status =
    try get_own_status g o
    with NotOwnableName -> raise NotOwnableSpace
  in
  match o_status with
  | Property status -> get_property_rent g status space
  | Utility status -> get_utility_rent g status roll
  | Railroad status -> get_railroad_rent g status roll

and get_property_rent g status space =
  match status with
  | P_Owned (player, houses) -> (
      match space with
      | Board.Property p ->
          if houses = 0 && has_monopoly g player p.color then
            (* double rent for monopoly with zero houses *)
            2 * p.rent.(0)
          else p.rent.(houses)
      | _ -> failwith "Ownable Status has Incorrect Ownable Type")
  | _ -> 0

and get_utility_rent g status roll =
  match status with
  | U_Owned player ->
      let dice_sum = fst roll + snd roll in
      let both_utilities = has_both_utilities g player in
      if both_utilities then 10 * dice_sum else 4 * dice_sum
  | _ -> 0

and get_railroad_rent g status roll =
  match status with
  | RR_Owned player -> 25 * num_rrs_owned g player
  | _ -> 0

(* makes all of a player's ownables available *)
let rec make_player_ownables_available g p = function
  | [] -> ()
  | h :: t ->
      make_ownable_available g p h;
      make_player_ownables_available g p t

let rec net_worth_helper acc g ownables =
  let board = get_board g in
  match ownables with
  | [] -> acc
  | h :: t ->
      if is_mortgaged g h then net_worth_helper acc g t
      else
        let worth = calc_worth g board h in
        net_worth_helper (acc + worth) g t

and calc_worth g board o =
  match get_own_status g o with
  | Property (P_Owned (player, houses)) ->
      let house_price = house_price g player o in
      (houses * house_price / 2) + (get_ownable_price board o / 2)
  | Utility (U_Owned player) -> get_ownable_price board o / 2
  | Railroad (RR_Owned player) -> get_ownable_price board o / 2
  | _ -> raise NotOwnableName

let get_net_worth g p =
  Player.get_ownable_name_list p
  |> net_worth_helper (Player.get_balance p) g

(* sells all houses on a property, and updates available fields.
   Requires: [name] is a property owned by p *)
let sell_houses_on_prop g p name =
  let houses = get_houses g name in
  if houses < 5 then g.houses_available <- g.houses_available + houses
  else g.hotels_available <- g.hotels_available + 1;
  Hashtbl.replace g.ownables name (Property (P_Owned (p, 0)))

(* sells all houses/hotels owned by player [p], doesn't update balance *)
let rec sell_all_houses g p properties =
  match properties with
  | [] -> ()
  | h :: t ->
      sell_houses_on_prop g p h;
      sell_all_houses g p t

let sell_all g p =
  Player.update_balance p (get_net_worth g p);
  let ownables = Player.get_ownable_name_list p in
  let properties = get_properties g p in
  sell_all_houses g p properties;
  let rec mortgage_all lst =
    match lst with
    | [] -> ()
    | h :: t ->
        make_ownable_mortgaged g p h;
        mortgage_all t
  in
  mortgage_all ownables

let goes_bankrupt g p cost = get_net_worth g p < cost

let delete_player g p =
  let players_lst = Array.to_list g.players in
  let new_players_lst = List.filter (fun x -> x <> p) players_lst in
  let new_players_array = Array.of_list new_players_lst in
  make_player_ownables_available g p (Player.get_ownable_name_list p);
  g.players <- new_players_array

let player_exists g p =
  let player_array = g.players in
  Array.exists (fun x -> x == p) player_array

let do_tax g p s =
  match s with
  | Board.Tax t ->
      let tax_cost = t.cost in
      (try Player.update_balance p (-tax_cost)
       with Player.BalanceBelowZero -> raise MustCheckBankrupt);
      g.free_parking <- g.free_parking + tax_cost
  | _ -> failwith "Tried to complete a tax on a non-tax space."
