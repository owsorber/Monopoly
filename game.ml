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
  let num_players = Array.length all_players in
  let all_props = Hashtbl.create num_players in
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
        | Utility u -> acc + 1
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
        | Railroad r -> acc + 1
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
          | _ -> failwith "Ownable Status has Incorrect Ownable Type")
      | _ -> 0)
  | Utility status -> (
      match status with
      | U_Owned player ->
          let dice_sum = fst roll + snd roll in
          let both_utilities = has_both_utilities g player in
          if both_utilities then 10 * dice_sum else 7 * dice_sum
      | _ -> 0)
  | Railroad status -> (
      match status with
      | RR_Owned player -> 25 * num_rrs_owned g player
      | _ -> 0)

let make_ownable_owned g p o =
  match get_own_status g o with
  | Property _ ->
      Hashtbl.replace g.ownables o (Property (P_Owned (p, 0)))
  | Utility _ -> Hashtbl.replace g.ownables o (Utility (U_Owned p))
  | Railroad _ -> Hashtbl.replace g.ownables o (Railroad (RR_Owned p))

let can_mortgage g p o =
  match get_own_status g o with
  | Property status -> (
      match status with
      | P_Owned (player, houses) -> player = p && houses = 0
      | _ -> false)
  | Utility status -> (
      match status with U_Owned player -> player = p | _ -> false)
  | Railroad status -> (
      match status with RR_Owned player -> player = p | _ -> false)

let rec all_mortgagable_helper g acc p ownables =
  match ownables with
  | [] -> acc
  | h :: t ->
      if can_mortgage g p h then all_mortgagable_helper g (h :: acc) p t
      else all_mortgagable_helper g acc p t

let all_mortgagable g p =
  let ownables = Player.get_ownable_name_list p in
  all_mortgagable_helper g [] p ownables |> List.rev |> Array.of_list

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

let get_ownable_status g space =
  match space with
  | Board.Property p -> Some (get_own_status g p.name)
  | Utility u -> Some (get_own_status g u.name)
  | Railroad r -> Some (get_own_status g r.name)
  | _ -> None

let get_ownable_name own = failwith "Unimplemented"

let get_ownable_price own = failwith "Unimplemented"

let is_available t o =
  let own_status = get_own_status t o in
  match own_status with
  | Property p -> ( match p with P_Available -> true | _ -> false)
  | Railroad r -> ( match r with RR_Available -> true | _ -> false)
  | Utility u -> ( match u with U_Available -> true | _ -> false)

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
          | P_Available -> None)
      | Railroad r -> (
          match r with
          | RR_Owned player -> Some player
          | RR_Mortgaged player -> Some player
          | RR_Available -> None)
      | Utility u -> (
          match u with U_Owned player -> Some player | _ -> None))
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

(** [get_houses g name] returns the number of houses on the space with
    [ownable_name] name*)
let get_houses g name =
  let status = get_own_status g name in
  match status with
  | Property p -> (
      match p with P_Owned (player, houses) -> houses | _ -> 0)
  | Railroad r -> 0
  | Utility u -> 0

let has_houses_on_color g p col =
  let prop_list = get_properties g p in
  let rec find_house_with_col lst =
    match lst with
    | [] -> false
    | h :: t ->
        let is_color =
          if
            Board.color g.board (get_property_from_space_name g.board h)
            = col
          then true
          else false
        in
        if is_color && get_houses g h > 0 then true
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
let check_four_houses g prop_name = get_houses g prop_name < 4

(** [can_add_house p property_name] returns true iff player [p] can add
    a house on the property with name [property_name]. Raises:
    [NotPropertyName] if [property_name] is not a property name. *)
let can_add_house t player property_name =
  let space = get_property_from_space_name t.board property_name in
  let cur_space_color = Board.color t.board space in
  has_monopoly t player cur_space_color
  && t.houses_available > 0
  && check_four_houses t property_name
  && check_even_build t player property_name cur_space_color

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

let add_house t property_name =
  let property_space =
    Board.space_from_space_name t.board property_name
  in
  let property_check (space : Board.space option) =
    match space with
    | Some s -> (
        match s with Property p -> true | _ -> raise NotPropertyName)
    | _ -> raise NotPropertyName
  in
  if property_check property_space then
    Hashtbl.add t.ownables property_name
      (Property (new_property_house t property_name))

let can_add_hotel t p name = failwith "Unimplemented"

let add_hotel t name = failwith "Unimplemented"
