type num_houses = int

type ownable_name = string

type property_status =
  | Owned of (Player.t * num_houses)
  | Mortgaged of Player.t
  | Available

type rr_status =
  | Owned of Player.t
  | Mortgaged of Player.t
  | Available

type util_status =
  | Owned of Player.t
  | Available

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
  ownables : (ownable_name, ownable_status) Stdlib__hashtbl.t;
}

let get_board t = t.board

let current_player t = t.players.(t.cur_player)

let get_all_players t = t.players

let init_ownable (space : Board.space) =
  match space with
  | Board.Property p -> Property Available
  | Board.Railroad r -> Railroad Available
  | Board.Utility u -> Utility Available
  | _ -> raise NotOwnableSpace

let init_hashtbl hashtbl b =
  let num_spaces = Board.length b in
  for i = 0 to num_spaces - 1 do
    let space = Board.space_from_location b i in
    let space_name = Board.space_name b i in
    if Board.is_ownable space then
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

let get_rent o roll = failwith "Unimplemented"

let make_ownable_owned p o = failwith "Unimplemented"

let make_ownable_mortgaged p o = failwith "Unimplemented"

let all_mortgagable p = failwith "Unimplemented"

let get_ownable space = failwith "Unimplemented"

let get_ownable_name own = failwith "Unimplemented"

let get_ownable_price own = failwith "Unimplemented"

let get_own_status t o =
  match Hashtbl.find_opt t.ownables o with
  | None -> raise NotOwnableName
  | Some status -> status

let is_available t o =
  let own_status = get_own_status t o in
  match own_status with
  | Property p -> ( match p with Available -> true | _ -> false )
  | Railroad r -> ( match r with Available -> true | _ -> false )
  | Utility u -> ( match u with Available -> true | _ -> false )

let owner t o =
  let available = is_available t o in
  let own_status = get_own_status t o in
  match available with
  | false -> (
      match own_status with
      | Property p -> (
          match p with
          | Owned (player, houses) -> Some player
          | Mortgaged player -> Some player
          | Available -> None )
      | Railroad r -> (
          match r with
          | Owned player -> Some player
          | Mortgaged player -> Some player
          | Available -> None )
      | Utility u -> (
          match u with Owned player -> Some player | _ -> None ) )
  | true -> None

(** Gets the property (space) from a ownable name*)
let get_property_from_space_name board name =
  match Board.space_from_space_name board name with
  | Some s -> s
  | None -> raise NotPropertyName

(** Returns how many properties of [color] col owned by the player*)
let color_owned g p col =
  let prop_list = Player.get_property_name_list p in
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
      match p with Owned (player, houses) -> houses | _ -> 0 )
  | Railroad r -> 0
  | Utility u -> 0

let has_houses_on_color g p col =
  let prop_list = Player.get_property_name_list p in
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
  let prop_list = Player.get_property_name_list p in
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
    | Owned (a, b) -> (a, b + 1)
    | _ -> failwith "Impossible: Precondition Violation"
  in
  Owned upd_prop_status

let add_house t property_name =
  let property_space =
    Board.space_from_space_name t.board property_name
  in
  let property_check (space : Board.space option) =
    match space with
    | Some s -> (
        match s with Property p -> true | _ -> raise NotPropertyName )
    | _ -> raise NotPropertyName
  in
  if property_check property_space then
    Hashtbl.add t.ownables property_name
      (Property (new_property_house t property_name))

let can_add_hotel t p name = failwith "Unimplemented"

let add_hotel t name = failwith "Unimplemented"
