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

let get_own_status t o =
  match Hashtbl.find_opt t.ownables o with
  | None -> raise NotOwnableName
  | Some status -> status

(** [is_available o] returns true iff the ownable with name [o] is
    available. Raises: [NotOwnableName] if [o] does not correspond to an
    ownable. *)
let is_available t o =
  let own_status = get_own_status t o in
  match own_status with
  | Property p -> ( match p with Available -> true | _ -> false )
  | Railroad r -> ( match r with Available -> true | _ -> false )
  | Utility u -> ( match u with Available -> true | _ -> false )

(** [owner o] returns Some Player.t if the ownable with name [o] is
    owned by a player and None if it available. Raises: [NotOwnableName]
    if [o] does not correspond to an ownable. *)
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

let color_owned t p col =
  (* each player has a property list, so we can iterate through the list
    and check how many of the colors they have.
    Question: how do we check if a property_name has x color?*)


(* [has_monopoly p col] returns true iff player [p] has a monopoly on
   the color group [col] i.e. [p] owns every property with color [col]. *)
let has_monopoly t p col = 
  let num_col = Board.num_of_color t.board col in


(* [has_houses_on_color] returns true iff player [p] has any houses on
   any properties with color [col]. *)
let has_houses_on_color t p col = failwith "Unimplemented"

(* [can_add_house p property_name] returns true iff player [p] can add a
   house on the property with name [property_name]. Raises:
   [NotPropertyName] if [property_name] is not a property name. *)
let can_add_house t player property_name = failwith "Unimplemented"

(** [add_house p] adds a house to the property with name [p]. Requires:
    [p] is a property name and a house can be added to it. Raises:
    [NotPropertyName] if [p] does not correspond to a property. *)
let add_house t property_name = failwith "Unimplemented"
