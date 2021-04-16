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

(*TODO: Add update hashtable method*)
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

let can_add_house player property_name = failwith "Unimplemented"

let add_house property_name = failwith "Unimplemented"

let is_available o = failwith "Unimplemented"

let owner o = failwith "Unimplemented"

let has_monopoly p col = failwith "Unimplemented"

let has_houses_on_color p col = failwith "Unimplemented"
