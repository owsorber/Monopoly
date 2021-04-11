type num_houses = int

type ownable_name = string

type house_rr_status =
  | Owned of (num_houses * Player.t)
  | Mortgaged of Player.t
  | Available

type util_status =
  | Owned of Player.t
  | Available

type ownable =
  | Property of house_rr_status
  | Railroad of house_rr_status
  | Utility of util_status

type t = {
  board : Board.t;
  players : Player.t array;
  mutable cur_player : int;
  mutable free_parking : int;
  ownable_spaces : (ownable_name, ownable) Stdlib__hashtbl.t;
}

let get_board t = t.board

let current_player t = t.players.(t.cur_player)

(*Note: Could delete this function, as players already have mutable
  fields *)
let update_player p t =
  let current_index = t.cur_player in
  t.players.(current_index) <- p

let get_all_players t = t.players

(* let is_property = function Board.Property t -> true | _ -> false *)

let init_ownable (space : Board.space) =
  match space with
  | Property p -> Property Available
  | Railroad r -> Railroad Available
  | Utility u -> Utility Available
  | _ -> Property Available

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
    ownable_spaces = all_props;
  }

let next_player t =
  let player_amt = Array.length t.players in
  t.cur_player <- (t.cur_player + 1) mod player_amt

let get_free_parking t = t.free_parking
