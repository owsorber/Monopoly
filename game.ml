type num_houses = int

type property_name = string

type status =
  | Owned of (num_houses * Player.t)
  | Mortgaged of Player.t

type property_info = {
  own_status: status;
  available: bool
}

type t = {
  board: Board.t;
  players: Player.t array;
  cur_player: int;
  properties: (property_name, property_info) Stdlib__hashtbl.t
}

let get_board t = t.board

let current_player t = t.players.(t.cur_player)

let update_player p t = 
  let current_index = t.cur_player in
  t.players.(current_index) <- p

let get_all_players t = t.players

(*TODO: Add an argument [names], which is a list of player names, and 
  initialize [player] array accordingly *)
(*TODO: Initialize properties hashtable*)
let init_game b n = 
  let all_players = Array.make n (Player.make_player "") in
  {board = b; players = all_players; cur_player = 0; 
  properties = Hashtbl.create n}

let next_player t = {t with cur_player = t.cur_player + 1}