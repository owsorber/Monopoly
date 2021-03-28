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
  mutable cur_player: int;
  properties: (property_name, property_info) Stdlib__hashtbl.t
}

let get_board t = t.board

let current_player t = t.players.(t.cur_player)

let update_player p t = 
  let current_index = t.cur_player in
  t.players.(current_index) <- p

let get_all_players t = t.players

(*TODO: Initialize properties hashtable*)
let init_game b names = 
  let num_players = List.length names in
  let all_players = Array.make num_players (Player.make_player "") in
  let rec init_players names index =
    match names with 
    | [] -> ()
    | h :: t -> 
      all_players.(index) <- Player.make_player h;
      init_players t (index + 1)
  in
  init_players names 0;
  {board = b; players = all_players; cur_player = 0; 
  properties = Hashtbl.create num_players}

let next_player t = 
  let player_amt = Array.length t.players in
  t.cur_player <- (t.cur_player + 1) mod player_amt
  