type num_houses = int

type property_name = string

type status =
  | Owned of (num_houses * Player.t)
  | Mortgaged of Player.t

type property_info = 
  | Unavailable of status
  | Available

type t = {
  board: Board.t;
  players: Player.t array;
  mutable cur_player: int;
  properties: (property_name, property_info) Stdlib__hashtbl.t
}

let get_board t = t.board

let current_player t = t.players.(t.cur_player)

(*Note: Could delete this function, as players already have mutable fields *)
let update_player p t = 
  let current_index = t.cur_player in
  t.players.(current_index) <- p

let get_all_players t = t.players

let is_property = function
  | Board.Property t -> true
  | _ -> false

(*TODO: Add update hashtable method*)
let init_hashtbl hashtbl b = 
  let num_spaces = Board.length b in
  for i = 0 to num_spaces - 1 do
    let space = Board.space_from_location b i in
    let space_name = Board.space_name b i in
    if is_property space then Hashtbl.add hashtbl space_name Available 
    else ()
  done

let init_game b all_players = 
  let num_players = Array.length all_players in
  let all_props = Hashtbl.create num_players in
  init_hashtbl all_props b;
  {board = b; players = all_players; cur_player = 0; 
  properties = all_props}

let next_player t = 
  let player_amt = Array.length t.players in
  t.cur_player <- (t.cur_player + 1) mod player_amt
  