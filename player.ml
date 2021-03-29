type player_id = string

type balance = int

type location = int

type property_name_list = string list

type rolled_dice = int * int

exception BalanceBelowZero

type t = { player_id: player_id; mutable balance: balance; 
  mutable location : location; mutable property_name_list : property_name_list}

let get_player_id player = player.player_id

let get_balance player = player.balance

let get_location player = player.location

let get_property_name_list player = player.property_name_list

let make_player id = {
  player_id = id;
  balance = 1500;
  location = 0;
  property_name_list = []
}

(**[sums roll] is the sum of the two die in [roll]*)
let sums roll = fst roll + snd roll

let roll () = ((Random.self_init (); Random.int 5 + 1), 
                (Random.self_init (); Random.int 5 + 1))

let update_balance player i = 
  if player.balance + i < 0 then raise BalanceBelowZero
  else player.balance <- player.balance + i

let passes_go roll player =
  player.location + sums roll >= 40

let move_player roll player =
  if passes_go roll player then player.balance <- player.balance + 200;
  let new_pos = (player.location + sums roll) mod 40 in
  player.location <- new_pos;