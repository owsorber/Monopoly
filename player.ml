type player_id = string

type balance = int

type location = int

type property_name_list = string list

type rolled_dice = int * int

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

let roll () = ((Random.self_init (); Random.int 5 + 1), (Random.self_init (); Random.int 5 + 1))

let update_balance player i =  player.balance <- player.balance + i

(** [move_helper player sum] is [sum] if [sum] is less than 40 and id sum is greater
  than 40, 200 is added to the [player]'s account and [sum] mod 40 is returned*)
let move_helper player sum = 
  if sum >= 40 then (print_string "You passed Go and received $200!"; update_balance player 200; sum mod 40) else sum

let move_player roll player = player.location <- move_helper player (player.location + sums roll)