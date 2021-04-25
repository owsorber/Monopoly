type player_id = string

type balance = int

type location = int

type ownable_name = string

type ownable_name_list = string list

type rolled_dice = int * int

type quarantine_status =
  | In of int
  | Out

exception BalanceBelowZero

exception InQuarantine of int

type t = {
  player_id : player_id;
  mutable balance : balance;
  mutable location : location;
  mutable ownable_name_list : ownable_name_list;
  mutable quarantine_status : quarantine_status;
  mutable get_out_of_quarantine_card: bool;
}

let get_player_id player = player.player_id

let get_balance player = player.balance

let get_location player = player.location

let get_ownable_name_list player = player.ownable_name_list

let make_player id =
  {
    player_id = id;
    balance = 1500;
    location = 0;
    ownable_name_list = [];
    quarantine_status = Out;
    get_out_of_quarantine_card = false
  }

(* [sums roll] is the sum of the two value die in [roll] *)
let sums roll = fst roll + snd roll

let roll () =
  ( (Random.self_init ();
     Random.int 6 + 1),
    (Random.self_init ();
     Random.int 6 + 1) )

let update_balance player i =
  if player.balance + i < 0 then raise BalanceBelowZero
  else player.balance <- player.balance + i

let passes_go roll player = player.location + sums roll >= 40

let move_player roll player =
  match player.quarantine_status with
  | In i -> raise (InQuarantine i)
  | Out ->
      if passes_go roll player then
        player.balance <- player.balance + 200;
      let new_pos = (player.location + sums roll) mod 40 in
      player.location <- new_pos

let projected_space roll player board =
  let new_pos = (player.location + sums roll) mod 40 in
  Board.space_name board new_pos

let go_to_quarantine_status player = player.quarantine_status <- In 3; 
  player.location <- 9

let decrement_day_quarantine player = 
  match player.quarantine_status with
  |Out -> player.quarantine_status <- Out
  |In i -> if i = 1 then player.quarantine_status <- Out 
  else player.quarantine_status <-In (i - 1)

let quarantine player = player.quarantine_status

let buy_ownable p prop i =
  update_balance p (-i);
  p.ownable_name_list <- prop :: p.ownable_name_list

let pay p1 p2 i =
  update_balance p1 (-i);
  update_balance p2 i


let move_player_to p l =
  let o = p.location in 
  if o = l then () else (if o > l then (update_balance p 200; 
  p.location <- l;) else p.location <- l) 


let got_gooq_card player = player.get_out_of_quarantine_card <- true

let used_gooq_card player = player.get_out_of_quarantine_card <- false