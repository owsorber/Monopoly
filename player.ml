type player_id = string

type balance = int

type location = int

type ownable_name = string

type ownable_name_list = string list

type rolled_dice = int * int

type quarantine_status =
  | In of int
  | Out

type stock_portfolio =
  (Stockmarket.stock_name, Stockmarket.stock_value) Hashtbl.t

exception BalanceBelowZero

exception InQuarantine of int

exception NotEnoughShares

type t = {
  player_id : player_id;
  mutable balance : balance;
  mutable location : location;
  mutable ownable_name_list : ownable_name_list;
  mutable quarantine_status : quarantine_status;
  mutable owned_stocks : stock_portfolio;
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
    owned_stocks = Hashtbl.create 5;
  }

(* [sums roll] is the sum of the two value die in [roll] *)
let sums roll = fst roll + snd roll

let roll () =
  ( (Random.self_init ();
     Random.int 6 + 1),
    (Random.self_init ();
     Random.int 6 + 1) )

let update_balance player amount =
  if player.balance + amount < 0 then raise BalanceBelowZero
  else player.balance <- player.balance + amount

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

let go_to_quarantine_status player =
  player.quarantine_status <- In 3;
  player.location <- 10

let decrement_day_quarantine player =
  match player.quarantine_status with
  | Out -> player.quarantine_status <- Out
  | In i ->
      if i = 1 then player.quarantine_status <- Out
      else player.quarantine_status <- In (i - 1)

let leave_quarantine player = player.quarantine_status <- Out

let quarantine player = player.quarantine_status

let buy_ownable p property amount =
  update_balance p (-amount);
  p.ownable_name_list <- property :: p.ownable_name_list

(* returns true if [elt] is contained in [lst] *)
let rec contains elt = function
  | [] -> false
  | h :: t -> if elt = h then true else contains elt t

(* [remove_properties p lst] removes all properties in [own_lst] from
   player [p]'s property list*)
let remove_properties p own_lst =
  let rec remove_helper acc player_lst =
    match player_lst with
    | [] -> acc
    | h :: t ->
        if contains h own_lst then remove_helper acc t
        else remove_helper (h :: acc) t
  in
  let new_lst = remove_helper [] p.ownable_name_list in
  p.ownable_name_list <- new_lst

(* [add_properties p lst] adds all properties in [own_lst] to player
   [p]'s property list *)
let rec add_properties p own_lst =
  match own_lst with
  | [] -> ()
  | h :: t ->
      p.ownable_name_list <- h :: p.ownable_name_list;
      add_properties p t

let swap_properties p1 p2 own_lst =
  remove_properties p1 own_lst;
  add_properties p2 own_lst

let pay p1 p2 amount =
  update_balance p2 amount;
  update_balance p1 (-amount)

let move_player_to p location can_pass =
  let o = p.location in
  if o = location then ()
  else if o > location && can_pass then (
    update_balance p 200;
    p.location <- location)
  else p.location <- location

let get_stocks p =
  Hashtbl.fold
    (fun k v acc -> Array.append acc [| (k, v) |])
    p.owned_stocks [||]

let num_shares_owned p stock =
  try Hashtbl.find p.owned_stocks stock with Not_found -> 0

let buy_stocks p s n c =
  update_balance p (-c);
  let num_shares = num_shares_owned p s in
  Hashtbl.replace p.owned_stocks s (num_shares + n)

let sell_stocks p s n c =
  let num_shares = num_shares_owned p s in
  if num_shares < n then raise NotEnoughShares else update_balance p c;
  Hashtbl.replace p.owned_stocks s (num_shares - n)
