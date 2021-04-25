open Yojson.Basic.Util

type card = {
  message: string;
  action : string;
  extra: string
}

exception NotValidCard

(* make deck an array and stack is a shuffled list of deck's values *)
type t =  {
mutable chance_deck: card array;
mutable chance_int: int;
mutable community_chest_deck: card array;
mutable community_chest_int: int;
}

let card_from_json j = {
  message  = j |> member "title" |> to_string; 
  action  = j |> member "action" |> to_string;
  extra = j |> member "extra" |> to_string;
}

(** uses knuth algorithum, change to list*)
let shuffle a = 
  for i = (Array.length a-1) downto 1 do
    let place = Random.int (i+1) in
      let swaper = a.(place) in
        a.(place) <- a.(i);
        a.(i) <- swaper
  done;
  a

let rec init_deck_builder builder cards =
  match cards with
    | [] -> builder
    | h :: t -> init_deck_builder (card_from_json h :: builder) t 

(** returns a shuffles array of cards from a list of cards*)
let init_shuffle l = 
  shuffle (Array.of_list l)

let init_board json = {
  chance_deck = init_shuffle (to_list (json |> member "chance") |> 
    init_deck_builder []);
  chance_int = 0;
  community_chest_deck = init_shuffle (to_list (json |> member 
    "communitychest") |> init_deck_builder []);
  community_chest_int = 0;
  }

let draw_chance_card t = 
  if t.chance_int < (Array.length t.chance_deck - 1) 
    then Array.get t.chance_deck t.chance_int 
  (** change int*)
    else (let c = Array.get t.chance_deck t.chance_int in (t.chance_deck 
    <-shuffle t.chance_deck); t.chance_int <- 0; c;)

let draw_community_chest_card t = 
  if t.community_chest_int < (Array.length t.community_chest_deck - 1) 
    then Array.get t.community_chest_deck t.community_chest_int 
    else (let c = Array.get t.community_chest_deck t.community_chest_int in 
    (t.community_chest_deck <-shuffle t.community_chest_deck); 
    t.community_chest_int <- 0; c;)

let move_card loc p board= 
  Player.move_player_to p (Board.location_from_space_name board loc)

let change_funds p funds = Player.update_balance p (int_of_string funds)

let jail_card p extra =
  match extra with
  |"go to" -> Player.go_to_quarantine_status p
  |"get out" -> Player.got_gooq_card p
  |_ ->  raise  NotValidCard

let move_nearest = failwith "unimplemented"

(* from game- 5th house is hotel *)
let property_charges = failwith "unimplemented"

let change_others_funds = failwith "unimplemented"

let do_card card p board=
  match card.action with
  |"move" -> move_card (card.extra) p board 
  |"addfunds" -> change_funds p card.extra
  |"jail"-> jail_card p card.extra
  |"removefunds" -> change_funds p card.extra
  |"movenum" -> move_card (string_of_int((Player.get_location p) + 
    int_of_string card.extra)) p board
  |"movenearest"-> move_nearest
  |"propertycharges" -> property_charges 
  |"removefundstoplayers" -> change_others_funds 
  |"addfundsfromplayers" -> change_others_funds
  |_ -> raise NotValidCard

