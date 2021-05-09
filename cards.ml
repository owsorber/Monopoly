open Yojson.Basic.Util

type card = {
  message : string;
  action : string;
  extra : string;
}

exception NotValidCard

(* make deck an array and stack is a shuffled list of deck's values *)
type t = {
  mutable chance_deck : card array;
  mutable chance_int : int;
  mutable community_chest_deck : card array;
  mutable community_chest_int : int;
}

let card_from_json j =
  {
    message = j |> member "title" |> to_string;
    action = j |> member "action" |> to_string;
    extra = j |> member "extra" |> to_string;
  }

(** uses knuth algorithum*)
let shuffle a =
  for i = Array.length a - 1 downto 1 do
    let place = Random.int (i + 1) in
    let swaper = a.(place) in
    a.(place) <- a.(i);
    a.(i) <- swaper
  done;
  a

let rec init_deck_builder builder cards =
  match cards with
  | [] -> builder
  | h :: t -> init_deck_builder (card_from_json h :: builder) t

(** returns a shuffles array of cards*)
let init_shuffle l = shuffle (Array.of_list l)

let init_cards filename =
  let json = Yojson.Basic.from_file filename in
  {
    chance_deck =
      init_shuffle
        (to_list (json |> member "chance") |> init_deck_builder []);
    chance_int = 0;
    community_chest_deck =
      init_shuffle
        ( to_list (json |> member "communitychest")
        |> init_deck_builder [] );
    community_chest_int = 0;
  }

let draw_chance_card t =
  if t.chance_int < Array.length t.chance_deck - 1 then (
    let c = t.chance_deck.(t.chance_int) in
    t.chance_int <- t.chance_int + 1;
    c )
  else
    let c = t.chance_deck.(t.chance_int) in
    t.chance_deck <- shuffle t.chance_deck;
    t.chance_int <- 0;
    c

let draw_community_chest_card t =
  if t.community_chest_int < Array.length t.community_chest_deck - 1
  then (
    let c = t.community_chest_deck.(t.community_chest_int) in
    t.community_chest_int <- t.community_chest_int + 1;
    c )
  else
    let c = t.community_chest_deck.(t.community_chest_int) in
    t.community_chest_deck <- shuffle t.community_chest_deck;
    t.community_chest_int <- 0;
    c

let move_card loc p can_pass (loc_type, rent_mult) =
  Player.move_player_to p loc can_pass;
  let dummyRent =
    if loc_type = "utility" then (
      let roll = Player.roll () in
      Printers.magenta_print
        ( "First dice: "
        ^ string_of_int (fst roll)
        ^ ". Second dice: "
        ^ string_of_int (snd roll)
        ^ "\n" );
      Player.sums roll )
    else 0
  in
  (dummyRent, rent_mult)

let change_funds p g funds =
  ( try Player.update_balance p (int_of_string funds)
    with Player.BalanceBelowZero -> Game.delete_player g p );
  (0, 1)

let quarantine_card p extra =
  let _ =
    match extra with
    | "go to" -> Player.go_to_quarantine_status p
    | "get out" -> Player.got_gooq_card p
    | _ -> raise NotValidCard
  in
  (0, 1)

let rec find_nearest board loc typ =
  match Board.space_from_location board loc with
  | Railroad r ->
      if typ = "rr" then loc else find_nearest board (loc + 1) typ
  | Utility u ->
      if typ = "utility" then loc else find_nearest board (loc + 1) typ
  | _ -> find_nearest board (loc + 1) typ

let move_nearest player board extra game =
  let cmd_list = String.split_on_char ' ' extra in
  let loc_type = List.hd cmd_list in
  let rent_mult = int_of_string (List.nth cmd_list 1) in
  move_card
    (find_nearest board (Player.get_location player) loc_type)
    player true (loc_type, rent_mult)

let rec num_houses game list house hotel =
  match list with
  | [] -> (house, hotel)
  | h :: t ->
      let num = Game.get_houses game h in
      if num = 5 then num_houses game t house (hotel + 1)
      else num_houses game t (house + num) hotel

let rec string_to_int_list str =
  match str with
  | [] -> []
  | h :: t -> int_of_string h :: string_to_int_list t

let property_charges game player extra =
  let num = num_houses game (Game.get_properties game player) 0 0 in
  let list = string_to_int_list (String.split_on_char ' ' extra) in
  ( try
      Player.update_balance player
        (-((fst num * List.hd list) + (snd num * List.nth list 1)))
    with Player.BalanceBelowZero -> Game.delete_player game player );
  (0, 1)

let add_others_funds player game amount =
  Array.iter
    (fun p ->
      try Player.pay p player amount
      with Player.BalanceBelowZero -> Game.delete_player game p)
    (Game.get_all_players game);
  (0, 1)

let receive_others_funds player game amount =
  Array.iter
    (fun p ->
      try Player.pay player p amount
      with Player.BalanceBelowZero -> Game.delete_player game player)
    (Game.get_all_players game);
  (0, 1)

let do_card card p board game =
  match card.action with
  | "move" ->
      move_card
        (Board.location_from_space_name board card.extra)
        p true ("", 1)
  | "addfunds" -> change_funds p game card.extra
  | "quarantine" -> quarantine_card p card.extra
  | "removefunds" -> change_funds p game card.extra
  | "movenum" ->
      move_card
        (Player.get_location p + int_of_string card.extra)
        p false ("", 1)
  | "movenearest" -> move_nearest p board card.extra game
  | "propertycharges" -> property_charges game p card.extra
  | "removefundstoplayers" ->
      receive_others_funds p game (int_of_string card.extra)
  | "addfundsfromplayers" ->
      add_others_funds p game (int_of_string card.extra)
  | _ -> raise NotValidCard
