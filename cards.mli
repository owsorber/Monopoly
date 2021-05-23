type card = {
  message : string;
  action : string;
  extra : string;
}

exception NotValidCard

(* Raised when a balance update attempt with int cost is made such that
   we must potentially bankrupt the player. *)
exception MustCheckBankrupt of int

type t = {
  mutable chance_deck : card array;
  mutable chance_int : int;
  mutable community_chest_deck : card array;
  mutable community_chest_int : int;
}

(** [init_cards filename] returns the type t representing the state of
    the cards from the json file describing the chance and community
    chest cards with name [filename]. *)
val init_cards : string -> t

(** [shuffle deck] returns a shuffled array of the cards in [deck] using
    knuth's algorithum*)
val shuffle : card array -> card array

(** [draw_chance_card t] returns the card that at the location
    chance_int in the chance_deck provided by [t] and changes chance_int
    to chance_int + 1. if chance_int is the location of the last card in
    the chance_deck, that card is picked and the deck is then reshuffled
    and chance_int is set to 0 *)
val draw_chance_card : t -> card

(** [draw_community_chest t] returns the card that at the location
    community_chest_int in the community_chest_deck provided by [t] and
    changes community_chest to community_chest + 1. if
    community_chest_int is the location of the last card in the
    community_chest_deck, that card is picked and the deck is then
    reshuffled and community_chest_int is set to 0 *)
val draw_community_chest_card : t -> card

(**[do_card card player board game] performs the action dicated by
   [card] on [player] in [game] with [board], and returns the next
   space's rent and multiplier, if applicable. Raises:
   [MustCheckBankrupt] if doing the card tries to make the [player]'s
   balance go below zero. *)
val do_card : card -> Player.t -> Board.t -> Game.t -> int * int
