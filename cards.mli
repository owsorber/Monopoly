(** Representation of a deck of community chest and chance cards

    This module represents the deck of community chest and chance cards,
    including accessors to the top card of the deck and methods to
    shuffle each deck. Methods to complete actions stated by each card
    are in this module, as well. *)

(** The type [card] represents a card in the community chest or chance
    decks. Each card contains a message, action, and any additional
    information needed to perform the action. *)
type card = {
  message : string;
  action : string;
  extra : string;
}

(** Raised when do_card is called on an argument that does not have
    valid card properties set in our abstract type card *)
exception NotValidCard

(** Raised when a balance update attempt is made such that we must
    potentially bankrupt the player attempting to pay an integer amount. *)
exception MustCheckBankrupt of int

(** The abstract type of values representing a deck of community chest
    and chance cards. *)
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
    Knuth's algorithum. *)
val shuffle : card array -> card array

(** [draw_chance_card t] returns the card that at the location
    chance_int in the chance_deck provided by [t] and changes chance_int
    to chance_int + 1. If chance_int is the location of the last card in
    the chance_deck, that card is picked and the deck is then reshuffled
    and chance_int is set to 0. *)
val draw_chance_card : t -> card

(** [draw_community_chest t] returns the card that at the location
    community_chest_int in the community_chest_deck provided by [t] and
    changes community_chest to community_chest + 1. If
    community_chest_int is the location of the last card in the
    community_chest_deck, that card is picked and the deck is then
    reshuffled and community_chest_int is set to 0. *)
val draw_community_chest_card : t -> card

(**[do_card card player board game] performs the action dicated by
   [card] on [player] in [game] with [board], and returns the next
   space's rent and multiplier, if applicable. Raises:
   [MustCheckBankrupt] if doing the card tries to make the [player]'s
   balance go below zero. *)
val do_card : card -> Player.t -> Board.t -> Game.t -> int * int
