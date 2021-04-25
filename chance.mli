
type card = {
    message: string;
    action : string
  }
  
exception NotValidCard

type t =  {
    chance_deck: card array;
    mutable chance_int: int;
    community_chest_deck: card array;
    mutable community_chest_int: int;
  }
  
(** provides a shuffles list of ints that represent cards*)
val shuffle : unit -> int list

(** picks the card that is on the top of the int and pops that card off the top
    if it is the last card then it reshufffles the deck*)
val draw_chance_card: int list -> t

(** picks the card that is on the top of the int and pops that card off the top
    if it is the last card then it reshufffles the deck*)
val draw_community_chest_card: int list -> t


(**does the action the card says and then returns the cards message*)
val do_card: card -> Player.t -> string