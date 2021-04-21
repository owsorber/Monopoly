
type card = {
    card_number: int;
    message: string;
  }
  
  type t =  {
  deck: card list;
  mutable stack: card list 
  }
  
(** provides a shuffles list of ints that represent cards*)
val shuffle : unit -> int list

(** picks the card that is on the top of the int and pops that card off the top
    if it is the last card then it reshufffles the deck*)
val draw_card: int list -> t

(**does the action the card says and then returns the cards message*)
val do_card: t -> string