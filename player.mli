(** The type representing a player*)
type t

(** the type of player_id*)
type player_id = string

(** the type of balance*)
type balance = int

(** the type of location*)
type location = int

(** the type of property_name_list*)
type property_name_list = string list

type rolled_dice = int*int 

(** [get_player_id t] returns the player_id of [t]*)
val get_player_id : t -> player_id 

(** [get_balance t] returns the balance of [t]*)
val get_balance : t -> balance

(** [get_location t] returns the location of [t]*)
val get_location : t -> location

(** [get_ t] returns the player_id of t*)
val get_property_name_list : t -> property_name_list

(** [make_player id] is a player at the begining of the game with 
    player_id [id]*)
val make_player : player_id -> t

(**[roll] returns a tuple containing 2 random ints between 1 and 6 inclusive*)
val roll : rolled_dice

(** [move_player loc roll] updates a players location bases on their 
    current location [loc ]and the value of their dice roll [roll]*)
val move_player : t -> (int*int) -> unit

(** [update_balance player i] modifies [player]'s balance by adding [i] to 
    [player]'s balance.
    requries: [i] is a positive number if money needs to be added
        and is negative if money needs to be decuted *)
val update_balance : t -> int -> unit


