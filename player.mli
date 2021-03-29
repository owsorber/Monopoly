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

exception BalanceBelowZero

(** [get_player_id t] returns the name of player [t]*)
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
val roll : unit -> rolled_dice

(** [move_player roll p] updates player [p]'s location based on the dice 
    values in [roll] and updates their balance if they pass "Go". *)
val move_player : rolled_dice -> t -> unit

(** [passed_go roll p] returns true if the number of spaces player [p] will move
    due to [roll] will cause them to pass "Go" *)
val passes_go : rolled_dice -> t -> bool

(** [update_balance player i] modifies [player]'s balance by adding [i] to 
    [player]'s balance.
    Requires: [i] is a positive number if money needs to be added and is 
    negative if money needs to be decuted.
    Raises: [BalanceBelowZero] if [i] will cause [player]'s balance to fall
    below zero. *)
val update_balance : t -> int -> unit