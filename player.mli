(** The abstract type representing a player *)
type t

(** the type of player_id *)
type player_id = string

(** the type of balance *)
type balance = int

(** the type of location *)
type location = int

(** the type of property_name_list *)
type property_name_list = string list

(** the type of rolled_dice where each int is between 1 and 6 inclusive*)
type rolled_dice = int * int

(** type that give the player's quarantine status
    if the player is in quarentine it, gives how many days are left*)
type quarantine_status =
|In of int
|Out

(** Raised when a player's Balance is below 0*)
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
    player_id [id] *)
val make_player : player_id -> t

(** [roll] returns a tuple containing 2 random ints between 1 and 6
    inclusive *)
val roll : unit -> rolled_dice

(** [move_player roll p] if the player's quarantine_status is out 
    updates player [p]'s location based on the dice values in [roll] and updates
    their balance if they pass "Go". 
    If the player's quarantine_status is In of Int
    player rolls
    if roll is doubles (two die of the same value) the player 
    quarentine status is updated to out and move player is called again.
    if player does not roll doubles and the int is > 1, the int is decreased by 
    1 and the turn ends
    if the player does not roll doubles and the int is = 1, they must pay to get 
    out of jail their quarentine staus is changed, and move player is called 
    again.
    Requires: [rolled_dice] is a valid roll. *)
val move_player : rolled_dice -> t -> unit

(** [projected_space roll p b] is the name of the space that player [p] is 
    projected to land on, given roll [roll] and board [b]. *)
val projected_space : rolled_dice -> t -> Board.t -> string

(** [passed_go roll p] returns true if the number of spaces player [p]
    will move due to [roll] will cause them to pass "Go". *)
val passes_go : rolled_dice -> t -> bool

(** [update_balance player i] modifies [player]'s balance by adding [i]
    to [player]'s balance. Requires: [i] is a positive number if money
    needs to be added and is negative if money needs to be decuted.
    Raises: [BalanceBelowZero] if [i] will cause [player]'s balance to
    fall below zero. *)
val update_balance : t -> int -> unit

(** [quarantine t] Checks if player [t] is in quarantine or not and returns
    player's quarentine_status*)
val quarantine : t -> quarantine_status

(**[add_house t prop] updates houses in game for property [prop] owned by 
    player [t] and deducts cost of house from player [t]'s balance
    requires: player [t] owns prop,
        player [t]'s balance is greater than or equal to the cost of a house
        player [t] owns all other properties in the property group*)
val add_house : t -> Game.property -> unit

(**[add_house t prop] adds property [prop] to player [t]'s property list, 
    deducts cost of house and from player [t]'s balance and updates game 
    property hashmap
    requires: [prop] is unowned
        player [t]'s balance is greater than or equal to the cost of the 
        property*)
val buy_property : t -> Game.ownable -> unit

(** [mortgage_property t ownable] checks if space is owned by player [t], if so
    then changes property [ownable]'s mortaged value to mortaged, increases 
    player [t]'s balance by the property's [ownable] mortage value and updates 
    the game hashmap to show that property is morgaged.
    requires: property [ownable] can be morgaged
        player [t] owns [ownable]*)
val mortgage_property : t -> Game.ownable -> unit
