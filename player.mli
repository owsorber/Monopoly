(** Representation of a player in Monopoly

    Module contains methods that allow querying of a player state, as
    well as methods that update player state. *)

(** The abstract type representing a player. *)
type t

(** The type [player_id] represents the player_id of a player. *)
type player_id = string

(** The type [balance] represents a player's balance. *)
type balance = int

(** The type [location] represents a location on the Monopoly board. *)
type location = int

(** The type [ownable_name] represents the name of a space on the
    Monopoly board which can be owned. *)
type ownable_name = string

(** The type [ownable_name_list] represents a list of ownable_name. *)
type ownable_name_list = ownable_name list

(** The type [rolled_dice] represents a the result of a roll of a two
    dice, where the roll must be between 1 and 6, inclusive. *)
type rolled_dice = int * int

(** The type [quarantine_status] represents a player's quarantine status
    (whether they are in or out of quarantine). *)
type quarantine_status =
  | In of int
  | Out

(** Raised when a player's Balance is below 0. *)
exception BalanceBelowZero

(** Raised when a player attempts to move and is in quarantine with the
    days remaining in quarantine. *)
exception InQuarantine of int

(** Raised when a player attempts to sell shares of stock that they do
    not have. *)
exception NotEnoughShares

(** [get_player_id t] returns the name of player [t]. *)
val get_player_id : t -> player_id

(** [get_balance t] returns the balance of [t]. *)
val get_balance : t -> balance

(** [get_location t] returns the location of [t]. *)
val get_location : t -> location

(** [get_ownable_name_list t] returns the player_id of t. *)
val get_ownable_name_list : t -> ownable_name_list

(** [make_player id] is a player at the begining of the game with
    player_id [id]. *)
val make_player : player_id -> t

(** [sums d] returns the sum of the rolled [d] values. *)
val sums : rolled_dice -> int

(** [roll] returns a tuple containing 2 random ints between 1 and 6
    inclusive. *)
val roll : unit -> rolled_dice

(** [move_player roll p] updates player [p]'s location based on the dice
    values in [roll] and updates their balance if they pass "Go".
    Requires: [rolled_dice] is a valid roll. Raises: [InQuarantine i]
    where [i] is the remaining days in quarantine. *)
val move_player : rolled_dice -> t -> unit

(** [go_to_quarantine_status p] sends player [p] to quarantine by making
    their location quarantine and changes their quarantine_status to In
    of 3. *)
val go_to_quarantine_status : t -> unit

(** [decrement_day_quarantine p] changes the player [p]'s quarantine
    status to In of (i - 1) where i is the current int in [p]'s
    quarantine_status. Requires: [p].quarantine status is In of int
    where int is > 1. *)
val decrement_day_quarantine : t -> unit

(** [projected_space roll p b] is the name of the space that player [p]
    is projected to land on, given roll [roll] and board [b]. *)
val projected_space : rolled_dice -> t -> Board.t -> string

(** [passed_go roll p] returns true iff the number of spaces player [p]
    will move due to [roll] will cause them to pass "Go". *)
val passes_go : rolled_dice -> t -> bool

(** [update_balance player i] modifies [player]'s balance by adding [i]
    to [player]'s balance. Requires: [i] is a positive number if money
    needs to be added and is negative if money needs to be decuted.
    Raises: [BalanceBelowZero] if [i] will cause [player]'s balance to
    fall below zero. *)
val update_balance : t -> int -> unit

(** [quarantine t] checks if player [t] is in quarantine or not and
    returns player's quarentine_status. *)
val quarantine : t -> quarantine_status

(** [buy_ownable t o i] adds ownable with name [o] to player [t]'s
    property list and deducts cost [i] of property and from player [t]'s
    balance. Requires: [o] is unowned, and player [t]'s balance is
    greater than or equal to the cost of the property. *)
val buy_ownable : t -> ownable_name -> int -> unit

(** [swap_properties p1 p2 own_lst] removes all properties in [own_lst]
    from player [p1]'s ownable list and adds the properties to player
    [p2]'s ownable list. Requires: all properties in [own_lst] are owned
    by p1. *)
val swap_properties : t -> t -> ownable_name list -> unit

(** [pay p1 p2 amount] deducts [amount] from [p1].balance and adds
    [amount] to [p2].balance. *)
val pay : t -> t -> int -> unit

(** [move_player_to p l can_pass] updates player [p]'s location to [l].
    Updates their balance if they pass "Go" and [can_pass] is true. *)
val move_player_to : t -> location -> bool -> unit

(** [leave_quarantine player] updates [player]'s quarantine status to
    Out. *)
val leave_quarantine : t -> unit

(** [get_stocks p] returns a list of the stocks owned by player [p] in
    the form of an association list with the stock name as the first
    item in each pair and the number of shares owned as the second item
    in each pair. *)
val get_stocks : t -> (Stockmarket.stock_name * int) array

(** [buy_stocks p s n c] completes all parts of a transaction (adding
    the stock to the player's portfolio, decreasing the player's
    balance) where player [p] buys [n] shares of stock with name [s] at
    a total cost [c]. Raises: [BalanceBelowZero] if the purchase is
    invalid because cost [c] exceeds the player's current balance. *)
val buy_stocks : t -> Stockmarket.stock_name -> int -> int -> unit

(** [sell_stocks p s n c] completes all parts of a transaction (removing
    the shares from the player's portfolio, increasing the player's
    balance) where player [p] sells [n] shares of stock with name [s] at
    a total cost [c]. Raises: [NotEnoughShares] if the player owns less
    than [n] shares of stock with name [s]. *)
val sell_stocks : t -> Stockmarket.stock_name -> int -> int -> unit
