(** The abstract type representing a Game State *)
type t

type ownable

type ownable_name

(** Raised when a space cannot be owned. *)
exception NotOwnableSpace

(** Raised when a string does not correspond to an ownable name. *)
exception NotOwnableName

(** Raised when an ownable cannot be mortgaged. *)
exception MortgageFailure

(** Raised when a string does not correspond to a property name. *)
exception NotPropertyName

(** [get_board g] returns the board field in [game] g *)
val get_board : t -> Board.t

(** [get_player g] returns the [player] associated with the current
    player in [game] g *)
val current_player : t -> Player.t

(** [get_all_players g] returns a list of all players in [game] g *)
val get_all_players : t -> Player.t array

(** [init_game b p] returns a [game] with [board] b and with [players]
    given by the [player] array p *)
val init_game : Board.t -> Player.t array -> t

(** [next_player g] mutates a game [g] with a new current player whose
    turn it is. *)
val next_player : t -> unit

(** [get_rent o r] returns the rent associated with landing on the
    ownable with name [o] after rolling [r]. Raises: [NotOwnableName] if
    [o] does not correspond to an ownable. *)
val get_rent : ownable_name -> Player.rolled_dice -> int

(* [make_ownable_owned p o] makes player [p] own the ownable [o]. *)
val make_ownable_owned : Player.t -> ownable_name -> unit

(* [make_ownable_mortgaged p o] makes player [p] mortgage the ownable
   [o]. Raises: [MortgageFailure] if the ownable cannot be mortgaged
   either because it is a utility or because the player does not own it. *)
val make_ownable_mortgaged : Player.t -> ownable_name -> unit

(* [all_mortgagable p] returns an array of the ownables that [p] can
   mortgage i.e. all ownables that are owned (and if a property, has
   zero houses on the color group). *)
val all_mortgagable : Player.t -> ownable_name array

(* [can_add_house p property_name] returns true iff player [p] can add a
   house on the property with name [property_name]. Raises:
   [NotPropertyName] if [property_name] is not a property name. *)
val can_add_house : Player.t -> ownable_name -> bool

(** [add_house p] adds a house to the property with name [p]. Requires:
    [p] is a property name and a house can be added to it. Raises:
    [NotPropertyName] if [p] does not correspond to a property. *)
val add_house : ownable_name -> unit

(** [is_available o] returns true iff the ownable with name [o] is
    available. Raises: [NotOwnableName] if [o] does not correspond to an
    ownable. *)
val is_available : ownable_name -> bool

(** [owner o] returns Some Player.t if the ownable with name [o] is
    owned by a player and None if it available. Raises: [NotOwnableName]
    if [o] does not correspond to an ownable. *)
val owner : ownable_name -> Player.t option

(** [get_free_parking g] returns the accumulated free parking amount in
    the game [g]. *)
val get_free_parking : t -> int

(* [has_monopoly p col] returns true iff player [p] has a monopoly on
   the color group [col] i.e. [p] owns every property with color [col]. *)
val has_monopoly : Player.t -> string -> bool

(* [has_houses_on_color] returns true iff player [p] has any houses on
   any properties with color [col]. *)
val has_houses_on_color : Player.t -> string -> bool
