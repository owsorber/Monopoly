(** The abstract type representing a Game State *)
type t

type ownable_name = string

type ownable_status

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

(** [get_free_parking g] returns the accumulated free parking amount in
    the game [g]. *)
val get_free_parking : t -> int

(** [get_rent g i r] returns the rent associated with landing on the
    space with location after rolling [r] in game [g]. TODO: Raise
    Exception. *)
val get_rent : t -> int -> Player.rolled_dice -> int

(** [get_ownable s] returns Some ownable if [s] is a valid ownable
    property and None otherwise. *)
val get_ownable_status : t -> Board.space -> ownable_status option

(** [get_ownable_price b o] returns the price to buy ownable property
    [o] if it is available in board [b]. *)
val get_ownable_price : Board.t -> ownable_name -> int

(** [make_ownable_owned g p o] makes player [p] own the ownable [o] in
    game [g] i.e. changes the ownable's status to Owned and performs no
    checks. *)
val make_ownable_owned : t -> Player.t -> ownable_name -> unit

(** [make_ownable_mortgaged g p o] makes player [p] mortgage the ownable
    [o] in the game [g]. Raises: [MortgageFailure] if the ownable cannot
    be mortgaged either because it is a utility or because the player
    does not own it. *)
val make_ownable_mortgaged : t -> Player.t -> ownable_name -> unit

(** [all_mortgagable p] returns an array of the ownables that [p] can
    mortgage i.e. all ownables that are owned (and if a property, has
    zero houses on the color group). *)
val all_mortgagable : t -> Player.t -> ownable_name array

(** [can_add_house g p property_name] returns true iff player [p] can
    add a house on the property with name [property_name] in game [g].
    Requires: Player [p] owns property with name [property_name].
    Raises: [NotPropertyName] if [property_name] is not a property name. *)
val can_add_house : t -> Player.t -> ownable_name -> bool

(** [add_house g p] adds a house to the property with name [p] in game
    [g]. Requires: [p] is a property name and a house can be added to
    it. Raises: [NotPropertyName] if [p] does not correspond to a
    property. *)
val add_house : t -> ownable_name -> unit

(** [can_add_hotel g p property_name] returns true iff player [p] can
    add a hotel on the property with name [property_name] in game [g].
    Requires: Player [p] owns property with name [property_name].
    Raises: [NotPropertyName] if [property_name] is not a property name. *)
val can_add_hotel : t -> Player.t -> ownable_name -> bool

(** [add_hotel g p] adds a hotel to the property with name [p] in game
    [g]. Requires: [p] is a property name and a house can be added to
    it. Raises: [NotPropertyName] if [p] does not correspond to a
    property. *)
val add_hotel : t -> ownable_name -> unit

(** [is_available g o] returns true iff the ownable with name [o] is
    available in game [g]. Raises: [NotOwnableName] if [o] does not
    correspond to an ownable. *)
val is_available : t -> ownable_name -> bool

(** [owner g o] returns Some Player.t if the ownable with name [o] is
    owned by a player and None if it available in game [g]. Raises:
    [NotOwnableName] if [o] does not correspond to an ownable. *)
val owner : t -> ownable_name -> Player.t option

(* [has_monopoly g p col] returns true iff player [p] has a monopoly on
   the color group [col] in game [g] i.e. [p] owns every property with
   color [col]. *)
val has_monopoly : t -> Player.t -> string -> bool

(* [has_houses_on_color g p col] returns true iff player [p] has any
   houses on any properties with color [col] in game [g]. *)
val has_houses_on_color : t -> Player.t -> string -> bool
