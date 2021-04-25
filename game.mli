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

(** Raised when a house cannot be added to a property *)
exception CannotAddHouse of string

(** Raised when a hotel cannot be added to a hotel *)
exception CannotAddHotel of string

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

(** [do_free_parking game player] increases the player's balance by the
    total amount of money accumulated at the free parking spot, and
    resets the free parking spot to $0. *)
val do_free_parking : t -> Player.t -> unit

(** [do_tax game player space] decreases the player's balance by the tax
    amount of [space] and increases the free parking by the same amount. *)
val do_tax : t -> Player.t -> Board.space -> unit

(** [get_rent g i r] returns the rent associated with landing on the
    space with location [i] after rolling [r] in game [g]. TODO: Raise
    Exception. *)
val get_rent : t -> int -> Player.rolled_dice -> int

(** [get_ownable s] returns Some ownable if [s] is a valid ownable
    property and None otherwise. *)
val get_ownable_status : t -> Board.space -> ownable_status option

(** [get_ownable_price b o] returns the price to buy ownable property
    [o] if it is available in board [b]. *)
val get_ownable_price : Board.t -> ownable_name -> int

(** [get_ownable_info g b o] returns a string containing relevant
    information about the space with name [ownable_name]. Requires: *)
val get_ownable_info : t -> Board.t -> ownable_name -> string

(** [get_houses g name] returns the number of houses on the space with
    [ownable_name] name*)
val get_houses : t -> ownable_name -> int

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
    Raises: [NotPropertyName] if [property_name] is not a property name.
    [CannotAddHouse s] if a house cannot be added to property [p] with
    the appropriate exception message [s].*)
val can_add_house : t -> Player.t -> ownable_name -> bool

(** [next_house_price g p property_name] returns the price of the next
    house that would be bought on property with name [property_name].
    Requires: Player [p] can buy a house on the property. *)
val next_house_price : t -> Player.t -> ownable_name -> int

(** [all_can_buy_house p] returns an array of the ownables that [p] can
    put a house on. *)
val all_can_buy_house : t -> Player.t -> ownable_name array

(** [all_can_buy_hotel p] returns an array of the ownables that [p] can
    put a hotel on. *)
val all_can_buy_hotel : t -> Player.t -> ownable_name array

(** [hotel_price g p property_name] returns the price of the hotel that
    would be bought on property with name [property_name]. Requires:
    Player [p] can buy a hotel on the property. *)
val hotel_price : t -> Player.t -> ownable_name -> int

(** [add_house g p adding_house] adds a house to the property with name
    [p] in game [g]. Decrements houses available if [adding_house],
    decrements hotels available if ![adding_house]. Requires: [p] is a
    property name and a house can be added to it. Raises:
    [NotPropertyName] if [p] does not correspond to a property. *)
val add_house : t -> ownable_name -> bool -> unit

(** [can_add_hotel g p property_name] returns true iff player [p] can
    add a hotel on the property with name [property_name] in game [g].
    Requires: Player [p] owns property with name [property_name].
    Raises: [NotPropertyName] if [property_name] is not a property name.
    [CannotAddHotel s] if [adding_house] is false and a hotel cannot be
    added to property [p]*)
val can_add_hotel : t -> Player.t -> ownable_name -> bool

(* (** [add_hotel g p] adds a hotel to the property with name [p] in
   game [g]. Requires: [p] is a property name and a house can be added
   to it. Raises: [NotPropertyName] if [p] does not correspond to a
   property. *) val add_hotel : t -> ownable_name -> unit *)

(** [is_available g o] returns true iff the ownable with name [o] is
    available in game [g]. Raises: [NotOwnableName] if [o] does not
    correspond to an ownable. *)
val is_available : t -> ownable_name -> bool

(** [is_available g o] returns true iff the ownable with name [o] is
    mortgaged in game [g]. Raises: [NotOwnableName] if [o] does not
    correspond to an ownable. *)
val is_mortgaged : t -> ownable_name -> bool

(** [owner g o] returns Some Player.t if the ownable with name [o] is
    owned by a player and None if it available in game [g]. Raises:
    [NotOwnableName] if [o] does not correspond to an ownable. *)
val owner : t -> ownable_name -> Player.t option

(** [has_monopoly g p col] returns true iff player [p] has a monopoly on
    the color group [col] in game [g] i.e. [p] owns every property with
    color [col]. *)
val has_monopoly : t -> Player.t -> string -> bool

(** [has_houses_on_color g p col] returns true iff player [p] has any
    houses on any properties with color [col] in game [g]. *)
val has_houses_on_color : t -> Player.t -> string -> bool

(** [landing_on_space g p b r s] returns a string of relevant
    information, after enacting the necessary actions for player [p]
    landing on the space with name [s], after rolling [r]. *)
val landing_on_space :
  t -> Player.t -> Board.t -> Player.rolled_dice -> string -> string
