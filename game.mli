(** The abstract type representing a Game State *)
type t

type ownable_name = string

type property_status =
  | P_Owned of (Player.t * int)
  | P_Mortgaged of Player.t
  | P_Available

type rr_status =
  | RR_Owned of Player.t
  | RR_Mortgaged of Player.t
  | RR_Available

type util_status =
  | U_Owned of Player.t
  | U_Mortgaged of Player.t
  | U_Available

type ownable_status =
  | Property of property_status
  | Railroad of rr_status
  | Utility of util_status

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

(** Raised when a hotel cannot be added to a property *)
exception CannotAddHotel of string

(** Raised when a house cannot be sold on a property *)
exception CannotSellHouse of string

(** Raised when a hotel cannot be sold on a property *)
exception CannotSellHotel of string

(** [get_board g] returns the board field in [game] g *)
val get_board : t -> Board.t

(** [get_player g] returns the [player] associated with the current
    player in [game] g *)
val current_player : t -> Player.t

(** [get_all_players g] returns an array of all players in [game] g *)
val get_all_players : t -> Player.t array

(** [init_game b p] returns a [game] with [board] b and with [players]
    given by the [player] array p *)
val init_game : Board.t -> Player.t array -> t

(** [next_player g] mutates a game [g] with a new current player whose
    turn it is. *)
val next_player : t -> unit

(** [get_properties g p] returns the list of the names of the properties
    owned by player [p] in game [g]. *)
val get_properties : t -> Player.t -> ownable_name list

(** [get_free_parking g] returns the accumulated free parking amount in
    the game [g]. *)
val get_free_parking : t -> int

(** [get_houses_available g] returns the number of houses available in
    the game [g]. *)
val get_houses_available : t -> int

(** [get_hotels_available g] returns the number of hotels available in
    the game [g]. *)
val get_hotels_available : t -> int

(** [do_free_parking game player] increases the player's balance by the
    total amount of money accumulated at the free parking spot, and
    resets the free parking spot to $0, and returns the amount paid to
    [player] *)
val do_free_parking : t -> Player.t -> int

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

(** [can_trade g p ownable_name] returns true iff player [p] can put up
    the ownable with name [ownable_name] for trade in game [g].
    Requires: player [p] owns [ownable_name] *)
val can_trade : t -> Player.t -> ownable_name -> bool

(** [all_can_trade g p] returns an array of the ownables that [p] can
    trade.*)
val all_can_trade : t -> Player.t -> ownable_name array

(** [can_add_house g p property_name] returns true iff player [p] can
    add a house on the property with name [property_name] in game [g].
    Requires: Player [p] owns property with name [property_name].
    Raises: [NotPropertyName] if [property_name] is not a property name.
    [CannotAddHouse s] if a house cannot be added to property [p] with
    the appropriate exception message [s].*)
val can_add_house : t -> Player.t -> ownable_name -> bool

(** [all_can_buy_house g p] returns an array of the ownables that [p]
    can put a house on. *)
val all_can_buy_house : t -> Player.t -> ownable_name array

(** [all_can_buy_hotel g p] returns an array of the ownables that [p]
    can put a hotel on. *)
val all_can_buy_hotel : t -> Player.t -> ownable_name array

(** [house_price g p property_name] returns the price of a house that
    could be bought on property with name [property_name]. *)
val house_price : t -> Player.t -> ownable_name -> int

(** [add_house g p adding_house] adds a house to the property with name
    [p] in game [g]. Decrements houses available if [adding_house],
    decrements hotels available if ![adding_house]. Requires: [p] is a
    property name and a house can be added to it. *)
val add_house : t -> ownable_name -> bool -> unit

(** [can_add_hotel g p property_name] returns true iff player [p] can
    add a hotel on the property with name [property_name] in game [g].
    Requires: Player [p] owns property with name [property_name].
    Raises: [NotPropertyName] if [property_name] is not a property name.
    [CannotAddHotel s] if a hotel cannot be added to property [p] *)
val can_add_hotel : t -> Player.t -> ownable_name -> bool

(** [get_net_worth g p] returns player [p]'s balance + (value of owned
    assets if they were all sold) *)
val get_net_worth : t -> Player.t -> int

(** [sell_house g o selling_house] sells a house on the property with
    name [o] in game [g]. Increments houses available if
    [selling_house], increments hotels available if ![selling_house]. *)
val sell_house : t -> ownable_name -> bool -> unit

(** [sell_all g p] sells all houses and hotels owned by player [p] in
    game [g] and mortgages all owned spaces of player [p], updating
    balance accordingly. *)
val sell_all : t -> Player.t -> unit

(** [can_sell_house g p property_name] returns true iff player [p] can
    sell a house on the property with name [property_name] in game [g].
    Requires: Player [p] owns property with name [property_name].
    Raises: [NotPropertyName] if [property_name] is not a property name.
    [CannotSellHouse s] if a house cannot be sold on property [p] *)
val can_sell_house : t -> Player.t -> ownable_name -> bool

(** [can_sell_hotel g p property_name] returns true iff player [p] can
    sell a hotel on the property with name [property_name] in game [g].
    Requires: Player [p] owns property with name [property_name].
    Raises: [NotPropertyName] if [property_name] is not a property name.
    [CannotSellHotel s] if a hotel cannot be sold on property [p] *)
val can_sell_hotel : t -> Player.t -> ownable_name -> bool

(** [all_can_sell_house p] returns an array of the ownables that [p] can
    sell a house on. *)
val all_can_sell_house : t -> Player.t -> ownable_name array

(** [all_can_sell_hotel p] returns an array of the ownables that [p] can
    sell a hotel on. *)
val all_can_sell_hotel : t -> Player.t -> ownable_name array

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

(** [delete_player g p] removes player [p] from game [g]. *)
val delete_player : t -> Player.t -> unit
