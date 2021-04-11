(** The abstract type representing a Game State *)
type t

type ownable

type ownable_name

(** Raised when a space represented by type [ownable] is not ownable*)
exception NotOwnable

(** [get_board g] returns the board field in [game] g *)
val get_board : t -> Board.t

(** [get_player g] returns the [player] associated with the current
    player in [game] g *)
val current_player : t -> Player.t

(** [update_player p g] mutates the [player] array in [game] g to update
    values following the current player's move *)
val update_player : Player.t -> t -> unit

(** [get_all_players g] returns a list of all players in [game] g *)
val get_all_players : t -> Player.t array

(** [init_game b p] returns a [game] with [board] b and with [players]
    given by the [player] array p *)
val init_game : Board.t -> Player.t array -> t

(** [next_player g] mutates a [game] g with an updated current player *)
val next_player : t -> unit

(** [get_rent o r] returns the rent associated with landing on a space
    with type ownable [o] with the roll [r]. *)
val get_rent : ownable -> Player.rolled_dice -> int

(** [add_house p] adds a house to the property with name [p]. Requires:
    [p] is a Property. Raises: [NotProperty] if [p] is not a property. *)
val add_house : ownable_name -> unit

(** [is_available o] returns whether or not the space with type
    [ownable] o is available. Requires: ownable [o] is an ownable
    property. *)
val is_available : ownable -> bool

(** [owner o] returns Some Player.t if the space with type [ownable] o
    is owned by a player and None if it available. Requires: ownable [o]
    is an ownable property. *)
val owner : ownable -> Player.t option

(** [get_free_parking t] returns the accumulated free parking amount in
    [game] t *)
val get_free_parking : t -> int
