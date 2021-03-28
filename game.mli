(** The abstract type representing a Game State*)
type t

(** [get_board g] returns the board field in [game] g *)
val get_board: t -> Board.t

(** [get_player g] returns the [player] associated with
    the current player in [game] g *)
val current_player: t -> Player.t

(** [update_player p g] mutates the [player] array in [game] g to updates
    values following the current player's move *)
val update_player: Player.t -> t -> unit

(** [get_all_players g] returns a list of all players in [game] g *)
val get_all_players: t -> Player.t array

(** [init_game b n] returns a [game] with [board] b and n players *)
val init_game: Board.t -> int -> t

(** [next_player g] returns a [game] g with an updated current player *)
val next_player: t -> t
