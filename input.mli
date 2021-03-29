(** The abstract type representing an action by a player *)
type t

(** The type representing the result of an attempted input. *)
type result =
  | Legal of t
  | Illegal

(** *)
val get_action : t -> Player.t -> unit

(** [turn player game] is the result of a queried input from a player
    during one turn. It returns [Legal] of the desired move if the input
    is valid given game state [game] or [Illegal] if the input is
    invalid. *)
val turn : Player.t -> Game.t -> result
