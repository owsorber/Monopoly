(** Representation of a player's action and the result of their actions

    This module represents a player's action, and contains methods that
    modify the overall game state based on these actions. Allows players
    to input their game choices, which affect the game accordingly. *)

(** The abstract type representing an action by a player *)
type t

(** The type [result] represents the result of an attempted input. *)
type result =
  | Legal of t
  | Illegal

(** [get_action result player] is the action given by [result] that
    should be acting on player [player]*)
val get_action : t -> Player.t -> unit

(** [get_double result] is true iff the action of [result] is a roll and
    both numbers of that roll are the same. *)
val get_double : t -> bool

(** [get_end result] is true iff the action of [result] is to end the
    turn. *)
val get_end : t -> bool

(** [graceful_shutdown b g] ends the game [g] given board [b] and prints
    endgame statistics*)
val graceful_shutdown : Board.t -> Game.t -> unit

(** [turn player board game phase cards market] is the result of a
    queried input from a player during one turn, on phase 1 if [phase]
    is true and phase 2 if [phase] is false. It returns [Legal] of the
    desired move if the input is valid given game state [game], board
    [board], and market [market] or [Illegal] if the input is invalid. *)
val turn :
  Player.t ->
  Board.t ->
  Game.t ->
  bool ->
  Cards.t ->
  Stockmarket.t ->
  result
