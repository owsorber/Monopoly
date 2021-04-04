(** The abstract type representing an action by a player *)
type t

(** The type representing the result of an attempted input. *)
type result =
  | Legal of t
  | Illegal

(**[get_action result player] is the action given by [result] that should be
    acting on player [player]*)
val get_action : t -> Player.t -> unit

(** [turn player board game phase] is the result of a queried input from a 
player during one turn, on phase 1 if [phase] is true and phase 2 if [phase] is 
false. It returns [Legal] of the desired move if the input is valid given game 
state [game] and board [board] or [Illegal] if the input is invalid. *)
val turn : Player.t -> Board.t -> Game.t -> bool -> result

    
