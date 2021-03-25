(** The abstract type representing a Board *)
type t

type space =
  | Property of {name: string; price: int; house_price: int; color: string; 
                  rent: int list}
  | Railroad of {name: string; price: int}
  | Utility of {name: string; price: int}
  | Tax of {name: string; cost: int}
  | Go
  | Chance
  | CommunityChest
  | Jail
  | FreeParking
  | GoToJail

(** Raised when an integer does not refer to a valid 0-indexed location on the
    Monopoly board. *)
exception NotOnBoard of int

(** [init_board json] returns the board represented by [json].
    Requires: [json] is a valid JSON representation of a standard Monopoly 
    Board. *)
val init_board : Yojson.Basic.t -> t

(** [space_from_location board i] returns value of type space representing the 
    space at location [i] on [board], starting from "Go" at 0 and counting up 
    by 1 around the board clockwise.
    Requires: [i] refers to a valid 0-indexed space on [board]
    Raises: [NotOnBoard] if [i] does not indicate a valid space on [board] *)
val space_from_location : t -> int -> space

(** [space_name board i] returns a string holding the name of the space at 
    location [i] on [board], starting from "Go" at 0 and counting up by 1 around
    the board clockwise.

    Ex: [space_name board 1] should return "Mediterranean Avenue" if [board] is 
    created from "board.json".

    Requires: [i] refers to a valid 0-indexed space on [board]
    Raises: [NotOnBoard] if [i] does not indicate a valid space on [board] *)
val space_name : t -> int -> string

(** [start_space board] returns the name of the space all players start on in 
    [board]. *)
val start_space : t -> string