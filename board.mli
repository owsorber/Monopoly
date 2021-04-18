(** The abstract type representing a Board *)
type t

type space =
  | Property of {
      name : string;
      price : int;
      house_price : int;
      color : string;
      rent : int array;
    }
  | Railroad of {
      name : string;
      price : int;
    }
  | Utility of {
      name : string;
      price : int;
    }
  | Tax of {
      name : string;
      cost : int;
    }
  | Go
  | Chance
  | CommunityChest
  | Quarantine
  | FreeParking
  | GoToQuarantine

(** Raised when an integer does not refer to a valid 0-indexed location
    on the Monopoly board. *)
exception NotOnBoard of int

(** [init_board json] returns the board represented by [json]. Requires:
    [json] is a valid JSON representation of a standard Monopoly Board. *)
val init_board : Yojson.Basic.t -> t

(** [length b] is the number of spaces in board [b] *)
val length : t -> int

(** [space_from_location board i] returns value of type space
    representing the space at location [i] on [board], starting from
    "Go" at 0 and counting up by 1 around the board clockwise. Requires:
    [i] refers to a valid 0-indexed space on [board] Raises:
    [NotOnBoard] if [i] does not indicate a valid space on [board] *)
val space_from_location : t -> int -> space

(** [space_from_space_name b str] returns Some s if there exists space s
    on board [b] with name [str]. *)
val space_from_space_name : t -> string -> space option

(** [is_ownable s] returns true iff [s] is an ownable space. More
    specifically, an "ownable space" is either a property, railroad, or
    utility. *)
val is_ownable : t -> space -> bool

(** [space_name board i] returns a string holding the name of the space
    at location [i] on [board], starting from "Go" at 0 and counting up
    by 1 around the board clockwise.

    Ex: [space_name board 1] should return "Mediterranean Avenue" if
    [board] is created from "board.json".

    Requires: [i] refers to a valid 0-indexed space on [board] Raises:
    [NotOnBoard] if [i] does not indicate a valid space on [board] *)
val space_name : t -> int -> string

(** [start_space board] returns the name of the space all players start
    on in [board]. *)
val start_space : t -> string

(* [color board s] returns the color of space [s]. *)
val color : t -> space -> string

(* [num_of_color board col] returns the number of properties in [board]
   with the color [col]. *)
val num_of_color : t -> string -> int
