(**Representation of a monopoly board with different spaces and
   information about them.

   This module represents the board composed of spaces, which can be a
   Property, Railroad, Utility, Tax, Go, Chance, CommunityChest,
   Quarantine, FreeParking, or GoToQuarantine. Methods to query
   information about spaces or the board are also provided. *)

(** The abstract type representing a Board *)
type t

(** The type representing a space. *)
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

(** Raised when a space name is provided but is not present on the
    board. *)
exception NameNotOnBoard of string

(** Raised when the color is asked for from a space that does not have a
    color on the Monopoly board. *)
exception SpaceDoesNotHaveColor

(** Raised when a color does not exist on a board. *)
exception BoardDoesNotHaveColor of string

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

(* [color board s] returns the color of space [s]. Raises
   [SpaceDoesNotHaveColor] if [space] is not a property. *)
val color : t -> space -> string

(* [num_of_color board col] returns the number of properties in [board]
   with the color [col]. Raises: [BoardDoesNotHaveColor col] if [col] is
   not the color of any property in [board]. *)
val num_of_color : t -> string -> int

(** [location_from_space_name b str] returns an int representing the
    location of s if there exists space s on board [b] with name [str].
    requires: [str] is the only space with that name. *)
val location_from_space_name : t -> string -> int
