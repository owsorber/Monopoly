(** Representation of the Monopoly Stockmarket

    This module contains methods that allow querying of specific stock
    market shares, and updating the market based off multiple
    probabilistic formulas *)

(** The abstract type representing a stock market. *)
type t

(** The type [stock_name] represents the name of a stock contained in
    the stockmarket. *)
type stock_name = string

(** The type [stock_value] represents the value (price) of a single
    share of a stock in the market. *)
type stock_value = int

(** [init_market j] initializes and returns a stock market based on json
    [j]. Requires: [j] is a valid JSON representation for the way we've
    defined a stock market. *)
val init_market : Yojson.Basic.t -> t

(** [value_of m s] is the value corresponding to the stock with name [s]
    in market [m]. *)
val value_of : t -> stock_name -> stock_value

(** [value_of_num_shares m s n] returns the value of [n] shares of stock
    with name [s] in market [m]. *)
val value_of_num_shares : t -> stock_name -> int -> stock_value

(** [percent_change_of m s] is the percent change in value corresponding
    to the stock with name [s] in market [m] since the previous update
    of market [m]. *)
val percent_change_of : t -> stock_name -> float

(** [update_market m] updates a stock market [m] after a single turn,
    which corresponds to a time-step. *)
val update_market : t -> unit

(** [stock_array m] returns an array containing all of the stock names
    in market [m]. *)
val stock_array : t -> stock_name array
