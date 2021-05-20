(** The abstract type representing a stock market. *)
type t

type stock_name = string

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
val value_of : t -> stock_name -> stock_value

(** [update_market m] updates a stock market [m] after a single turn,
    which corresponds to a time-step. *)
val update_market : t -> unit

val stock_array : t -> stock_name array
