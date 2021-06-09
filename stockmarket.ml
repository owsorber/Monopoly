open Yojson.Basic.Util

type stock_name = string

type stock_value = int

(* A stock has a current value, an expected return (mu), and a standard
   deviation of return (sigma). [mu] and [sigma] are numbers from 0 to 1
   representing a proportion of the value of the stock. *)
type stock_info = {
  curr_value : stock_value;
  percent_change : float;
  mu : float;
  sigma : float;
}

type t = (stock_name, stock_info) Hashtbl.t

(* Initializes a tuple of stock name paired with stock info for a given
   correctly formatted piece of JSON that represents a stock. *)
let init_stock json =
  let name = json |> member "name" |> to_string in
  let info =
    {
      curr_value = json |> member "start" |> to_int;
      mu = json |> member "mu" |> to_float;
      sigma = json |> member "sigma" |> to_float;
      percent_change = 0.0;
    }
  in
  (name, info)

(* Helper to initialize a stock market. *)
let rec init_market_helper table = function
  | [] -> table
  | h :: t ->
      let name, info = init_stock h in
      Hashtbl.add table name info;
      init_market_helper table t

let init_market json =
  let tbl = Hashtbl.create 5 in
  json |> to_list |> init_market_helper tbl

let info_of m s = Hashtbl.find m s

let value_of m s =
  let info = info_of m s in
  info.curr_value

let value_of_num_shares m s n = n * value_of m s

let percent_change_of m s =
  let info = info_of m s in
  info.percent_change

let expected_return m s =
  let info = info_of m s in
  info.mu

let st_dev_return m s =
  let info = info_of m s in
  info.sigma

(* Approximates a normal distribution with mean = zero and standard
   deviation [sigma]. This is used to simulate the volatility of a
   stock's price by returning a normally distributed coefficient for the
   "shock" term.

   Uses the Central Limit Theorem: the sum of a large enough amount of
   uniformly distributed random variables should be approximately
   normally distributed. The input [n] is used to specify the number of
   uniformly distributed random variables to use. *)
let gen_approx_zero_mean_gaussian sigma n =
  let a = sqrt (3. /. float_of_int n) *. sigma in
  Random.self_init ();
  let rec sum_uniform acc num =
    if num = 0 then acc
    else sum_uniform (acc +. Random.float (2. *. a) -. a) (num - 1)
  in
  sum_uniform 0. n

(* Calculates the new stock value and percent change for a stock with
   name [s] in market [m] after one time-step using the formula
   documented here:
   https://www.investopedia.com/articles/07/montecarlo.asp *)
let new_stock_value_and_change m s =
  let curr_value = float_of_int (value_of m s) in
  let epsilon = gen_approx_zero_mean_gaussian 1. 50 in
  let mu = expected_return m s in
  let sigma = st_dev_return m s in
  let percent_change = mu +. (epsilon *. sigma) in
  ( max 0. (curr_value +. (curr_value *. percent_change))
    |> Float.round |> int_of_float,
    percent_change )

(* Updates stock with name [s] in market [m]. *)
let update_stock m s info =
  let value, change = new_stock_value_and_change m s in
  Hashtbl.replace m s
    { info with curr_value = value; percent_change = 100. *. change }

let update_market m = Hashtbl.iter (update_stock m) m

let stock_array m =
  Hashtbl.fold (fun k _ acc -> Array.append acc [| k |]) m [||]
