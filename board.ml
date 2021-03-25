open Yojson.Basic.Util

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

exception NotOnBoard of int

(* TODO: replace [unit] with a type to represent a board *)
type t = unit

let init_board json =
  failwith "Not Implemented"

let space_from_location board i =
  failwith "Not Implemented"

let space_name board i =
  failwith "Not Implemented"

let start_space board =
  failwith "Not Implemented"