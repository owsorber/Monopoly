open Yojson.Basic.Util

type space =
  | Property of {name: string; price: int; house_price: int; color: string; 
                  rent: int array}
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

type t = space array

(* Creates a rent array from a JSON list of rent values *)
let create_rent_array j =
  let rec create_int_list init j =
    match j with
    | [] -> init
    | h :: t -> create_int_list (to_int h :: init) t in
  List.rev j |> create_int_list [] |> Array.of_list

(* Creates a space with the Property constructor from json; assumes json struct 
    has type "property". *)
let property_from_json j =
  Property {
    name = j |> member "name" |> to_string; 
    price = j |> member "cost" |> to_int; 
    house_price = j |> member "house" |> to_int; 
    color = j |> member "color" |> to_string;
    rent = j |> member "rent" |> to_list |> create_rent_array
  }

(* Creates a space from the json for a space by pattern-matching against 
  "type". *)
let space_from_json j =
  let space_type = j |> member "type" |> to_string in
  match space_type with
  | "property" -> property_from_json j
  | "railroad" -> Railroad {
    name = j |> member "name" |> to_string; 
    price = j |> member "cost" |> to_int }
  | "utility" -> Utility {
    name = j |> member "name" |> to_string; 
    price = j |> member "cost" |> to_int }
  | "tax" -> Tax {
    name = j |> member "name" |> to_string; 
    cost = j |> member "cost" |> to_int }
  | "go" -> Go
  | "chance" -> Chance
  | "community-chest" -> CommunityChest
  | "jail" -> Jail
  | "free-parking" -> FreeParking
  | "go-to-jail" -> GoToJail
  | _ -> failwith "Board JSON File gave invalid space type"

(* Generates a space list from json *)
let rec init_board_helper builder spaces =
  match spaces with
  | [] -> builder
  | h :: t -> init_board_helper (space_from_json h :: builder) t

let init_board json =
  let board_list = to_list json |> init_board_helper [] in
  List.rev board_list |> Array.of_list

let space_from_location board i =
  try board.(i) with
  | Invalid_argument _ -> raise (NotOnBoard i)


let space_name board i =
  let s = space_from_location board i in
  match s with
  | Property p -> p.name
  | Railroad r -> r.name
  | Utility u -> u.name
  | Tax t -> t.name
  | Go -> "Go"
  | Chance -> "Chance"
  | CommunityChest -> "Community Chest"
  | Jail -> "Jail"
  | FreeParking -> "Free Parking"
  | GoToJail -> "Go To Jail"

let start_space board =
  space_name board 0