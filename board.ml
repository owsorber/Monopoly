open Yojson.Basic.Util

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

exception NotOnBoard of int

exception NameNotOnBoard of string

exception SpaceDoesNotHaveColor


type t = space array

(* Creates a rent array from a JSON list of rent values *)
let create_rent_array j =
  let rec create_int_list init j =
    match j with
    | [] -> init
    | h :: t -> create_int_list (to_int h :: init) t
  in
  List.rev j |> create_int_list [] |> Array.of_list

(* Creates a space with the Property constructor from json; assumes json
   struct has type "property". *)
let property_from_json j =
  Property
    {
      name = j |> member "name" |> to_string;
      price = j |> member "cost" |> to_int;
      house_price = j |> member "house" |> to_int;
      color = j |> member "color" |> to_string;
      rent = j |> member "rent" |> to_list |> create_rent_array;
    }

(* Creates a space from the json for a space by pattern-matching against
   "type". *)
let space_from_json j =
  let space_type = j |> member "type" |> to_string in
  match space_type with
  | "property" -> property_from_json j
  | "railroad" ->
      Railroad
        {
          name = j |> member "name" |> to_string;
          price = j |> member "cost" |> to_int;
        }
  | "utility" ->
      Utility
        {
          name = j |> member "name" |> to_string;
          price = j |> member "cost" |> to_int;
        }
  | "tax" ->
      Tax
        {
          name = j |> member "name" |> to_string;
          cost = j |> member "cost" |> to_int;
        }
  | "go" -> Go
  | "chance" -> Chance
  | "community-chest" -> CommunityChest
  | "jail" -> Quarantine
  | "free-parking" -> FreeParking
  | "go-to-jail" -> GoToQuarantine
  | _ ->
      failwith ("Board JSON File gave invalid space type " ^ space_type)

(* Generates a space list from json *)
let rec init_board_helper builder spaces =
  match spaces with
  | [] -> builder
  | h :: t -> init_board_helper (space_from_json h :: builder) t

let init_board json =
  let board_list = to_list json |> init_board_helper [] in
  List.rev board_list |> Array.of_list

let length board = Array.length board

let space_from_location board i =
  try board.(i) with Invalid_argument _ -> raise (NotOnBoard i)

  let space_from_space_name_helper board space_name = 
    Array.fold_left (fun i acc ->
    match board.(i) with
  | Property p -> if (space_name = p.name) then acc + i +1  else acc
  | Railroad r -> if (space_name = r.name) then acc + i +1  else acc
  | Utility u -> if (space_name = u.name) then acc + i +1  else acc
  | Tax t -> if (space_name = t.name) then acc + i +1  else acc
  | Go -> if (space_name = "Go") then acc + i +1  else acc
  | Chance -> if (space_name = "Chance") then acc + i +1  else acc
  | CommunityChest -> if (space_name = "Community Chest") then acc +i+1 else acc
  | Quarantine -> if (space_name = "Jail") then acc + i +1  else acc
  | FreeParking -> if (space_name = "Free Parking") then acc + i +1  else acc
  | GoToQuarantine -> if (space_name = "Go To Jail") then acc + i +1  else acc) 
  (-1) (Array.init (length board) (fun x -> x))

let space_from_space_name board space_name = 
  let i = space_from_space_name_helper board space_name in 
  if i = (-1) then raise (NameNotOnBoard space_name) else Some board.(i)
  
let is_ownable board space =
  match space with
  | Property _ -> true
  | Railroad _ -> true
  | Utility _ -> true
  | _ -> false

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
  | Quarantine -> "Jail"
  | FreeParking -> "Free Parking"
  | GoToQuarantine -> "Go To Jail"

let start_space board = space_name board 0

let make_color_hashmap hashtbl board = 
  for i = 0 to length board - 1  do 
  match board.(i) with
  | Property p -> let color = p.color in if (Hashtbl.mem hashtbl color) then 
    Hashtbl.replace hashtbl color ((Hashtbl.find hashtbl color)+1) else 
      Hashtbl.add hashtbl 
    color 1;
  |_ ->  () 
    done

let color board space = 
  match space with
  |Property p -> p.color
  |_ -> raise (SpaceDoesNotHaveColor)

let num_of_color board color = failwith "Unimplemented"
