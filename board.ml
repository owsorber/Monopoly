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

exception BoardDoesNotHaveColor of string

type t = {
  spaces : space array;
  num_color : (string, int) Hashtbl.t;
}

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
let rec space_from_json j =
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
  | _ -> space_from_json' space_type j
  
and space_from_json' space_type j=
match space_type with
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

let init_spaces json =
  let board_list = to_list json |> init_board_helper [] in
  List.rev board_list |> Array.of_list

let make_color_hashmap spaces hashtbl =
  let () =
    for i = 0 to Array.length spaces - 1 do
      match spaces.(i) with
      | Property p ->
          let color = p.color in
          if Hashtbl.mem hashtbl color then
            Hashtbl.replace hashtbl color
              (Hashtbl.find hashtbl color + 1)
          else Hashtbl.add hashtbl color 1
      | _ -> ()
    done
  in
  hashtbl

let init_board json =
  let space_array = init_spaces json in
  {
    spaces = space_array;
    num_color =
      Array.length space_array
      |> Hashtbl.create
      |> make_color_hashmap space_array;
  }

let length board = Array.length board.spaces

let space_from_location board loc =
  try board.spaces.(loc) with Invalid_argument _ -> raise (NotOnBoard loc)

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
  | Quarantine -> "Quarantine"
  | FreeParking -> "Free Parking"
  | GoToQuarantine -> "Go To Quarantine"

let rec space_from_space_name_helper board acc s =
  if acc >= length board then -1
  else if space_name board acc = s then acc
  else space_from_space_name_helper board (acc + 1) s

let space_from_space_name board space_name =
  let loc = space_from_space_name_helper board 0 space_name in
  if loc = -1 then raise (NameNotOnBoard space_name)
  else Some board.spaces.(loc)

let start_space board = space_name board 0

let color board space =
  match space with
  | Property p -> p.color
  | _ -> raise SpaceDoesNotHaveColor

let num_of_color board color =
  try Hashtbl.find board.num_color color
  with Not_found -> raise (BoardDoesNotHaveColor color)

let location_from_space_name board name =
  space_from_space_name_helper board 0 name
