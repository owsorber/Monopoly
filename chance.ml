open Yojson.Basic.Util

type card = {
  card_number: int;
  message: string;
}

type t =  {
deck: card list;
mutable stack: int array
}

let card_from_json j = {
  card_number  = j |> member "number" |> to_int;
  message  = j |> member "title" |> to_string 
}

let rec init_chance_helper builder spaces =
  match spaces with
    | [] -> builder
    | h :: t -> init_chance_helper (card_from_json h :: builder) t

(** uses knuth algorithum*)
let shuffle l = 
  let stack = Array.init 15 (fun x -> x) in 
  for i = l-1 downto 1 do
    let place = Random.int (i+1) in
      let swaper = stack.(place) in
        stack.(place) <- stack.(i);
        stack.(i) <- swaper
  done;
  stack

let init_board json ={
  deck = to_list json |> init_chance_helper [];
  stack = shuffle (List.length (to_list json))
  }

let draw_card stack = failwith "unimplemented"