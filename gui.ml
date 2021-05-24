open Graphics

(* let play_sound filename = let buffer = SFSoundBuffer.loadFromFile
   ("./sounds/" ^ filename) in

   let sound = SFSound.create () in SFSound.setBuffer sound buffer;
   SFSound.play sound;

   while SFSound.getStatus sound = SFSound.Playing do SFTime.sleep
   (SFTime.of_seconds 0.1) done;

   SFSound.stop sound *)

(* draws a rectange of width w and height h at x y in color *)
let draw_color color x y w h =
  set_color color;
  fill_rect x y w h;
  set_color black;
  draw_rect x y w h

(* writes names of properties from list on their correct space *)
let rec write_name space_lst x y is_corner is_side =
  moveto x y;
  match space_lst with
  | [] -> ()
  | h :: t ->
      if String.length h <= 8 || is_corner then (
        draw_string h;
        write_name t x (y - 15) true false)
      else if is_side then (
        draw_string h;
        write_name t x (y - 15) false false)
      else (
        draw_string (String.sub h 0 8);
        let t = ("-" ^ String.sub h 8 (String.length h - 8)) :: t in
        write_name t x (y - 15) false false)

(* returns a tuple of the bottom left cooridinates for space i, orianted *)
let space_dim space_loc bottom space_width =
  if space_loc < 10 then
    ( ((11 - space_loc) * space_width) + 3,
      bottom + (5 * space_width / 4) )
  else if space_loc = 10 then (3, bottom + (5 * space_width / 4))
  else if space_loc < 20 then (3, ((space_loc - 7) * space_width) + 15)
  else if space_loc = 20 then (3, size_y () - (space_width / 2))
  else if space_loc <= 30 then
    (((space_loc - 19) * space_width) + 3, size_y () - (space_width / 2))
  else if space_loc < 40 then
    ( size_y () - bottom - (3 * space_width / 2),
      ((43 - space_loc) * space_width) + 15 )
  else (0, 0)

let house_place space_loc space_width board_y space_height =
  let x, y = space_dim space_loc board_y space_width in
  if space_loc < 10 then (x + 3, y + (space_height / 10))
  else if space_loc < 20 then (x + (3 * space_height / 4), y + 3)
  else if space_loc < 30 then (x + 3, y - (3 * space_height / 4))
  else if space_loc < 40 then (x - (space_height / 4), y + 3)
  else (0, 0)

let rec draw_house num x y is_horizontal color =
  set_color color;
  let length = 7 in
  if is_horizontal then draw_house_horizonal num x y length
  else draw_house_vertical num x y length

and draw_house_horizonal num x y length =
  match num with
  | 1 -> fill_rect x (y + 10) length length
  | 2 ->
      fill_rect x (y + 10) length length;
      fill_rect (x + (3 * length / 2)) (y + 10) length length
  | 3 ->
      fill_rect x (y + 10) length length;
      fill_rect (x + (3 * length / 2)) (y + 10) length length;
      fill_rect (x + (6 * length / 2)) (y + 10) length length
  | 4 ->
      fill_rect x (y + 10) length length;
      fill_rect (x + (3 * length / 2)) (y + 10) length length;
      fill_rect (x + (6 * length / 2)) (y + 10) length length;
      fill_rect (x + (9 * length / 2)) (y + 10) length length
  | 5 -> fill_rect x (y + 10) (length * 5) length
  | _ -> fill_circle x (y + 10) length

and draw_house_vertical num x y length =
  match num with
  | 1 -> fill_rect x y length length
  | 2 ->
      fill_rect x y length length;
      fill_rect x (y - (3 * length / 2)) length length
  | 3 ->
      fill_rect x y length length;
      fill_rect x (y - (3 * length / 2)) length length;
      fill_rect x (y - (3 * length)) length length
  | 4 ->
      fill_rect x y length length;
      fill_rect x (y - (3 * length / 2)) length length;
      fill_rect x (y - (3 * length)) length length;
      fill_rect x (y - (9 * length / 2)) length length
  | 5 -> fill_rect x (y - (9 * length / 2)) length (length * 5)
  | _ -> fill_circle x (y + 10) length

(* Draws houses and hotels *)
let rec make_house board_y ownables_lst g space_width space_height color
    =
  match ownables_lst with
  | [] -> ()
  | n :: t ->
      (let board = Game.get_board g in
       let space_loc = Board.location_from_space_name board n in
       if space_loc < 10 || (space_loc > 20 && space_loc < 30) then (
         let x, y =
           house_place space_loc space_width board_y space_height
         in
         draw_house (Game.get_houses g n) x y true color;
         make_house board_y t g space_width space_height color;
         set_color color)
       else
         let x, y =
           house_place space_loc space_width board_y space_height
         in
         set_color color;
         draw_house (Game.get_houses g n) x y false color);
      make_house board_y t g space_width space_height color

let rec make_piece
    bottom
    space_width
    p
    color1
    color2
    color3
    position
    g
    height =
  match p with
  | [] -> ()
  | h :: t ->
      let loc = Player.get_location h in
      let x = space_dim loc bottom space_width in
      let friends =
        List.length (List.filter (fun x -> x = loc) position)
      in
      move_player_under x color1 color2 color3 friends;
      if String.length (Player.get_player_id h) > 4 then
        draw_string (String.sub (Player.get_player_id h) 0 4)
      else draw_string (Player.get_player_id h);
      let color = rgb color1 color2 color3 in
      make_house bottom
        (Player.get_ownable_name_list h)
        g space_width height color;
      make_piece bottom space_width t
        ((color3 + 50) mod 250)
        ((color1 + 50) mod 250)
        ((color2 + 20) mod 250)
        (loc :: position) g height

and move_player_under x color1 color2 color3 friends =
  set_color (rgb color1 color2 color3);
  fill_rect (fst x + 15) (snd x - (20 * friends)) 30 15;
  set_color white;
  moveto (fst x + 15) (snd x - (20 * friends))

let create_console () =
  set_color black;
  fill_rect (size_x () / 2) 0 (size_x ()) (size_y ());
  set_color cyan;
  moveto ((size_x () / 2) + 10) (size_y () - 20);
  draw_string "Console";
  moveto ((size_x () / 2) + 10) (size_y () - 25);
  lineto (size_x ()) (size_y () - 25);
  moveto ((size_x () / 2) + 10) (size_y () - 40)

let make_label board bottom w =
  for i = 0 to 39 do
    let x, y = space_dim i bottom w in
    if i == 10 || i == 20 || i == 30 || i == 0 then
      write_name
        (String.split_on_char ' ' (Board.space_name board i))
        x y true false
    else if (i > 10 && i < 20) || i > 30 then
      write_name
        (String.split_on_char ' ' (Board.space_name board i))
        x y false true
    else
      write_name
        (String.split_on_char ' ' (Board.space_name board i))
        x y false false
  done

let rec make_board g =
  set_color (rgb 205 230 208);
  fill_rect 0
    (size_y () - (size_x () / 2))
    (size_x () / 2)
    (size_x () / 2);
  let space_width = size_x () / 26 in
  let space_height = space_width * 2 in
  let board_y = size_y () - (size_x () / 2) in
  draw_board board_y space_height space_width;
  let x = current_x () in
  let y = current_y () in
  make_label (Game.get_board g) board_y space_width;
  draw_m board_y;
  make_piece board_y space_width
    (Array.to_list (Game.get_all_players g))
    63 212 194 [] g space_height;
  moveto x y

and draw_board board_y space_height space_width =
  set_line_width 2;
  set_color black;
  draw_rect 0 board_y space_height space_height;
  draw_rect 0 (board_y + (11 * space_width)) space_height space_height;
  draw_rect (11 * space_width) board_y space_height space_height;
  draw_rect (11 * space_width)
    (board_y + (11 * space_width))
    space_height space_height;
  let color_height = space_height / 4 in
  draw_bottom_spaces board_y space_width space_height color_height;
  draw_top_spaces board_y space_width space_height color_height;
  draw_left_spaces board_y space_width space_height color_height;
  draw_right_spaces board_y space_width space_height color_height

and draw_bottom_spaces board_y space_width space_height color_height =
  for i = 2 to 10 do
    draw_rect (i * space_width) board_y space_width space_height;
    if i = 2 || i = 3 || i = 5 then
      draw_color (rgb 170 224 250) (i * space_width)
        (board_y + (3 * space_height / 4))
        space_width color_height
    else if i = 8 || i = 10 then
      draw_color (rgb 149 84 54) (i * space_width)
        (board_y + (3 * space_height / 4))
        space_width color_height
    else ()
  done

and draw_top_spaces board_y space_width space_height color_height =
  for i = 2 to 10 do
    draw_rect (i * space_width)
      (board_y + (11 * space_width))
      space_width space_height;
    if i = 2 || i = 4 || i = 5 then
      draw_color (rgb 222 34 40) (i * space_width)
        (board_y + (11 * space_width))
        space_width color_height
    else if i = 7 || i = 8 || i = 10 then
      draw_color (rgb 254 242 0) (i * space_width)
        (board_y + (11 * space_width))
        space_width color_height
    else ()
  done

and draw_left_spaces board_y space_width space_height color_height =
  for i = 2 to 10 do
    draw_rect 0 (board_y + (i * space_width)) space_height space_width;
    if i = 2 || i = 4 || i = 5 then
      draw_color (rgb 217 58 150)
        (3 * space_height / 4)
        (board_y + (i * space_width))
        color_height space_width
    else if i = 7 || i = 9 || i = 10 then
      draw_color (rgb 247 148 29)
        (3 * space_height / 4)
        (board_y + (i * space_width))
        color_height space_width
    else ()
  done

and draw_right_spaces board_y space_width space_height color_height =
  for i = 2 to 10 do
    draw_rect (11 * space_width)
      (board_y + (i * space_width))
      space_height space_width;
    if i = 7 || i = 9 || i = 10 then
      draw_color (rgb 32 177 90) (11 * space_width)
        (board_y + (i * space_width))
        color_height space_width
    else if i = 2 || i = 4 then
      draw_color (rgb 0 114 187) (11 * space_width)
        (board_y + (i * space_width))
        color_height space_width
    else ()
  done

and draw_m board_y =
  set_color (rgb 225 245 227);
  fill_circle (size_x () / 4) ((size_x () / 4) + board_y) (size_x () / 6);
  set_color (rgb 164 186 166);
  set_line_width 20;
  moveto (size_x () / 4) ((size_x () / 4) + board_y);
  lineto (size_x () / 4 * 7 / 10) (((size_x () / 4) + board_y) * 10 / 7);
  lineto (current_x ()) (current_y () - (3 * size_x () / 12));
  moveto (size_x () / 4) ((size_x () / 4) + board_y);
  lineto (size_x () / 4 * 13 / 10) (((size_x () / 4) + board_y) * 10 / 7);
  lineto (current_x ()) (current_y () - (3 * size_x () / 12));
  set_line_width 1

let stock_change_color percent_change =
  let other_change = int_of_float (percent_change /. 100. *. 255.) in
  if percent_change < 0. then rgb 255 other_change other_change
  else if percent_change > 0. then rgb other_change 255 other_change
  else rgb 216 221 227

let draw_stock stock_name stock_value percent_change x y w h =
  (* set_color (rgb 216 221 227); *)
  (* print_endline (string_of_float percent_change); *)
  set_color (stock_change_color percent_change);
  fill_rect x y w h;
  set_color black;
  draw_rect x y w h;
  moveto (x + (w / 10)) (y + (2 * h / 3));
  draw_string ("Stock: " ^ stock_name);
  moveto (x + (w / 10)) (y + (h / 3));
  draw_string ("Value: " ^ string_of_int stock_value)

let rec make_stockmarket m =
  let stock_names = Stockmarket.stock_array m in
  let stock_values =
    Array.map (fun name -> Stockmarket.value_of m name) stock_names
  in
  let stock_changes =
    Array.map
      (fun name -> Stockmarket.percent_change_of m name)
      stock_names
  in
  let starting_x = current_x () in
  let starting_y = current_y () in
  let height = size_y () - (size_x () / 2) in
  let width = size_x () / 2 in
  draw_stocks_boxes width height;
  draw_stocks width height stock_values stock_changes stock_names;
  moveto starting_x starting_y

and draw_stocks_boxes width height =
  set_color (rgb 204 227 255);
  fill_rect 0 0 width height;
  set_line_width 2;
  set_color black;
  draw_rect 0 0 width height;
  set_line_width 1

and draw_stocks width height stock_values stock_changes stock_names =
  let boxh = height - (2 * height / 5) in
  let boxy = (height - boxh) / 4 in
  let boxgap = width / 50 in
  let num_stocks =
    let len = Array.length stock_names in
    if len < 5 then len else 5
  in
  let boxw = (width - (boxgap * (num_stocks + 1))) / num_stocks in
  moveto ((width / 2) - (width / 15)) (height - (height / 5));
  draw_string "Stock Tracker";
  lineto ((width / 2) - (width / 15)) (height - (height / 5));
  Array.iteri
    (fun i name ->
      if i <= num_stocks then
        draw_stock name stock_values.(i) stock_changes.(i)
          (((i + 1) * boxgap) + (i * boxw))
          boxy boxw boxh)
    stock_names

let wipe_console () =
  set_color black;
  fill_rect (size_x () / 2) 0 (size_x () / 2) (size_y ())

let scroll () =
  let above = get_image (size_x () / 2) 0 (size_x () / 2) (size_y ()) in
  wipe_console ();
  draw_image above (size_x () / 2) 20;
  moveto ((size_x () / 2) + 10) 0

let update_console s c =
  set_color c;
  draw_string s;
  moveto ((size_x () / 2) + 10) (current_y () - 20);
  if current_y () < 0 then scroll () else ();
  Unix.sleepf 0.025

let input_print s c =
  set_color black;
  fill_rect (size_x () / 2) (current_y ()) (size_x () / 2) 40;
  moveto ((size_x () / 2) + 10) (current_y () + 20);
  update_console s c

let create_window g m =
  open_graph "";
  resize_window 1440 810;
  set_window_title "Monopoly";
  create_console ();
  make_board g;
  make_stockmarket m

(* play_sound "intro.wav" *)

let wipe_game () =
  set_color white;
  fill_rect 0 0 (size_x () / 2) (size_y ())

let update_frame g m =
  wipe_game ();
  make_board g;
  make_stockmarket m
