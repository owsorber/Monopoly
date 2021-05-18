open Graphics

(* let play_sound filename = let buffer = SFSoundBuffer.loadFromFile
   ("./sounds/" ^ filename) in

   let sound = SFSound.create () in SFSound.setBuffer sound buffer;
   SFSound.play sound;

   while SFSound.getStatus sound = SFSound.Playing do SFTime.sleep
   (SFTime.of_seconds 0.1) done;

   SFSound.stop sound *)

let draw_color color x y w h =
  set_color color;
  fill_rect x y w h;
  set_color black;
  draw_rect x y w h

let rec write_name lst x y =
  moveto x y;
  match lst with
  | [] -> ()
  | h :: t ->
      if String.length h <= 8 then (
        draw_string h;
        write_name t x (y - 15))
      else (
        draw_string (String.sub h 0 8);
        let t = ("-" ^ String.sub h 8 (String.length h - 8)) :: t in
        write_name t x (y - 15))

(* returns a tuple of the bottom left cooridinates for space i, orianted *)
let space_dim i l w =
  if i < 10 then (((11 - i) * w) + 3, l + (5 * w / 4))
  else if i < 20 then (3, ((i - 7) * w) + 15)
  else if i < 30 then (((i - 19) * w) + 3, size_y () - (w / 2))
  else if i = 30 then (((i - 18) * w) + 3, size_y () - w)
  else if i < 40 then (size_y () - l - (3 * w / 2), ((43 - i) * w) + 15)
  else (0, 0)

let rec make_piece l w p c1 c2 c3 pos =
  match p with
  | [] -> ()
  | h :: t ->
      let loc = Player.get_location h in
      let x = space_dim loc l w in
      let friends = List.length (List.filter (fun x -> x = loc) pos) in
      set_color (rgb c1 c2 c3);

      (* if (snd x - (20*friends)) > l then let x_pos = (fst x + 45) in
         let y_pos = snd x in fill_rect x_pos (y_pos) 30 15; set_color
         white; moveto x_pos y_pos; draw_string (String.sub
         (Player.get_player_id h) 0 4); make_piece l w t ((c1+20) mod
         250) c2((c3 +7) mod 250) (loc::pos) else *)
      fill_rect (fst x + 15) (snd x - (20 * friends)) 30 15;
      set_color white;
      moveto (fst x + 15) (snd x - (20 * friends));
      if String.length (Player.get_player_id h) > 4 then
        draw_string (String.sub (Player.get_player_id h) 0 4)
      else draw_string (Player.get_player_id h);
      make_piece l w t
        ((c3 + 50) mod 250)
        ((c1 + 50) mod 250)
        ((c2 + 20) mod 250)
        (loc :: pos)

let create_console () =
  set_color black;
  fill_rect (size_x () / 2) 0 (size_x ()) (size_y ());
  set_color cyan;
  moveto ((size_x () / 2) + 10) (size_y () - 20);
  draw_string "Console";
  moveto ((size_x () / 2) + 10) (size_y () - 25);
  lineto (size_x ()) (size_y () - 25);
  moveto ((size_x () / 2) + 10) (size_y () - 40)

let make_label board l w =
  for i = 0 to 39 do
    let x = space_dim i l w in
    write_name
      (String.split_on_char ' ' (Board.space_name board i))
      (fst x) (snd x)
  done

let make_board g =
  set_color (rgb 205 230 208);
  fill_rect 0
    (size_y () - (size_x () / 2))
    (size_x () / 2)
    (size_x () / 2);
  let space_width = size_x () / 26 in
  let space_height = space_width * 2 in
  let board_y = size_y () - (size_x () / 2) in
  let color_h = space_height / 4 in
  set_line_width 2;
  set_color black;
  draw_rect 0 board_y space_height space_height;
  draw_rect 0 (board_y + (11 * space_width)) space_height space_height;
  draw_rect (11 * space_width) board_y space_height space_height;
  draw_rect (11 * space_width)
    (board_y + (11 * space_width))
    space_height space_height;

  (* bottom line *)
  for i = 2 to 10 do
    draw_rect (i * space_width) board_y space_width space_height;
    if i = 2 || i = 3 || i = 5 then
      draw_color (rgb 170 224 250) (i * space_width)
        (board_y + (3 * space_height / 4))
        space_width color_h
    else ();
    if i = 8 || i = 10 then
      draw_color (rgb 149 84 54) (i * space_width)
        (board_y + (3 * space_height / 4))
        space_width color_h
    else ()
  done;

  (*top line*)
  for i = 2 to 10 do
    draw_rect (i * space_width)
      (board_y + (11 * space_width))
      space_width space_height;
    if i = 2 || i = 4 || i = 5 then
      draw_color (rgb 222 34 40) (i * space_width)
        (board_y + (11 * space_width))
        space_width color_h
    else ();
    if i = 7 || i = 8 || i = 10 then
      draw_color (rgb 254 242 0) (i * space_width)
        (board_y + (11 * space_width))
        space_width color_h
    else ()
  done;

  (* left side *)
  for i = 2 to 10 do
    draw_rect 0 (board_y + (i * space_width)) space_height space_width;
    if i = 2 || i = 4 || i = 5 then
      draw_color (rgb 217 58 150)
        (3 * space_height / 4)
        (board_y + (i * space_width))
        color_h space_width
    else ();
    if i = 7 || i = 9 || i = 10 then
      draw_color (rgb 247 148 29)
        (3 * space_height / 4)
        (board_y + (i * space_width))
        color_h space_width
    else ()
  done;

  (* right side *)
  for i = 2 to 10 do
    draw_rect (11 * space_width)
      (board_y + (i * space_width))
      space_height space_width;
    if i = 7 || i = 9 || i = 10 then
      draw_color (rgb 32 177 90) (11 * space_width)
        (board_y + (i * space_width))
        color_h space_width
    else ();
    if i = 2 || i = 4 then
      draw_color (rgb 0 114 187) (11 * space_width)
        (board_y + (i * space_width))
        color_h space_width
    else ()
  done;
  let x = current_x () in
  let y = current_y () in

  make_label (Game.get_board g) board_y space_width;

  (* draw m *)
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
  set_line_width 1;

  make_piece board_y space_width
    (Array.to_list (Game.get_all_players g))
    63 212 194 [];
  moveto x y

let wipe_console () =
  let lower_quarter =
    get_image (size_x () / 2) 0 (size_x ()) (size_y () / 4)
  in
  set_color black;
  fill_rect (size_x () / 2) 0 (size_x () / 2) (size_y ());
  draw_image lower_quarter (size_x () / 2) (3 * size_y () / 4);
  moveto ((size_x () / 2) + 10) ((3 * size_y () / 4) - 20)

let update_console s c =
  set_color c;
  if current_y () < 0 then wipe_console () else ();
  draw_string s;
  moveto ((size_x () / 2) + 10) (current_y () - 20);
  Unix.sleepf 0.05

let input_print s c =
  set_color black;
  fill_rect (current_x ()) (current_y ()) ((size_x () / 2) + 10) 20;
  moveto ((size_x () / 2) + 10) (current_y () + 20);
  update_console s c

let create_window g =
  open_graph "";
  resize_window 1440 810;
  set_window_title "Monopoly";
  create_console ();
  make_board g
(* play_sound "intro.wav" *)

let update_frame g = ()