open Graphics

let create_console () =
  set_color black;
  fill_rect (size_x () / 2) 0 (size_x ()) (size_y ());
  set_color cyan;
  moveto ((size_x () / 2) + 10) (size_y () - 20);
  draw_string "Console";
  moveto ((size_x () / 2) + 10) (size_y () - 25);
  lineto (size_x ()) (size_y () - 25);
  moveto ((size_x () / 2) + 10) (size_y () - 40)

let wipe_console () =
  clear_graph ();
  create_console ()
(* set_color black; fill_rect 0 0 (size_x ()) 300; create_console ();
   moveto 10 280 *)

let update_console s c =
  set_color c;
  if current_y () < 0 then wipe_console () else ();
  draw_string s;
  moveto ((size_x () / 2) + 10) (current_y () - 20)

let create_window g =
  open_graph "";
  resize_window 1440 810;
  set_window_title "Monopoly";
  create_console ();
  sound 262 10000
(* let input = Char.escaped (read_key ()) in update_console input *)

let update_frame g = ()

(* let main () = failwith "Unimplemented" let () = main () *)
