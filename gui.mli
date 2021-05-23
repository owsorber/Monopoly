(** creates board*)

val create_window : Game.t -> Stockmarket.t -> unit

val update_frame : Game.t -> Stockmarket.t -> unit

val update_console : string -> Graphics.color -> unit

val wipe_console : unit -> unit

val input_print : string -> Graphics.color -> unit

(* val play_sound : string -> unit *)
