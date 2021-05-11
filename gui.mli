(** creates board*)

val create_window : Game.t -> unit

val update_frame : Game.t -> unit

val update_console : string -> Graphics.color -> unit

val wipe_console : unit -> unit

val play_sound : string -> unit
