(** Interface for the graphical user interface.

    This module allows other modules to create or update information to
    be displayed to the end user of the game.*)

(**[create_window game market] creates the visual monopoly board on the
   left side of the window, a visual stock market below the board, and a
   written console on the right side of the window, given the current
   game [game] and stock market [market]. *)
val create_window : Game.t -> Stockmarket.t -> unit

(**[update_frame game market] updates the board and stock market on the
   user interface. The board will now display the new game state [game]
   and the stock market will now display the new stock market [market]. *)
val update_frame : Game.t -> Stockmarket.t -> unit

(**[update_console s c] prints the string [s] in the color [c] to the
   console on the right side of the interface in one line. *)
val update_console : string -> Graphics.color -> unit

(**[wipe_console ()] clears the console on the right side of the
   interface. *)
val wipe_console : unit -> unit

(**[input_print s c] is a special type of printing to the console, where
   the last line printed in the console is replaced with [s] written in
   color [c]. *)
val input_print : string -> Graphics.color -> unit
