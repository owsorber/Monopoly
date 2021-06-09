open Graphics

let terminal_cyan_print =
  ANSITerminal.print_string [ ANSITerminal.cyan ]

let termainal_magenta_print =
  ANSITerminal.print_string [ ANSITerminal.magenta ]

let terminal_red_print = ANSITerminal.print_string [ ANSITerminal.red ]

let terminal_green_print =
  ANSITerminal.print_string [ ANSITerminal.green ]

let terminal_yellow_print =
  ANSITerminal.print_string [ ANSITerminal.yellow ]

let terminal_white_print = print_endline

let terminal_print_horizontal_line () =
  print_endline "\n-----------------------------------------------\n"

let cyan_print s = Gui.update_console s cyan

let magenta_print s = Gui.update_console s magenta

let red_print s = Gui.update_console s red

let green_print s = Gui.update_console s green

let yellow_print s = Gui.update_console s yellow

let white_print s = Gui.update_console s white

let print_horizontal_line () =
  white_print "-----------------------------------------------"
