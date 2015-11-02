open Curses
let w  = initscr ()
let _  = move 5 5
let _  = addstr "Hello, World!"
let _ = refresh ()
let () = Unix.sleep 2
let () = endwin ()
