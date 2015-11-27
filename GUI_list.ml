open GUI

module Dummy : GUI = struct
  let fail () = failwith("Must be dynamically replaced")
  let init _        = fail ()
  let deinit _      = fail ()
  let board _       = fail ()
  let menu _ _ _    = fail ()
  let display_win _ = fail ()
end

let modref = ref(module Dummy : GUI)

let nameref = ref("Dummy")

let get_gui () = (!nameref, !modref)

let modval () = !modref

let set_gui (modname, modval) =
  nameref := modname;
  modref  := modval

module GUI : GUI = struct
  let init () =
    print_endline("init: " ^ fst (get_gui ()));
    let module Mod = (val modval () : GUI) in
    Mod.init ()
  let deinit () =
    print_endline("deinit: " ^ fst (get_gui ()));
    let module Mod = (val modval () : GUI) in
    Mod.deinit ()
  let board b =
    print_endline("board: " ^ fst (get_gui ()));
    let module Mod = (val modval () : GUI) in
    Mod.board b
  let menu t os d =
    print_endline("menu: " ^ fst (get_gui ()));
    let module Mod = (val modval () : GUI) in
    Mod.menu t os d
  let display_win p =
    print_endline("display_win: " ^ fst (get_gui ()));
    let module Mod = (val modval () : GUI) in
    Mod.display_win p
end

let default_gui = "graphical", (module Graphical.GUI : GUI)

let gui_list =
  [ "ascii",     (module Ascii.GUI     : GUI)
  ; "graphical", (module Graphical.GUI : GUI)
  ]
