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
    let module Mod = (val modval () : GUI) in
    Mod.init ()
  let deinit () =
    let module Mod = (val modval () : GUI) in
    Mod.deinit ()
  let board b =
    let module Mod = (val modval () : GUI) in
    Mod.board b
  let menu t os d =
    let module Mod = (val modval () : GUI) in
    Mod.menu t os d
  let display_win p =
    let module Mod = (val modval () : GUI) in
    Mod.display_win p
end

let default_gui = "3D", (module ThreeD.GUI : GUI)

let gui_list =
  [ "ascii", (module Ascii.GUI : GUI)
  ; "CLI", (module CLI.GUI : GUI)
  ; "2D", (module Graphical.GUI : GUI)
  ; "3D", (module ThreeD.GUI : GUI)
  ]