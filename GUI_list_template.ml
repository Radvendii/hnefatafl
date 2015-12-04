(* NOTE: Do NOT edit GUI_list.ml
 * to make changes that last, edit GUI_list_template.ml
 * and regenerate GUI_list.ml with generate_module_lists.sh *)
open GUI

type t = string * (module GUI)

let default_gui = "3D", (module ThreeD.GUI : GUI)

let modref = ref(default_gui)

let get_gui () = !modref

let string_of_gui = fst

let module_of_gui = snd

let modval () = snd (get_gui ())

let set_gui gui =
  modref := gui

(* this module just refers to the dynamically loaded module
 * it is necessary to setup the module within every function
 * because it must be set up at runtime *)
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
