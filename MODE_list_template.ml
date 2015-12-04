(* NOTE: Do NOT edit MODE_list.ml
 * to make changes that last, edit MODE_list_template.ml
 * and regenerate MODE_list.ml with generate_module_lists.sh *)
open Game_mode

let default_mode = "CS3110", (module Default.Mode : Game_mode)

let modref = ref(fst default_mode, module (snd default_mode) : GUI)

let get_mode () = !modref

let modval () = snd (get_mode ())

let string_of_mode = fst

let module_of_mode = snd

let set_mode mode =
  modref := mode

(* this module just refers to the dynamically loaded module
 * it is necessary to setup the module within every function
 * because it must be set up at runtime *)
module Mode : Game_mode = struct
  let init_board () =
    let module Mod = (val modval () : Game_mode) in
    Mod.init_board ()
  let valid_moves c b =
    let module Mod = (val modval () : Game_mode) in
    Mod.valid_moves c b
  let piece_taken c b =
    let module Mod = (val modval () : Game_mode) in
    Mod.piece_taken c b
  let player_won b =
    let module Mod = (val modval () : Game_mode) in
    Mod.player_won b
end

