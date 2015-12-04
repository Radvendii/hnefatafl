open Game_mode

module Dummy : Game_mode = struct
  let fail () = failwith("Must be dynamically replaced")
  let init_board _    = fail ()
  let valid_moves _ _ = fail ()
  let piece_taken _ _ = fail ()
  let player_won _    = fail ()
end

let modref = ref(module Dummy : Game_mode)

let nameref = ref("Dummy")

let get_mode () = (!nameref, !modref)

let modval () = !modref

let set_mode (modname, modval) =
  nameref := modname;
  modref  := modval

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

let default_mode = "CS3110", (module Default.Mode : Game_mode)

let mode_list =
  [ "copenhagen", (module Copenhagen.Mode : Game_mode)
  ; "CS3110", (module Default.Mode : Game_mode)
  ; "fetlar", (module Fetlar.Mode : Game_mode)
  ]