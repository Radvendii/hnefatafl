open Helpers
open Game_types
open GUI

module Dummy : GUI = struct
  let fail () = failwith("Must be dynamically replaced")
  let init _        = fail ()
  let deinit _      = fail ()
  let board _       = fail ()
  let menu _ _ _    = fail ()
  let display_win _ = fail ()
end

let modref = ref (module Dummy : GUI)

let modval () = !modref

let change modval = modref := modval

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

(* let required_modules = ref [] *)
let test = ref 0
let testequals i = test := i


(* module Termbox = Termbox *)
(* module Graphics = Graphics *)
module Thread = Thread

let load_GUI s =
  (* try *)
    Dynlink.loadfile ("_build/" ^ s)
  (* with *)
  (* | _ -> *)
  (*   print_string "\n"; *)
  (*   print_string "\n"; *)
  (*   print_int !test; *)
  (*   print_string "\n"; *)
  (*   print_string "\n"; *)
  (*   failwith "here" *)
  (*   Dynlink.add_interfaces ["Termbox"; "Graphics"; "Thread"] *)
  (*     [ "/home/vagrant/.opam/4.02.3/lib/termbox/" *)
  (*     ; "/home/vagrant/.opam/4.02.3/lib/ocaml/" *)
  (*     ; "/home/vagrant/.opam/4.02.3/lib/ocaml/threads" *)
  (*     ]; *)
  (*   Dynlink.loadfile ("_build/" ^ s) *)
