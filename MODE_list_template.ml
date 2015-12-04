(* NOTE: Do NOT edit MODE_list.ml
 * to make changes that last, edit MODE_list_template.ml
 * and regenerate MODE_list.ml with generate_module_lists.sh *)
open Game_mode
open Game_types
open Helpers

type t = string * (module Game_mode)

let default_mode = "CS3110", (module Default.Mode : Game_mode)

let modref = ref(default_mode)

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

(* calculate the next board state given an attempted move
 * integrating all of the Game_mode functions
 * of the currently loaded Game_mode module
 * returns None if the action is Quit*)
let board_gen (b:board) (a:action) : board option =
  let module Mod = (val modval (): Game_mode) in
  let open Mod in
  match a with
            | Move(c1, c2) ->
                  { b with
                    turn = next_turn b.turn ;
                    pieces =
                      let ps, p1 = pop_find (fun (_,c) -> c = c1) b.pieces in
                      let ps', _ = pop_find (fun (_,c) -> c = c2) ps in
                      match p1 with
                      | None -> failwith "checked for in valid_move"
                      | Some(p1') ->
                        (* move the piece!*)
                        let nps = (fst p1', c2)::ps' in
                        (* remove captured pieces *)
                        let rps = piece_taken c2 {b with pieces = nps} in
                        List.filter (fun x -> not @@ List.mem (snd x) rps) nps
                  }
            | Nop -> b
            | Quit -> None
