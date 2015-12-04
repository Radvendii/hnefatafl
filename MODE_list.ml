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

  let valid_move c1 c2 b =
    let module Mod = (val modval (): Game_mode) in
    let open Mod in
    List.mem c2 (valid_moves c1 b) &&
    match piece_at c1 b with
    | None -> false
    | Some BPawn -> b.turn = Black
    | _ -> b.turn = White

(* calculate the next board state given an attempted move
 * integrating all of the Game_mode functions
 * of the currently loaded Game_mode module
 * returns None if the action is Quit*)
let board_gen (b:board) (a:action) : board option =
  let module Mod = (val modval (): Game_mode) in
  let open Mod in
  match a with
  | Move(c1, c2) ->
    (
      (* ignore invalid moves *)
      if not @@ valid_move c1 c2 b then Some(b)
      else
        let ps, p1 = pop_find (fun (_,c) -> c = c1) b.pieces in
        let ps', _ = pop_find (fun (_,c) -> c = c2) ps in
        match p1 with
        | None -> failwith "checked for in valid_move"
        | Some(p1') ->
          (* move the piece!*)
          let nps = (fst p1', c2)::ps' in
          (* captured pieces *)
          let rps = piece_taken c2 {b with pieces = nps} in
          (* remove captured pieces from (pieces with original piece moved) *)
          let pieces =
            List.filter (fun x -> not @@ List.mem (snd x) rps) nps in
          let n_taken = List.length rps in
          Some { b with
                 turn = other_player b.turn ;
                 pieces = pieces;
                 captured =
                   match player_of_piece(fst p1') with
                   | Black -> n_taken + fst b.captured, snd b.captured
                   | White -> fst b.captured, n_taken + snd b.captured
               }
    )
  | Nop -> Some(b)
  | Quit -> None

let mode_list =
  [ "copenhagen", (module Copenhagen.Mode : Game_mode)
  ; "CS3110", (module Default.Mode : Game_mode)
  ; "fetlar", (module Fetlar.Mode : Game_mode)
  ]
