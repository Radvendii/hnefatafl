open Termbox

type coord = int * int
type piece = BPawn | WPawn | WKing
let char_of_piece = function
  | BPawn -> 'X'
  | WPawn -> 'O'
  | WKing -> '@'

type board = {
  dims   : int * int;
  pieces : (piece * coord) list
}

(* http://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function *)
let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

let prod l1 l2 =
  List.flatten @@ List.map (fun x -> List.map (fun y -> (x,y)) l2) l1

module type GUI = sig
  (* [runGUI b movef] runs the GUI with b as the initial board configuration
   *                  and movef as the callback. Where:
   * [movef p c1 c2]  returns the board that results from the user attempting to
   *                  move the piece [p] at position [c1] to position [c2].
   *                  This attempt needn't necessarily succeed. *)
  val runGUI : board -> (piece -> coord -> coord -> board) -> unit
end

module AsciiGUI : GUI = struct
  let draw_board b =
    (* draw the border *)
    let () = List.iter
        (fun x ->
           set_cell_char x 0 '-';
           set_cell_char x (fst b.dims + 1) '-')
        (0--(snd b.dims + 1)) in
    let () = List.iter
        (fun y ->
           set_cell_char 0 y '|';
           set_cell_char (snd b.dims + 1) y '|')
        (0--(fst b.dims + 1)) in
    (* draw the pieces *)
    let () = List.iter
        (fun (p,c) ->
           set_cell_char (snd c) (fst c) (char_of_piece p))
        b.pieces in
    ()

  let runGUI b movef =
    let _ = init () in
    let _ = draw_board b in
    let _  = present () in
    let _ = Unix.sleep 5 in
    let _ = shutdown () in
    ()
end

open AsciiGUI

(* TODO? read in inital positions and rules from textfile *)
let init =
  { dims = (11,11)
  ; pieces =
      List.flatten @@ (* TODO: this double-list is a bit of a kludge; fix it.*)
      List.map (List.map (fun c -> BPawn, c))
        [ (prod (4--8) [1;11])
        ; (prod [1;11] (4--8))
        ; (prod [2;10] [6])
        ; (prod [6] [2;10])]

      @
      List.map (List.map (fun c -> WPawn, c))
        [ (prod (5--7) (5--7))
        ; (prod [4;8] [6])
        ; (prod [6] [4;8])]

      @
      [[WKing, (6,6)]]
  }
let _ = runGUI init (fun p c1 c2 -> init) (* Do nothing function for now. *)
