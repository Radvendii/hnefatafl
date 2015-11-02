open Curses

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

module AsciiGUI = struct

  let putch i1 i2 c =
    let (x,y) = getyx (stdscr ()) in
    let _ = mvaddch i1 i2 (int_of_char c) in
    let _ = move x y in
    ()

  let draw_board b =
    (* draw the border *)
    let () = List.iter
        (fun y ->
           putch y 0 '|';
           putch y (snd b.dims + 1) '|')
        (0--(fst b.dims + 1)) in
    let () = List.iter
        (fun x ->
           putch 0 x '-';
           putch (fst b.dims + 1) x '-')
        (0--(snd b.dims + 1)) in
    (* draw the pieces *)
    let () = List.iter
        (fun (p,c) ->
           putch (snd c) (fst c) (char_of_piece p))
        b.pieces in
    ()

  let runGUI init movef =
    let _ = initscr () in
    let _ = draw_board init in
    let _  = refresh () in
    let _ = Unix.sleep 5 in
    let _ = endwin () in
    ()
end

open AsciiGUI

let init =
  { dims = (11,11)
  ; pieces =
      List.flatten @@
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
let _ = runGUI init (fun x -> x)
