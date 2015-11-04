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
let piece_at c b =
  fst @@ List.find (fun (_,c') -> c' = c) b.pieces

(* http://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function *)
let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

let prod l1 l2 =
  List.flatten @@ List.map (fun x -> List.map (fun y -> (x,y)) l2) l1

module type GUI = sig
  (* [runGUI b movef] runs the GUI with b as the initial board configuration
   *                  and movef as the callback.
   * Where:
   * [movef c1 c2]    returns the board that results from the user attempting to
   *                  move the piece at position [c1] to position [c2].
   *                  This attempt needn't necessarily succeed. *)
  val runGUI : board -> (board -> coord -> coord -> board) -> unit
end

module AsciiGUI : GUI = struct
  type action =
    | MoveC  of coord
    | Select of coord 
    | Move   of (coord * coord)
    | Quit
    | Nop

  type state = { cursor   : coord
               ; selected : coord option
               ; board    : board (* TODO: I am not satisfied with needing to
                                   * keep the board as state in the graphics
                                   * part of the program; let's try to factor
                                   * it out. *)
               }

  let action_of_event e s =
    let (cx,cy) = s.cursor in
    match e with
    | Utf8 _ | Resize _ -> Nop
    | Key k ->
      (match k with
       | Arrow_left  -> MoveC(cx-1, cy)
       | Arrow_down  -> MoveC(cx,   cy+1)
       | Arrow_up    -> MoveC(cx,   cy-1)
       | Arrow_right -> MoveC(cx+1, cy)
       | _ -> Nop)
    | Ascii c ->
      (match c with
       | 'h' -> MoveC(cx-1, cy)
       | 'j' -> MoveC(cx,   cy+1)
       | 'k' -> MoveC(cx,   cy-1)
       | 'l' -> MoveC(cx+1, cy)
       | 'q' | '\x1B' -> Quit
       | ' ' ->
         (match s.selected with
          | None -> Select(cx,cy)
          | Some(sx, sy) -> Move((sx,sy),(cx,cy)))
       | _   -> Nop)

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
           set_cell_char (fst c) (snd c) (char_of_piece p))
        b.pieces in
    ()

  let rec loop_while f s =
    match f s with
    | s', true -> loop_while f s'
    | _, false -> ()

  let in_range (xd,yd) (x,y) =
    0 < x   &&
    0 < y   &&
    x <= xd &&
    y <= yd

  let runGUI b movef =
    let _ = init () in
    draw_board b ;
    present () ;
    loop_while (fun s ->
        match action_of_event (poll_event ()) s with
        | MoveC(x, y) ->
          if in_range s.board.dims (x,y)
          then (set_cursor x y
               ; present ()
               ; {s with cursor = (x,y)}, true)
          else s, true
        | Select(x, y) ->
          set_cell_char ~bg:Blue x y (char_of_piece @@ piece_at (x,y) s.board) (* TODO: dekludge *)
        ; {s with selected = Some (x,y)}, true
        | Move(c1,c2) ->
          let board = movef s.board c1 c2 in
          clear ()
        ; draw_board @@ board
        ; present ()
        ; {s with selected = None; board}, true
        | Nop -> s, true
        | Quit -> s, false)
      {board = b; cursor = (1,1); selected = None} ;
    shutdown () ;
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
        ; (prod [6] [2;10])
        ]

      @
      List.map (List.map (fun c -> WPawn, c))
        [ (prod (5--7) (5--7))
        ; (prod [4;8] [6])
        ; (prod [6] [4;8])
        ]

      @
      [[WKing, (6,6)]]
  }

let rec pop_find f = function
  | [] -> [], None
  | x::xs ->
    if f x
    then (xs, Some x)
    else let (xs', x') = pop_find f xs in (x::xs', x')

let () = runGUI init (fun b c1 c2 ->
    { b with
      pieces =
        let ps, p1 = pop_find (fun (_,c) -> c = c1) b.pieces in
        let ps', _ = pop_find (fun (_,c) -> c = c2) ps in
        match p1 with
        | None -> ps (* if someone tries to move nothing, nothing happens*)
        | Some(p1') -> (fst p1', c2)::ps' (* move the piece!*)
    })
