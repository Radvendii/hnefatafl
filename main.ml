open Helpers
open Game_types
open Ascii.AsciiGUI

(* TODO? read in inital positions and rules from textfile *)
let init_board =
  { dims = (11,11)
  ; pieces =
      List.flatten @@ (* TODO: this double-list is a bit of a kludge; fix it.*)
      List.map (List.map (fun c -> BPawn, c))
        [ prod (3--7) [0;10]
        ; prod [0;10] (3--7)
        ; prod [1;9] [5]
        ; prod [5] [1;9]
        ]

      @
      List.map (List.map (fun c -> WPawn, c))
        [ List.filter ((<>)(5,5)) (prod (4--6) (4--6))
        ; prod [3;7] [5]
        ; prod [5] [3;7]
        ]

      @
      [[WKing, (5,5)]]
  }

let rec pop_find f = function
  | [] -> [], None
  | x::xs ->
    if f x
    then (xs, Some x)
    else let (xs', x') = pop_find f xs in (x::xs', x')

type direction = Up | Down | Left | Right
let step_dir dir (x,y) =
  match dir with
  | Up -> (x,y+1)
  | Down -> (x, y-1)
  | Left -> (x-1, y)
  | Right -> (x+1, y)

let step_two_dir dir (x,y) =
  match dir with
  | Up -> (x,y+2)
  | Down -> (x, y-2)
  | Left -> (x-2, y)
  | Right -> (x+2, y)

let check_capture_wpawn (dir:direction) (c:coord) (b:board) : coord list =
  match piece_at (step_dir dir c) b with
  |None -> []
  |Some WPawn -> []
  |Some WKing -> []
  |Some BPawn -> begin
    match piece_at (step_two_dir dir c) b with
    |None -> []
    |Some WPawn -> [step_dir dir c]
    |Some BPawn -> []
    |Some WKing -> []
  end

let make_cross dir (x,y) b =
  match dir with
  |Up -> (piece_at (x,y+2) b, piece_at (x-1,y+1) b, piece_at (x+1,y+1) b)
  |Down -> (piece_at (x,y-2) b, piece_at (x-1,y-1) b, piece_at (x+1,y-1) b)
  |Left -> (piece_at (x-2,y) b, piece_at (x-1,y+1) b, piece_at (x-1,y-1) b)
  |Right -> (piece_at (x+2,y) b, piece_at (x+1,y+1) b, piece_at (x+1,y-1) b)

let check_capture_bpawn (dir:direction) (c:coord) (b:board) : coord list =
  match piece_at (step_dir dir c) b with
  |None -> []
  |Some BPawn -> []
  |Some WPawn -> begin
    match piece_at (step_two_dir dir c) b with
    |None -> []
    |Some BPawn -> [step_dir dir c]
    |Some WPawn -> []
    |Some WKing -> []
    end
  |Some WKing -> begin
    match make_cross dir c b with
    |(Some BPawn, Some BPawn, Some BPawn) -> [step_dir dir c]
    |_ -> []
    end


(*Capturing of pieces,
 *the white king cannot participate in a capture both actively and passively
 *)
let piece_taken ((x,y):coord) (b:board) : coord list =
  match piece_at (x,y) b with
  |None -> failwith "No piece here"
  |Some WKing -> []
  |Some BPawn ->
    List.flatten @@ List.map (fun d -> check_capture_bpawn d (x,y) b)
      [Up; Down; Left; Right]
  |Some WPawn ->
    List.flatten @@ List.map (fun d -> check_capture_wpawn d (x,y) b)
      [Up; Down; Left; Right]


(* let closest_pieces (a,b) bd = *)
(*   let contact_pieces_in_yPos = List.filter (fun (_,(x,y)) -> x=a && y>b) bd.pieces in *)
(*   let contact_pieces_in_xPos = List.filter (fun (_,(x,y)) -> y=b && x>a) bd.pieces in *)
(*   let contact_pieces_in_yNeg = List.filter (fun (_,(x,y)) -> x=a && y<b) bd.pieces in *)
(*   let contact_pieces_in_xNeg = List.filter (fun (_,(x,y)) -> y=b && x<a) bd.pieces in *)
(*   let sorted_in_xPos = List.sort (fun (_,(x,_)) (_,(x',_)) -> x-x') contact_pieces_in_xPos in *)
(*   let sorted_in_yPos = List.sort (fun (_,(_,y)) (_,(_,y')) -> y-y') contact_pieces_in_yPos in *)
(*   let sorted_in_xNeg = List.sort (fun (_,(x,_)) (_,(x',_)) -> x'-x) contact_pieces_in_xNeg in *)
(*   let sorted_in_yNeg = List.sort (fun (_,(_,y)) (_,(_,y')) -> y'-y) contact_pieces_in_yNeg in *)
(*   [(if List.len sorted_in_xPos > 0 then Some (List.hd sorted_in_xPos) else None); *)
(*    (if List.len sorted_in_yPos > 0 then Some (List.hd sorted_in_yPos) else None); *)
(*    (if List.len sorted_in_xNeg > 0 then Some (List.hd sorted_in_xNeg) else None); *)
(*    (if List.len sorted_in_yNeg > 0 then Some (List.hd sorted_in_yNeg) else None);] *)

let valid_moves c1 b =
  let rec helper c2 dir =
    let c2' = step_dir dir c2 in
    if in_range b.dims c2' && piece_at c2' b = None
    then c2'::helper c2' dir
    else [] in
  (match piece_at c1 b with
   | None -> (fun _ -> [])
   | Some(WKing) -> List.filter (fun (x,y) -> (x - (fst c1) <= 3) && ((fst c1) - x <= 3) && (y - (snd c1) <= 3) && ((snd c1) - y <= 3))
   | _ -> (fun x -> x)) @@
  List.flatten @@ List.map (helper c1) [Up; Down; Left; Right;]

let valid_move c1 c2 b =
  List.mem c2 (valid_moves c1 b)

(*let valid_move_space (p:piece) ((a,b):coord) (bd:board) : coord list =
  let contact_pieces = closest_pieces (a,b) bd in
  let rec helper (p:piece) ((x,y):coord) (bd:board) (d:direction) =
    match (d,p) with
    |(Right,WKing)-> let issue_piece = List.nth contact_pieces 0 in
    |(Right,_)    -> let issue_piece = List.nth contact_pieces 0 in
    |(Up, WKing)  -> let issue_piece = List.nth contact_pieces 1 in
    |(Up,_)       -> let issue_piece = List.nth contact_pieces 1 in
    |(Left,WKing) -> let issue_piece = List.nth contact_pieces 2 in
    |(Left,_)     -> let issue_piece = List.nth contact_pieces 2 in
    |(Down,WKing) -> let issue_piece = List.nth contact_pieces 3 in
    |(Down,_)     -> let issue_piece = List.nth contact_pieces 3 in
*)

let () =
  init () ;
  let _ = menu "Test"
      [ "yes", ()
      ; "no",  ()
      ] in
  loop_while (fun b ->
      draw_board b ;
      match user_input () with
      | Move(c1, c2) ->
        (`Cont { b with
                 pieces =
                   let ps, p1 = pop_find (fun (_,c) -> c = c1) b.pieces in
                   let ps', _ = pop_find (fun (_,c) -> c = c2) ps in
                   match p1 with
                   | None -> ps (* if someone tries to move nothing, nothing happens*)
                   | Some(p1') ->
                     if valid_move c1 c2 b (* if it's a valid move*)
                     then
                       let nps = (fst p1', c2)::ps' in (* move the piece!*)
                       let rps = piece_taken c2 {b with pieces = nps} in
                       List.filter (fun x -> not @@ List.mem (snd x) rps) nps
                     else b.pieces (* otherwise do nothing *)
               })
      | Quit -> `Break(())
      | Nop -> `Cont(b)
    ) init_board ;
  deinit ()
