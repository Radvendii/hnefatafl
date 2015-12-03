open Helpers
open Game_types
open Game_mode

(*DEFAULT GAME_MODE with:
 *default setup with 11x11 board
 *attackers move first
 *pawns can move any number of spaces
 *king can move at most 3 spaces
 *king cannot take part in captures either actively or passively
 *cannot capture against a wall
 *capture king by surrounding on all 4 sides
 *black wins if captured king
 *white wins if king got to any edge of the board
 *pawns can occupy any location
*)

module Mode : Game_mode = struct
  let init_board () =
    { dims = (9,9)
    ; pieces =
        List.flatten @@
        List.map (List.map (fun c -> BPawn, c))
          [ prod (3--5) [0;8]
          ; prod [0;8] (3--5)
          ; prod [1;7] [4]
          ; prod [4] [1;7]
          ]

        @
        List.map (List.map (fun c -> WPawn, c))
          [ List.filter ((<>)(5,5)) (prod (2--6) [4])
          ; List.filter ((<>)(5,5)) (prod [4] (2--6))
          ]

        @
        [[WKing, (4,4)]]
    ; turn = Black
    ; captured = (0,0)}

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

  (*return the coordinates of the WKing on the board
   *Returns None if the king is no longer in play
  *)
  let rec find_wking (b:board) : coord option =
    match b.pieces with
    |[] -> None
    |(WKing, (x,y))::ps -> Some (x,y)
    |p::ps -> find_wking {b with pieces = ps}

  (*Naive implementation of winning.
   *Black wins if the White King was captured
   *White wins if the White King gets to any edge square of the board
  *)
  let player_won (b:board) : player option =
    match find_wking b with
    |None -> Some Black
    |Some (x,y) -> if x = 0 || y = 0 || x = ((fst b.dims)-1) || y = ((fst b.dims)-1)
      then Some White
      else None

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
end
