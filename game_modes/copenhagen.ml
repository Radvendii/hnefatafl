open Game_mode
open Helpers
open Game_types

(*COPENHAGEN GAME MODE
 *fetlar game mode + encircling + shieldwall
 *Shieldwall captures are done when a team brackets
 *in the other team with a row across the front and
 *two on the ends.  King can take part in capture.
 *still can use corner restricted squares
 *need 2+ talfmen in the opposing row
 *King can be in the group attacked, but king is not taken
 *the black team wins if they surround the king and all remaining defenders
 *)


let init_board =
  { dims = (11,11)
  ; pieces =
      List.flatten @@
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
  ; turn = Black
  ; captured = (0,0)}

  let check_capture_wpawn (dir:direction) (c:coord) (b:board) : coord list =
  match piece_at (step_dir dir c) b with
  |None -> []
  |Some WPawn -> []
  |Some WKing -> []
  |Some BPawn -> begin
    let c' = step_two_dir dir c in
    match piece_at c' b with
    |None -> if c' = (0,0) ||
              c' = (0,snd b.dims -1) ||
              c' = (fst b.dims -1, 0) ||
              c' = (fst b.dims -1, snd b.dims -1) ||
              c' = (fst b.dims/2, snd b.dims/2)
              then [step_dir dir c]
              else []
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

(*Locations around the throne
          5
        2   1
      6   x   8
        3   4
          7
*)
let check_center b (x,y) =
  let center = (fst b.dims/2, snd b.dims/2) in
  if (x-1,y-1) = center then 1
  else if (x+1,y-1) = center then 2
  else if (x+1,y+1) = center then 3
  else if (x-1,y+1) = center then 4
  else if (x,y-2) = center then 5
  else if (x+2,y) = center then 6
  else if (x,y+2) = center then 7
  else if (x-2,y) = center then 8
  else 0

let check_capture_bpawn (dir:direction) (c:coord) (b:board) : coord list =
  match piece_at (step_dir dir c) b with
  |None -> []
  |Some BPawn -> []
  |Some WPawn -> begin
    let c' = step_two_dir dir c in
    match piece_at (c') b with
    |None -> if c' = (0,0) ||
              c' = (0,snd b.dims -1) ||
              c' = (fst b.dims -1, 0) ||
              c' = (fst b.dims -1, snd b.dims -1) ||
              c' = (fst b.dims/2, snd b.dims/2)
              then [step_dir dir c]
              else []
    |Some BPawn -> [step_dir dir c]
    |Some WPawn -> []
    |Some WKing -> []
    end
  |Some WKing -> begin
    let i = check_center b c in
    match (dir,make_cross dir c b) with
    |(_,(Some BPawn, Some BPawn, Some BPawn)) -> [step_dir dir c]
    |(Up,(Some BPawn, Some BPawn, None))      -> if i=3 then [step_dir dir c] else []
    |(Up,(Some BPawn, None, Some BPawn))      -> if i=4 then [step_dir dir c] else []
    |(Up,(None, Some BPawn, Some BPawn))      -> if i=7 then [step_dir dir c] else []
    |(Down,(Some BPawn, Some BPawn, None))    -> if i=2 then [step_dir dir c] else []
    |(Down,(Some BPawn, None, Some BPawn))    -> if i=1 then [step_dir dir c] else []
    |(Down,(None, Some BPawn, Some BPawn))    -> if i=5 then [step_dir dir c] else []
    |(Left,(Some BPawn, Some BPawn, None))    -> if i=1 then [step_dir dir c] else []
    |(Left,(Some BPawn, None, Some BPawn))    -> if i=4 then [step_dir dir c] else []
    |(Left,(None, Some BPawn, Some BPawn))    -> if i=8 then [step_dir dir c] else []
    |(Right,(Some BPawn, Some BPawn, None))   -> if i=2 then [step_dir dir c] else []
    |(Right,(Some BPawn, None, Some BPawn))   -> if i=3 then [step_dir dir c] else []
    |(Right,(None, Some BPawn, Some BPawn))   -> if i=6 then [step_dir dir c] else []
    |_ -> []
    end

let piece_taken ((x,y):coord) (b:board) : coord list =
  match piece_at (x,y) b with
  |None -> failwith "No piece here"
  |Some WKing -> List.flatten @@ List.map (fun d -> check_capture_wpawn d (x,y) b)
      [Up; Down; Left; Right]
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

let rec sort_into_indices a lst =
  match lst with
  |[] -> a
  |h::t -> Array.set a (fst (snd h)) (h::(Array.get a (fst (snd h))));
            sort_into_indices a t

(*let black_surrounded_all_white (b:board) (c:coord) :bool =
  let all_white = List.filter (fun x -> fst x = WPawn || fst x = WKing) in
  let all_black = List.filter (fun x -> fst x = BPawn) in
  let a = Array.make (fst b.dims) [] in
  let sorted_black = sort_into_indices a all_black in
  let sorted_white = sort_into_indices a all_white in
  let black_is_first = ref false in
  let black_surrounds = ref false in
  let black_is_last = ref false in
  for i = 0 to fst b.dims do

  done*)

let player_won (b:board) : player option =
  match find_wking b with
  |None -> Some Black
  |Some c -> if c = (0,0) ||
              c = ((fst b.dims)-1,0) ||
              c = ((fst b.dims)-1,(snd b.dims)-1) ||
              c = (0,(snd b.dims)-1)
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
   | Some WKing -> (fun x -> x)
   |_ -> List.filter (fun c -> c <> (0,0) &&
                      c <> (0,snd b.dims -1) &&
                      c <> (fst b.dims -1, 0) &&
                      c <> (fst b.dims -1, snd b.dims -1) &&
                      c <> (fst b.dims/2, snd b.dims/2)))
  @@
  List.flatten @@ List.map (helper c1) [Up; Down; Left; Right;]

let valid_move c1 c2 b =
  match piece_at c1 b with
  |None -> false
  |Some BPawn -> b.turn = Black && List.mem c2 (valid_moves c1 b)
  |_ -> b.turn = White && List.mem c2 (valid_moves c1 b)
