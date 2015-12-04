open Game_mode
open Helpers
open Game_types

(*COPENHAGEN GAME MODE
 *fetlar game mode + shieldwall
 *Shieldwall captures are done when a team brackets
 *in the other team with a row across the front and
 *two on the ends.  King can take part in capture.
 *still can use corner restricted squares
 *need 2+ talfmen in the opposing row
 *King can be in the group attacked
 *)

module Mode : Game_mode = struct
  let init_board () =
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

  let rec shieldwall_helper_black dir dir1 cOrig c1 c2 b :(bool * (coord list)) =
    let a = piece_at (step_dir dir cOrig) b in
    let c = piece_at (step_dir dir1 cOrig) b in
    if a = Some BPawn && (c = Some WPawn || c = Some WKing)
      then let d = am_i_in_shieldwall dir (step_dir dir cOrig) b in
            (fst d, cOrig::(snd d))
    else if a = Some WPawn && (c = Some WPawn || c = Some WKing)
      then (true, [cOrig])
    else if a = None && (c = Some WPawn || c = Some WKing)
      then (step_dir dir cOrig = c1 || step_dir dir cOrig = c2, [cOrig])
    else (false, [])

  and shieldwall_helper_white dir dir1 cOrig c1 c2 b =
    let a = piece_at (step_dir dir cOrig) b in
    let c = piece_at (step_dir dir1 cOrig) b in
    if c = Some BPawn && (a = Some WPawn || a = Some WKing)
    then let d = am_i_in_shieldwall dir (step_dir dir cOrig) b in
    (fst d, cOrig::(snd d))
    else if c = Some BPawn && a = Some BPawn
    then (true, [cOrig])
    else if a = None && c = Some BPawn
    then (step_dir dir cOrig = c1 || step_dir dir cOrig = c2, [cOrig])
    else (false, [])

  and am_i_in_shieldwall (dir:direction) ((x,y):coord) (b:board) : (bool * (coord list)) =
    match ((piece_at (x,y) b), x, y) with
    |(Some BPawn,0,_) ->
      shieldwall_helper_black dir Right (x,y) (0, snd b.dims-1) (0,0) b
    |(Some BPawn, j,_) when j = fst b.dims -1 ->
      shieldwall_helper_black dir Left (x,y) (fst b.dims-1, 0) (fst b.dims-1,snd b.dims-1) b
    |(Some BPawn,_,0) ->
      shieldwall_helper_black dir Up (x,y) (fst b.dims-1, 0) (0,0) b
    |(Some BPawn,_,j) when j = snd b.dims -1 ->
      shieldwall_helper_black dir Down (x,y) (fst b.dims-1, snd b.dims-1) (0,snd b.dims-1) b
    |(Some WPawn,0,_) |(Some WKing,0,_) ->
      shieldwall_helper_white dir Right (x,y) (0, snd b.dims-1) (0,0) b
    |(Some WPawn,j,_) |(Some WKing,j,_) when j = fst b.dims -1->
      shieldwall_helper_white dir Left (x,y) (fst b.dims-1, snd b.dims-1) (fst b.dims-1,0) b
    |(Some WPawn,_,0) |(Some WKing,_,0) ->
      shieldwall_helper_white dir Up (x,y) (fst b.dims-1,0) (0,0) b
    |(Some WPawn,_,j) |(Some WKing,_,j) when j = snd b.dims -1 ->
      shieldwall_helper_white dir Down (x,y) (fst b.dims-1, snd b.dims-1) (0,snd b.dims-1) b
    |_ -> (false,[])

  let check_shieldwall (dir:direction) ((x,y):coord) (b:board) : coord list =
    match (dir, x, y) with
    |(Up, 0, _)->
      let s = am_i_in_shieldwall dir (0, y+1) b in
        if fst s then snd s else []
    |(Up, j, _) when j = fst b.dims -1->
      let s = am_i_in_shieldwall dir (fst b.dims-1, y+1) b in
        if fst s then snd s else []
    |(Up, _, j) when j = snd b.dims -2 ->
      let s = am_i_in_shieldwall Left (x, y+1) b in
          let t = am_i_in_shieldwall Right (x,y+1) b in
          if fst s && fst t then snd s @ snd t else []
    |(Down, 0, _) ->
      let s = am_i_in_shieldwall dir (0, y-1) b in
        if fst s then snd s else []
    |(Down, j, _) when j = fst b.dims -1->
      let s = am_i_in_shieldwall dir (fst b.dims-1, y-1) b in
        if fst s then snd s else []
    |(Down, _, 1) ->
      let s = am_i_in_shieldwall Left (x, 0) b in
      let t = am_i_in_shieldwall Right (x,0) b in
        if fst s && fst t then snd s @ snd t else []
    |(Left, 1, _)->
      let s = am_i_in_shieldwall Up (0, y) b in
      let t = am_i_in_shieldwall Down (0,y) b in
        if fst s && fst t then snd s @ snd t else []
    |(Left, _, 0) ->
      let s = am_i_in_shieldwall dir (x-1, y) b in
        if fst s then snd s else []
    |(Left, _, j) when j = snd b.dims-1 ->
      let s = am_i_in_shieldwall dir (x-1, y) b in
        if fst s then snd s else []
    |(Right, j, _) when j = snd b.dims -2 ->
      let s = am_i_in_shieldwall Up (snd b.dims-1, y) b in
      let t = am_i_in_shieldwall Down (snd b.dims-1,y) b in
        if fst s && fst t then snd s @ snd t else []
    |(Right, _, 0) ->
      let s = am_i_in_shieldwall dir (x+1, y) b in
        if fst s then snd s else []
    |(Right, _, j) when j = snd b.dims-1 ->
      let s = am_i_in_shieldwall dir (x+1, y) b in
        if fst s then snd s else []
    |_ -> []

  let check_capture_wpawn (dir:direction) (c:coord) (b:board) : coord list =
    match piece_at (step_dir dir c) b with
    |None -> []
    |Some WPawn -> []
    |Some WKing -> []
    |Some BPawn -> begin
      let c' = step_two_dir dir c in
      match piece_at c' b with
      |Some WPawn -> [step_dir dir c]
      |Some BPawn -> check_shieldwall dir c b
      |Some WKing -> []
      |None -> if c' = (0,0) ||
                c' = (0,snd b.dims -1) ||
                c' = (fst b.dims -1, 0) ||
                c' = (fst b.dims -1, snd b.dims -1) ||
                c' = (fst b.dims/2, snd b.dims/2)
                then [step_dir dir c]
                else check_shieldwall dir c b
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
      |Some BPawn -> [step_dir dir c]
      |Some WPawn -> check_shieldwall dir c b
      |Some WKing -> check_shieldwall dir c b
      |None -> if c' = (0,0) ||
                c' = (0,snd b.dims -1) ||
                c' = (fst b.dims -1, 0) ||
                c' = (fst b.dims -1, snd b.dims -1) ||
                c' = (fst b.dims/2, snd b.dims/2)
                then [step_dir dir c]
                else check_shieldwall dir c b
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
      |_ -> check_shieldwall dir c b
      end

  let piece_taken ((x,y):coord) (b:board) : coord list =
    match piece_at (x,y) b with
    |None -> failwith "No piece here"
    |Some BPawn ->
      List.flatten @@ List.map (fun d -> check_capture_bpawn d (x,y) b)
        [Up; Down; Left; Right]
    |Some WPawn |Some WKing ->
      List.flatten @@ List.map (fun d -> check_capture_wpawn d (x,y) b)
        [Up; Down; Left; Right]

  let valid_moves c1 b =
    let rec helper c2 dir =
      let c2' = step_dir dir c2 in
      if in_range b.dims c2' && piece_at c2' b = None
      then c2'::helper c2' dir
      else [] in
    (match piece_at c1 b with
     |None -> (fun _ -> [])
     |Some WKing -> (fun x -> x)
     |_ -> List.filter (fun c -> c <> (0,0) &&
                        c <> (0,snd b.dims -1) &&
                        c <> (fst b.dims -1, 0) &&
                        c <> (fst b.dims -1, snd b.dims -1) &&
                        c <> (fst b.dims/2, snd b.dims/2)))
    @@
    List.flatten @@ List.map (helper c1) [Up; Down; Left; Right;]

  let player_won (b:board) : player option =
    match find_wking b with
    |None -> Some Black
    |Some c -> if c = (0,0) ||
                c = ((fst b.dims)-1,0) ||
                c = ((fst b.dims)-1,(snd b.dims)-1) ||
                c = (0,(snd b.dims)-1)
                then Some White
                else None
end
