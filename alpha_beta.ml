open GUI
open Game_types

let board_gen b a =
  match MODE_list.board_gen b a with
  | None -> failwith "AI should not be trying to Quit"
  | Some(b) -> b

let rec find_wking (b:board) : coord option =
  match b.pieces with
  |[] -> None
  |(WKing, (x,y))::ps -> Some (x,y)
  |p::ps -> find_wking {b with pieces = ps}

let m_distance c1 c2 =
    ((abs(fst c1 - fst c2)) + (abs(snd c1 - snd c2)))

let white_util (b:board) : int =
 match (find_wking b) with
  | None -> (-24)
  | Some (x,y) ->
    let closest x acc c =
      let dist = m_distance x c in
      if dist < acc then dist else acc in
    let default_out = List.fold_left (closest ((fst b.dims)/2, (snd b.dims)/2)) (max_int) (MODE_list.Mode.win_squares ()) in
    let closest_win = List.fold_left (closest (x,y)) default_out (MODE_list.Mode.win_squares ())
     in
    if closest_win = default_out then 0
    else default_out - closest_win

(*Black Utility Extras*)

(*make black pieces huddle towards the king*)
let avg_king_distance (b:board) : int =
    match find_wking b with
    | None -> 0
    | Some king_cords -> begin
  let blacks = List.filter (fun (p,c) -> p = BPawn) b.pieces in
  let rec helper l =
    match l with
    | [] -> 0
    | hd :: tl -> m_distance king_cords (snd hd) + helper tl in
  (helper blacks) / List.length(blacks)
end

let black_util (b:board) : int =
  (*white_pieces_caputred - black_pieces_captured*)
  (fst b.captured) - (snd b.captured) + (avg_king_distance b)

let utility (b:board) =
  ((white_util b)/10) - black_util b

let rec filter_by_turn b =
  match b.turn with
  |White -> List.filter (fun (p,c) -> p = WKing || p = WPawn) b.pieces
  |Black -> List.filter (fun (p,c) -> p = BPawn) b.pieces

let board (b:board): action =
  Random.self_init ();
  let compare_moves (new_pair:(action*int)) (acc:(action*int)) =
        if ((snd new_pair) > (snd acc)) then (new_pair) else
        if ((snd new_pair) = (snd acc)) then
          if (Random.bool ()) then new_pair else acc
        else (acc) in
  let best_for_piece (p:(Game_types.piece)*(coord)): action*int =
    let action_space =
      List.map (fun valid_crd -> Move((snd p),valid_crd))
      (MODE_list.Mode.valid_moves (snd p) b) in
    let treated_list =
      (match b.turn with
      |White->List.map (fun act -> (act,(utility (board_gen b act)))) (action_space)
      |Black->List.map (fun act -> (act,((-1)*utility (board_gen b act)))) (action_space))
    in List.fold_left compare_moves (Nop,-24) treated_list in
  let treated_piece_list = List.map best_for_piece (filter_by_turn b) in
    fst (List.fold_left compare_moves (Nop,-24) treated_piece_list)

let random_move (b:board): action =
  Random.self_init ();
  let arbitrary_piece = List.nth (b.pieces) (Random.int (List.length b.pieces - 1)) in
  let action_space =
    List.map (fun valid_crd -> Move((snd arbitrary_piece),valid_crd))
        (MODE_list.Mode.valid_moves (snd arbitrary_piece) b) in
  List.nth action_space (Random.int (List.length action_space - 1))
