open GUI
open Game_types
open MODE_list

let board_gen b a =
  match board_gen b a with
  | None -> failwith "AI should not be trying to Quit"
  | Some(b) -> b


let rec find_wking (b:board) : coord option =
  match b.pieces with
  |[] -> None
  |(WKing, (x,y))::ps -> Some (x,y)
  |p::ps -> find_wking {b with pieces = ps}

let white_util (b:board) : int =
 match (find_wking b) with
  | None -> failwith "No King. This isnt a fucking democracy. White Looses"
  | Some (x,y) ->
                  if (x = (fst b.dims)/2) &&
                     (y = (snd b.dims)/2) then 0
                  else
                      let x_out = abs (x-((fst b.dims)/2)) in
                      let y_out = abs (y-((fst b.dims)/2)) in
                    if x_out > y_out then x_out
                  else
                    if x_out < y_out then y_out
                  else
                    if (Random.bool ()) then x_out else y_out


(*Black Utility Extras*)
let m_distance c1 c2 =
    ((abs(fst c1 - fst c2)) + (abs(snd c1 - snd c2)))

let net_king_distance (b:board) : int =
  let king_cords =
    match find_wking b with
      | None -> failwith "Derp"
      | Some (x,y) -> (x,y) in
    match (m_distance king_cords) b.pieces with
      | [] -> 0
      | hd :: tl -> hd +

let black_util (b:board) : int =
  (*white_pieces_caputred - black_pieces_captured*)
  (fst b.captured) - (snd b.captured)

let utility (b:board) =
  ((white_util b)/10) - black_util b

let rec random_prune (i:float) (l) =
  Random.self_init ();
  match l with
  | [] -> []
  | hd :: tl -> if ((Random.float (1.0) < (i))) then hd :: (random_prune i tl)
                                           else (random_prune i tl)

let board (b:board): action =
  Random.self_init ();
  let compare_moves (new_pair:(action*int)) (acc:(action*int)) =
        if ((fst new_pair) > (fst acc)) then (new_pair) else (acc) in
  let best_for_piece (p:(Game_types.piece)*(coord)): action*int =
    let action_space =
      List.map (fun valid_crd -> Move((snd p),valid_crd))
      (MODE_list.Mode.valid_moves (snd p) b) in
    let treated_list =
      (match b.turn with
        |White ->
        List.map (fun act -> (act,(utility (MODE_list.board_gen b act))))
        (action_space)
        |Black ->
        List.map (fun act -> (act,((-1)*utility (MODE_list.board_gen b act))))
            (action_space)) in
    List.fold_left compare_moves (Nop,0) treated_list in
  let treated_piece_list = List.map best_for_piece ((random_prune (0.9) (b.pieces))) in
    fst (List.fold_left compare_moves (Nop,0) treated_piece_list)

let random_move (b:board): action =
  Random.self_init ();
  let arbitrary_piece = List.nth (b.pieces) (Random.int (List.length b.pieces - 1)) in
  let action_space =
    List.map (fun valid_crd -> Move((snd arbitrary_piece),valid_crd))
        (MODE_list.Mode.valid_moves (snd arbitrary_piece) b) in
  List.nth action_space (Random.int (List.length action_space - 1))
