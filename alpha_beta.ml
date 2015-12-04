open GUI
open Main
open Game_types
open MODE_list

type win = int 2
type draw = int 1
type loss = int 0

let rec find_wking (b:board) : coord option =
  match b.pieces with
  |[] -> None
  |(WKing, (x,y))::ps -> Some (x,y)
  |p::ps -> find_wking {b with pieces = ps}

let white_util (b:board) : int =
 match (find_wking b) with
  | None -> failwith "No King. This isnt a fucking democracy. You lose"
  | Some (x,y) -> if (x = (fst b.dims)/2) &&
                     (y = (snd b.dims)/2) then 0
                  else
                      let x_out = abs (x-((fst b.dims)/2)) in
                      let y_out = abs (y-((fst b.dims)/2)) in
                    if x_out > y_out then x_out
                  else
                    if x_out < y_out then y_out
                  else
                    if (Random.bool ()) then x_out else y_out

let black_util (b:board) : int =
  (*white_pieces_caputred - black_pieces_captured*)
  (fst b.captured) - (snd b.captured)

let utility (b:board) =
  ((white_util b)/10) - black_util b

let simple_move (b:board): action =
  let compare_moves (new_pair:(action*int)) (acc:(action*int)) =
        if ((fst new_pair) > (fst acc)) then (new_pair) else (acc) in
  let best_for_piece (p:(Game_types.piece)*(coord)): action*int =
    let action_space =
      List.map (fun valid_crd -> Move((snd p),valid_crd))
      (MODE_list.Mode.valid_moves (snd p) b) in
    let treated_list =
      (match b.turn with
        |White ->
        List.map (fun act -> (act,(utility (board_gen b act)))) (action_space)
        |Black ->
        List.map (fun act -> (act,((-1)*utility (board_gen b act))))
            (action_space)) in
    List.fold_left compare_moves (Nop,0) treated_list in
  let treated_piece_list = List.map best_for_piece b.pieces in
    fst (List.fold_left compare_moves (Nop,0) treated_piece_list)

