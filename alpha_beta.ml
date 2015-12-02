open Game_types
open GUI
open Default
open Main

type win = int 2
type draw = int 1
type loss = int 0

let white_util (b:board) : int =
 match find_wking b with
  | None -> failwith "No King. This isnt a fucking democracy. You lose"
  | Some (x,y) -> if (x = (fst init_board.dims)/2) &&
                     (y = (snd init_board.dims)/2) then 0
                  else
                      let x_out = abs (x-((fst init_board.dims)/2)) in
                      let y_out = abs (y-((fst init_board.dims)/2)) in
                    if x_out > y_out then x_out
                  else
                    if x_out < y_out then y_out
                  else
                    if (Random.bool ()) then x_out else y_out

let black_util (b:board) : int =
  (*white_pieces_caputred - black_pieces_captured*)
  (fst captured) - (snd captured)

let utility (b:board) =
  ((white_util b)/10) - black_util b

let simple_move (b:board): action =
  let best_for_piece (b:board) (p:piece): action =
    let treated_list =
      match b.turn with
        |White ->
          List.map (fun act -> (act, (utility (board_gen act)))
            (valid_moves p d))
        |Black ->
          List.map (fun act -> (act, ((-1)*utility (board_gen act)))
            (valid_moves p d)) in
      let compare_moves (new_b) (acc) =
        (if (fst new_b > fst acc) then (new_b) else (acc)) in
          List.fold_left compare_moves (0, Nop) treated_list in
  let treated_piece_list = List.map best_for_piece b.pieces in
    List.fold_left compare_moves (0,Nop) treated_piece_list

