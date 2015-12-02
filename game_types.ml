type coord  = int * int
type player = | White | Black
let string_of_player = function
  | White -> "white"
  | Black -> "black"
type piece  = | BPawn | WPawn | WKing
let player_of_piece = function
  | BPawn -> Black
  | WPawn | WKing -> White

type board  = { dims   : int * int
              ; pieces : (piece * coord) list
              ; turn   : player
              ; captured : int*int (*white gone,black gone*)
              }

type action =
  | Quit
  | Move of (coord * coord)
  | Nop

let rec piece_at c b =
  match b.pieces with
  | [] -> None
  | (p,c')::ps ->
    if c' = c
    then Some(p)
    else piece_at c {b with pieces = ps}

let next_turn = function
  | White -> Black
  | Black -> White
