type coord  = int * int
type player = | White | Black
type piece  = | BPawn | WPawn | WKing

type board  = { dims   : int * int
              ; pieces : (piece * coord) list
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
