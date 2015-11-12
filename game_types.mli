type coord  = int * int
type player = | White | Black
type piece  = | BPawn | WPawn | WKing

type board  = { dims   : int * int
              ; pieces : (piece * coord) list
              }

val piece_at : coord -> board -> piece option

type action =
  | Quit
  | Move of (coord * coord)
  | Nop
