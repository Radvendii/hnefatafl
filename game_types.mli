(* coordinates on the board *)
type coord  = int * int

type player = | White | Black

val string_of_player : player -> string

(* these are the only pieces found in hnefatafl *)
type piece  = | BPawn | WPawn | WKing

(* the dimensions of the board, and what pieces are on it *)
type board  = { dims   : int * int
              ; pieces : (piece * coord) list
              ; turn   : player
              }

(* [piece_at c b] returns the piece at coordinate [c] on board [b]
 * or None if there is no piece there. *)
val piece_at : coord -> board -> piece option

val next_turn : board -> player

(* the actions that a user can perform when interacting with the game. *)
type action =
  | Quit
  | Move of (coord * coord)
  | Nop
