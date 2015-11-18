open Helpers
open Game_types

(*FETLAR GAME_MODE with:
 *default setups of 11x11 board
 *attackers move first
 *restricted squares = throne, 4 corners
 *no pawn can occupy a restricted square
 *corner squares are hostile to all pieces
 *throne always hostile to attackers
 *throne hostile to defenders when empty
 *all pieces can move any number of spaces
 *king may take part in captures
 *King must move to any of the four corners
 *attackers win if they capture king before escape
 *king capture on all four sides or 3 sides and throne
 *king cannot be captured on the edge
 *if player cannot move, he loses the game
 *)

