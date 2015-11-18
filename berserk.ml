open Helpers
open Game_types

(*BERSERK GAME_MODE with:
 *default setups of 11x11 board
 *attackers move first
 *restricted squares = throne, 4 corners
 *pawns cannot occupy restricted squares
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

(*ISSUE - need to be able to have 4 commanders on the Black and
 *1 knight on the White teams
 *)
(*Knight can jump over a single piece to capture it. then turn is done unless
 *following holds
 *player can continue to move the same piece
 *as long as he can make a capture
 *king needs to get to corner square
 *king captured on all 4 sides
 *if king not on or next to throne, 2 commanders can flank and
 *capture the king
 *1 commander can flank and capture the king agains one of the 4 corners
 *Player that cannot move = lose
 *)