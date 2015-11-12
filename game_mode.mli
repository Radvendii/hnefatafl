open Game_types

module type Game_mode = sig
  (* [init_board] is the initial board configuration for this game mode*)
  val init_board  : board

  (* [valid_moves c b] is the list of coordinates the piece
   * on board [b] at coordinate [c] can go in one turn *)
  val valid_moves : coord -> board -> coord list

  (* [piece_taken c b] is the list of coordinates for pieces that
   * have been taken when the piece at coordinate [c] has just been
   * moved there *)
  val piece_taken : coord -> board -> coord list

  (* [player_won b] is the player that has won when the board is in
   * position [b] or None if no player has won in that configuration*)
  val player_won  : board -> player option
end
