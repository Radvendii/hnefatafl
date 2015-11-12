module type Game_mode = sig
  val init_board  : board
  val valid_moves : coord -> board -> coord list
  val piece_taken : coord -> board -> coord list
  val player_won  : board -> player option
end
