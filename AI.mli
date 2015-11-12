open Game_types

(* [next_move p b] gives the AI's next move (starting coordinate,
 * ending coordinate), for player p given board configuration b *)
val next_move : player -> board -> (coord * coord)
