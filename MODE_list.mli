open Game_mode
open Game_types

type t
(* A list of possible game modes *)
val mode_list : t list

(* the game mode that Mode starts set to,
 * also a fallback mode in case mode_list is empty
 * i.e. a MODE_list must have at least one game mode *)
val default_mode : t

(* return the currently loaded game mode *)
val get_mode : unit -> t

(* return the name of the mode for displaying purposes *)
val string_of_mode : t -> string

(* returns the actual module behind a game mode *)
val module_of_mode : t -> (module Game_mode)

(* sets the module Mode to run the game mode passed in *)
val set_mode : t -> unit

(* this is how the loaded gui's functions are accessed.
 * just use this module as that gui. *)
module Mode : Game_mode

(* [valid_move c1 c2 b] is whether the piece at [c1] can move
 * to [c2] on board [b] *)
val valid_move : coord -> coord -> board -> bool

(* calculate the next board state given an attempted action
 * integrating all of the functions
 * of the currently loaded game mode
 * evaluates to None if the action is Quit *)
val board_gen : board -> action -> board option
