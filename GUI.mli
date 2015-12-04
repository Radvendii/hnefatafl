open Game_types

module type GUI = sig
  (* initializes the GUI libary for this GUI. *)
  val init : unit -> unit

  (* deinitializes the GUI library for this GUI. *)
  val deinit : unit -> unit

  (* [board b] presents the board to the user, and prompts the user
   * for input. It then returns an action to perform based on that
   * input *)
  val board : board -> action

  
  (* [draw_board b] presents the board to the ser. It does not ask for 
   * any input though *)
  val draw_board : board -> unit

  (* [menu title ms def] presents the string part of ms as menu options
   * to a menu named [title] and prompts the user to select an option.
   * It then returns the 'a part of the menu option that is selected,
   * or def if the user exits the menu *)
  val menu : string -> (string * 'a) list -> 'a -> 'a

  (* [display_win p] displays to the user that player [p] won.
   * for implementing this function [string_of_player] may be useful *)
  val display_win : player -> unit
end
