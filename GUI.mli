open Game_types

module type GUI = sig
  (* initializes the GUI libary for this GUI. *)
  val init       : unit -> unit

  (* deinitializes the GUI library for this GUI. *)
  val deinit     : unit -> unit

  (* [draw_board b] clears what is currently on
   * the screen and draws the given board. *)
  val draw_board : board -> unit

  (* [user_input ()] prompts the user for input (in whatever way is
   * appropriate for this GUI) and returns an action to perform. *)
  val user_input : unit -> action

  (* [menu title ms def] presents the string part of ms as menu options
   * to a menu named [title] and prompts the user to select an option.
   * It then returns the 'a part of the menu option that is selected,
   * or def if the user exits the menu *)
  val menu       : string -> (string * 'a) list -> 'a -> 'a

  val display_win : player -> unit
end
