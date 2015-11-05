open Game_types

module type GUI = sig
  val init : unit -> unit
  val deinit : unit -> unit
  val draw_board : board -> unit
  val user_input : unit -> action
end
