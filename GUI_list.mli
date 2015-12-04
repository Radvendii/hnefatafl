open GUI
module type GUI_list = sig
  type t
  (* A list of possible guis *)
  val gui_list : t list

  (* the gui that GUI starts set to,
   * also a fallback gui in case gui_list is empty
   * i.e. a GUI_list must have at least one gui *)
  val default_gui : t

  (* return the currently loaded gui *)
  val get_gui : unit -> t

  (* return the name of the gui for displaying purposes *)
  val string_of_gui : t -> string

  (* returns the actual module behind a gui *)
  val module_of_gui : t -> (module Game_mode)

  (* sets the module GUI to run the gui passed in *)
  val set_gui : t -> unit

  (* this is how the loaded gui's functions are accessed.
   * just use this module as that gui. *)
  module GUI : GUI
end
