open Helpers
open Game_types
open GUI
open Game_mode
open GUI_list.GUI
open GUI_list
open MODE_list.Mode
open MODE_list

type real_ai = | Real | AI
let string_of_real_ai = function
  | Real -> "Player"
  | AI   -> "AI"

(* data type for preferences for the game *)
type config = { white : real_ai
              ; black : real_ai
              ; mode : string * (module Game_mode)
              }

let real_ai_of_player_config p c =
  match p with
  | White -> c.white
  | Black -> c.black
let config_change_player p c n =
  match p with
  | White -> {c with white = n}
  | Black -> {c with black = n}
let config_flip_player p c =
  match realai_of_player_config p c with
  | Real -> config_change_player p c AI
  | AI   -> config_change_player p c Real

(* first menu presented to user *)
let initmenu () : config option =
  (* m is which sub-menu we are in
   * c is the current configuration state *)
  loop_while (fun (m,c) ->
      match m with
      | `Start ->
        menu "Hnefatafl"
          [ "Start Game",    Break(Some(c))
          ; "Configuration", Cont(`Config, c)
          ; "Quit", Break(None)
          ]
          (Break(None))
      | `Config ->
        (* clicking on an item flips the value Real <-> AI*)
        let player_opt x =
          (string_of_player p ^ ": " ^
           string_of_real_ai (real_ai_of_player_config p c)),
          Cont(`Config, config_flip_player p c) in
        menu "Configuration"
          [ player_opt White
          ; player_opt Black
          ; ("GUI: " ^ fst (get_gui ())), Cont(`GUI, c)
          ; ("Game Mode: " ^ fst c.mode), Cont(`Mode, c)
          ; "Back", Cont(`Start, c)
          ]
          (Cont(`Start, c))
      | `GUI ->
        (* menu to select gui *)
        let gui =
          menu ("GUI: " ^ fst (get_gui ()))
            (List.map (fun g -> (string_of_gui g, g)) gui_list)
            (get_gui ()) in
        (* switches to that gui *)
        deinit ();
        set_gui gui;
        init ();
        Cont(`Config, c)
      | `Mode ->
        Cont(`Config,
             {c with
              mode =
                menu ("Game Mode: " ^ fst c.mode)
                  (List.map (fun m -> (string_of_mode m, m)) mode_list)
                  (get_mode ())
             })
    )
    (`Start, {white=Real;black=Real;mode=(get_mode ())})
