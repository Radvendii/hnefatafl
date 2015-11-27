open Helpers
open Game_types
open GUI
open GUI_list.GUI
open GUI_list

type real_ai = | Real | AI
let string_of_real_ai = function
  | Real -> "Player"
  | AI   -> "AI"
type config = { white : real_ai
              ; black : real_ai
              }
let real_ai_of_player_config p c =
  match p with
  | White -> c.white
  | Black -> c.black
let config_change_player p c n =
  match p with
  | White -> {c with white = n}
  | Black -> {c with black = n}

let initmenu () : config option =
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
        let opt x = (string_of_player x ^ ": " ^ string_of_real_ai (real_ai_of_player_config x c)), Cont(`RealAI(x), c) in
        menu "Configuration"
          [ opt White
          ; opt Black
          ; ("GUI: " ^ fst (get_gui ())), Cont(`GUI, c)
          ; "Back", Cont(`Start, c)
          ]
          (Cont(`Start, c))
      | `RealAI(p) ->
        let opt x = string_of_real_ai x, Cont(`Config, config_change_player p c x) in
        menu (string_of_player p ^ ": " ^ string_of_real_ai (real_ai_of_player_config p c))
          [ opt Real
          ; opt AI
          ; "Back", Cont(`Config, c)
          ]
          (Cont(`Config, c))
      | `GUI ->
        let gui =
          menu ("GUI: " ^ fst (get_gui ()))
            (List.map (fun (a,b) -> (a,(a,b))) gui_list)
            (get_gui ()) in
        deinit ();
        set_gui gui;
        init ();
        Cont(`Config, c)
    )
    (`Start, {white=Real;black=Real})
