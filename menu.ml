open Helpers
open Game_types
open Ascii.AsciiGUI

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

let menu_with_default (t : string) (os : (string * 'a) list) (d : 'a) : 'a = (* for some reason this doesn't work... *)
  match menu t os with
  | None    -> d
  | Some(x) -> x

let initmenu () : config option =
  loop_while (fun (m,c) ->
      match m with
      | `Foo -> Break(Some{white=Real;black=Real})
      | `Start ->
        (
          match menu "Hnefatafl"
          [ "Start Game",    Break(Some(c))
          ; "Configuration", Cont((`Config, c))
          ]
          with
          | None -> Break(None)
          | Some(x) -> x
        )
      | `Config ->
        let opt x = (string_of_player x ^ ": " ^ string_of_real_ai (real_ai_of_player_config x c)), Cont((`RealAI(x), c)) in
        (
          match menu "Configuration"
          [ opt White
          ; opt Black
          ; "Back", Cont((`Start, c))
          ]
          with
          | None -> Cont((`Start, c))
          | Some(x) -> x
        )
      | `RealAI(p) ->
        let opt x = string_of_real_ai x, Cont((`Config, config_change_player p c x)) in
        (
          match menu (string_of_player p ^ ": " ^ string_of_real_ai (real_ai_of_player_config p c))
          [ opt Real
          ; opt AI
          ; "Back", Cont((`Config, c))
          ]
        with
        | None -> Cont((`Config, c))
        | Some(x) -> x
        )
    )
    (`Start, {white=Real;black=Real})
