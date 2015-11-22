open Helpers
open Game_types
open Graphical.GraphicsGUI
(* open Ascii.AsciiGUI *)
open Menu
open Default

let () =
  (* initialize graphics library *)
  init () ;
  (* initial menu (configuration) for now, disregard configuration *)
  loop_while (fun () ->
      let _ = initmenu () in
      loop_while (fun b ->
          match player_won b with
          | Some(p) -> display_win p; Break(())
          | None ->
            (* prompt user for input *)
            match board b with
            | Move(c1, c2) ->
              if not @@ valid_move c1 c2 b then Cont(b)
              else Cont
                  { b with
                    turn = next_turn b.turn ;
                    pieces =
                      let ps, p1 = pop_find (fun (_,c) -> c = c1) b.pieces in
                      let ps', _ = pop_find (fun (_,c) -> c = c2) ps in
                      match p1 with
                      | None -> failwith "checked for in valid_move"
                      | Some(p1') ->
                        (* move the piece!*)
                        let nps = (fst p1', c2)::ps' in
                        (* remove captured pieces *)
                        let rps = piece_taken c2 {b with pieces = nps} in
                        List.filter (fun x -> not @@ List.mem (snd x) rps) nps
                  }
            | Quit -> Break(())
            | Nop -> Cont(b)
        ) init_board ;
      Cont(())
    ) ()
    (* deinitialize graphics library *)
  deinit ()
