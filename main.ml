open Helpers
open Game_types
open GUI
open GUI_list.GUI
open GUI_list
open MODE_list.Mode
open MODE_list
open Menu

let () = set_gui default_gui
let () = set_mode default_mode

  let valid_move c1 c2 b =
    match piece_at c1 b with
    |None -> false
    |Some BPawn -> b.turn = Black && List.mem c2 (valid_moves c1 b)
    |_ -> b.turn = White && List.mem c2 (valid_moves c1 b)


let board_gen (b:board) (a:action) : board =
  match a with
            | Move(c1, c2) ->
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
            | Nop -> b
            | _ -> failwith "Quitting is not an option computer, You shouldnt
                              be generating it"

let () =
  (* initialize graphics library *)
  init () ;
  loop_while (fun () ->
  (* initial menu (configuration) for now, disregard configuration *)
      match initmenu () with
      | None -> Break(())
      | Some({mode;_}) ->
        set_mode mode;
        loop_while (fun b ->
            match player_won b with
            | Some(p) -> display_win p; Break(())
            | None ->
              (* prompt user for input *)
              match board b with
              | Move(c1, c2) ->
                if not @@ valid_move c1 c2 b then Cont(b)
                else
                let (pieces, p, n_taken) =
                  let ps, p1 = pop_find (fun (_,c) -> c = c1) b.pieces in
                  let ps', _ = pop_find (fun (_,c) -> c = c2) ps in
                  match p1 with
                  | None -> failwith "checked for in valid_move"
                  | Some(p1') ->
                    (* move the piece!*)
                    let nps = (fst p1', c2)::ps' in
                    (* remove captured pieces *)
                    let rps = piece_taken c2 {b with pieces = nps} in
                    (List.filter (fun x -> not @@ List.mem (snd x) rps) nps,
                      player_of_piece(fst p1'),
                    List.length rps) in
                Cont
                    { b with
                      turn = next_turn b.turn ;
                      pieces = pieces;
                      captured = if p = Black then (n_taken + fst b.captured, snd b.captured)
                                  else (fst b.captured, n_taken + snd b.captured)
                    }
              | Quit -> Break(())
              | Nop -> Cont(b)
          ) (init_board ()) ;
        Cont(())
    ) ();
    (* deinitialize graphics library *)
    deinit ()
