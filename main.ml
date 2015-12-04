open Helpers
open Game_types
open GUI
open GUI_list.GUI
open GUI_list
open MODE_list.Mode
open MODE_list
open Menu

let () =
  (* initialize graphics library *)
  init () ;
  loop_while (fun () ->
      (* initial menu (configuration) *)
      match initmenu () with
      | None -> Break(())
      | Some({mode;white;black}) ->
        let real_ai_of_player = function
          | White -> white
          | Black -> black in
        set_mode mode;
        loop_while (fun b ->
            match player_won b with
            | Some(p) -> display_win p; Break(())
            | None ->
              (* prompt user for input *)
              match
                (
                  (* generate the new board from the returned action*)
                  board_gen b
                    (
                      (* if this turn belongs to a player *)
                    if (real_ai_of_player b.turn = Real)
                    (* then prompt the player *)
                    then board b
                    (* otherwise give it to the AI *)
                    else
                      (draw_board b;
                       Alpha_beta.board b)
                    )
                )
              with
              | None -> Break(())
              | Some(b) -> Cont(b)
          ) (init_board ()) ;
        Cont(())
    ) ();
  (* deinitialize graphics library *)
  deinit ()
