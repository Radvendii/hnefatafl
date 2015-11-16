open Helpers
open Game_types
open GUI
open Graphics

module GraphicsGUI : GUI = struct
  let init () =
    open_graph "";
    auto_synchronize false;
    set_window_title "Hnefatafl"

  let deinit () = close_graph ()

  let pawn_points =
    [ (0.1,0.1)
    ; (0.9,0.1)
    ; (0.9,0.9)
    ; (0.1,0.9)
    ]

  let king_points =
    [ (0.5,0.1)
    ; (0.1,0.5)
    ; (0.5,0.9)
    ; (0.9,0.5)
    ]

  let shift_points cs x' y' w h =
    List.map (fun (x,y) -> (int_of_float(x'+.w*.x), int_of_float(y'+.h*.y))) cs

  let draw_piece p x y w h =
    let (x,y,w,h) = (float_of_int x, float_of_int y, float_of_int w, float_of_int h) in
    let pawn_points' = Array.of_list @@ shift_points pawn_points x y w h in
    let king_points' = Array.of_list @@ shift_points king_points x y w h in
    match p with
    | WPawn -> draw_poly pawn_points'
    | BPawn -> fill_poly pawn_points'
    | WKing -> draw_poly king_points'

  let draw_board b =
    clear_graph ();
    (* draw grid *)
    let (w,h) = b.dims in
    let draw_len = min (size_x () * 19 / 20) (size_y () * 19 / 20) in
    let size = max w h in
    let side_len = draw_len / size in
    let calc_x x = (size_x () - draw_len)/2 + side_len * x in
    let calc_y x = (size_y () - draw_len)/2 + side_len * x in
    List.iter (fun (i,j) ->
        draw_rect
          (calc_x i)
          (calc_y j)
          side_len
          side_len
      ) (prod (0 -- (w-1)) (0 -- (h-1)));
    (* draw pieces *)
    List.iter (fun (p, (i,j)) -> draw_piece p (calc_x i) (calc_y j) side_len side_len) b.pieces;
    synchronize ()

  let user_input () =
    loop_while (fun () -> Cont(())) ()

  let menu t os d =
    match os with
    | [] -> d
    | o::_ -> snd o
end
