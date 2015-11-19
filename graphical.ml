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
    let (x',y',w,h) = (float_of_int x', float_of_int y', float_of_int w, float_of_int h) in
    List.map (fun (x,y) -> (int_of_float(x'+.w*.x), int_of_float(y'+.h*.y))) cs

  let draw_piece p x y w h =
    let pawn_points' = Array.of_list @@ shift_points pawn_points x y w h in
    let king_points' = Array.of_list @@ shift_points king_points x y w h in
    match p with
    | WPawn ->
      set_color white;
      fill_poly pawn_points';
      set_color black;
      draw_poly pawn_points'
    | BPawn -> fill_poly pawn_points'
    | WKing ->
      set_color white;
      fill_poly king_points';
      set_color black;
      draw_poly king_points'

  type guistate = {selected : (piece * coord) option; board : board}

  let board b =
    let (w,h) = b.dims in
    let draw_len () = min (size_x () * 19 / 20) (size_y () * 19 / 20) in
    let size = max w h in
    let side_len () = draw_len () / size in
    let calc_x x = (size_x () - draw_len ())/2 + side_len () * x in
    let calc_y x = (size_y () - draw_len ())/2 + side_len () * x in
    let calc_inv_x x = (x - (size_x () - draw_len ())/2) / side_len () in
    let calc_inv_y y = (y - (size_y () - draw_len ())/2) / side_len () in
    let draw_board b' =
      (* draw grid *)
      List.iter (fun (i,j) ->
          draw_rect
            (calc_x i)
            (calc_y j)
            (side_len ())
            (side_len ())
        ) (prod (0 -- (w-1)) (0 -- (h-1)));
      (* draw pieces *)
      List.iter (fun (p, (i,j)) -> draw_piece p (calc_x i) (calc_y j) (side_len ()) (side_len ())) b'.pieces in

    loop_while (fun s ->
        clear_graph ();
        draw_board s.board;
        match s.selected with
        | None ->
          synchronize ();
          (let stat = wait_next_event [Button_down; Key_pressed] in
           if stat.keypressed
           then
             match stat.key with
             | 'q' | '\x1B' -> Break(Quit)
             | _ -> Cont(s)
           else
           match pop_find (fun (_,c) -> c = (calc_inv_x stat.mouse_x, calc_inv_y stat.mouse_y) ) b.pieces with
           | (_, None)      -> Cont(s)
           | (ps,Some(p,c)) -> Cont({ selected = Some(p,c)
                                    ; board = {b with pieces = ps}}))
        | Some (p,c) ->
          let (x,y) = mouse_pos () in
          (if button_down ()
           then
             ((draw_piece p x y (side_len ()) (side_len ()));
              synchronize ();
              Cont(s))
           else
             Break(Move(c, (calc_inv_x x, calc_inv_y y))))
      ) {selected = None; board=b}

  let menu t os d =
    match os with
    | [] -> d
    | o::_ -> snd o

  let display_win p = ()
end
