open Helpers
open Game_types
open GUI

module GraphicsGUI : GUI = struct
  module Graphics' = struct
    include Graphics
    let button_was_down = ref None
    let button_pressed () =
      let r = !button_was_down in
      button_was_down := None; r
    let _ =
      Thread.create
        (loop_while (fun () ->
             let stat = wait_next_event [Button_down] in
             button_was_down := Some(stat.mouse_x, stat.mouse_y);
             Cont(()))) ()
  end
  open Graphics'
  let init () =
    (* open a _window_ *)
    open_graph "";
    set_window_title "Hnefatafl";
    (* manually sync back buffer and front buffer *)
    auto_synchronize false

  let deinit () = close_graph ()

  (* points for the polygons to disply pieces *)
  (* represente as fractions of a square *)
  let pawn_poly =
    [ (0.1,0.1)
    ; (0.9,0.1)
    ; (0.9,0.9)
    ; (0.1,0.9)
    ]

  let king_poly =
    [ (0.5,0.1)
    ; (0.1,0.5)
    ; (0.5,0.9)
    ; (0.9,0.5)
    ]

  (* [cs] is a list of points by fractions of a box [x', y', w, h] describes a box
   * [shift_points cs x' y' w h] gives the points when shfited to that box *)
  let shift_points cs x' y' w h =
    let (x',y',w,h) = (float_of_int x', float_of_int y', float_of_int w, float_of_int h) in
    List.map (fun (x,y) -> (int_of_float(x'+.w*.x), int_of_float(y'+.h*.y))) cs

  let draw_piece p x y w h =
    let pawn_poly = Array.of_list @@ shift_points pawn_poly x y w h in
    let king_poly' = Array.of_list @@ shift_points king_poly x y w h in
    match p with
    | WPawn ->
      set_color white;
      fill_poly pawn_poly;
      set_color black;
      draw_poly pawn_poly
    | BPawn -> fill_poly pawn_poly
    | WKing ->
      set_color white;
      fill_poly king_poly';
      set_color black;
      draw_poly king_poly'

  type guistate = { selected : (piece * coord) option
                  ; board    : board}

  (* length of one side of the drawing area.
   * the margin is 1 / 20 of the smallest side *)
  let draw_len () = min (size_x () * 19 / 20) (size_y () * 19 / 20)
  (* bottom corner of the drawing area *)
  let start_draw_x () = (size_x () - draw_len ()) /2
  let start_draw_y () = (size_y () - draw_len ()) /2

  let board b =
    let (w,h) = b.dims in
    let size = max w h in
    (* side length for board squares *)
    let side_len () = draw_len () / size in
    (* calculate screen position for a board square *)
    let calc_x x = start_draw_x () + side_len () * x in
    let calc_y x = start_draw_y () + side_len () * x in
    (* calculate board square from a screen position *)
    let calc_inv_x x = (x - start_draw_x ()) / side_len () in
    let calc_inv_y y = (y - start_draw_y ()) / side_len () in

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

    (* handle click part of click-and-drag of pieces. Also quitting *)
    let handle_selection s =
      (* finish drawing the board *)
      synchronize ();
      if key_pressed () then
        match read_key () with
        | 'q' | '\x1B' -> Break(Quit)
        | _ -> Cont(s)
      else
        match button_pressed () with
        | None -> Cont(s)
        | Some(x,y) ->
          (match pop_find (fun (_,c) -> c = (calc_inv_x x, calc_inv_y y) ) b.pieces with
           (* someone selects an empty square or somewhere off the board *)
           | (_, None)      -> Cont(s)
           | (ps,Some(p,c)) -> Cont({ selected = Some(p,c)
                                    ; board = {b with pieces = ps}})) in

    (* handle drag part of click-and-drag of pieces *)
    let handle_piece_move s p c =
      let (x,y) = mouse_pos () in
      if button_down () (* still click-dragging? *)
      then
        ((draw_piece p x y (side_len ()) (side_len ()));
         (* finish drawing the board with the piece floating at the mouse cursor *)
         synchronize ();
         Cont(s))
      else
        Break(Move(c, (calc_inv_x x, calc_inv_y y))) in

    loop_while (fun s ->
        clear_graph ();
        draw_board s.board;
        match s.selected with
        | None -> handle_selection s
        | Some(p,c) -> handle_piece_move s p c
      ) {selected = None; board=b}

  let draw_menu title strs (sel : int) =
    clear_graph ();
    let spacing = 10 in
    let menu_color = 0xCCCCCC in
    let title_color = 0xDDDDDD in
    let selected_color = 0x2222DD in
    let item_color = 0xAAAAAA in
    let menu_rect =
      [ (0.1,0.1)
      ; (0.9,0.1)
      ; (0.9,0.9)
      ; (0.1,0.9)
      ] in
    let menu_rect' = shift_points menu_rect (start_draw_x ()) (start_draw_y ()) (draw_len ()) (draw_len ()) in

    (* draw box around/behind the menu *)
    (* TODO: make this box a tighter fit *)
    (* TRY: building up an IO () at the same time as the dimensions *)
    set_color menu_color;
    fill_poly (Array.of_list menu_rect');

    (* draw the menu items (including title) *)
    (* bound on the size of the box for the items *)
    let str_bound =
      List.fold_left (fun acc s ->
          let acc' = text_size s in
          (max (fst acc) (fst acc'), max (snd acc) (snd acc'))) (0,0)
        strs in
    List.mapi (fun i s ->
        let (sw,sh) = text_size s in
        (* title gets a perfectly fit box, all menu items get the same size box *)
        let (bw,bh) = if i = 0 then (sw,sh) else str_bound in
        (* position the box in the middle of the screen *)
        let bx = ((fst (List.nth menu_rect' 0) + fst (List.nth menu_rect' 1) - bw) / 2) in
        (* position it slightly below the previous item *)
        let by = (snd (List.nth menu_rect' 2) - (spacing + (snd str_bound))*(i+1)) in
        (* string goes in the center of the box*)
        let (sx,sy) = bx + (bw-sw)/2, by + (bh-sh)/2 in
        set_color (if i = 0
                   then title_color
                   else
                   if i = sel + 1
                   then selected_color
                   else item_color
                  );
        (* draw box under menu item *)
        fill_rect bx by bw bh;
        (* draw menu item *)
        moveto sx sy;
        set_color black;
        draw_string s;
        (* return box dimensions *)
        (bx,by,bw,bh)
      ) (title::strs)
      (* return a function that will tell you which menu item a coordinate is in *)
    |> (fun boxes ->
        synchronize ();
        let rec identify bs i (x,y) =
          match bs with
          | [] -> None
          | (bx,by,bw,bh)::bs' ->
            if i != 0 && (* ignore title *)
               bx < x &&
               by < y &&
               x < bx + bw &&
               y < by + bh
            then Some(i-1) (* title shift *)
            else identify bs' (i+1) (x,y) in
        identify boxes 0)

  let menu title options default =
    loop_while (fun s ->
        if button_pressed () <> None && s >= 0
        then
          Break(snd (List.nth options s))
        else
          let i_of_xy = draw_menu title (List.map fst options) s in
          match i_of_xy (mouse_pos ()) with
          | None -> Cont(-1)
          | Some(i) -> Cont(i)
      ) 0

  let display_win p = menu (string_of_player p ^ " wins!") ["play again", ()] ()
end
