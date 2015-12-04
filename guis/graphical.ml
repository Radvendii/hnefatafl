(*
   NAME: 2D
*)
open Helpers
open Game_types
open GUI

module GUI : GUI = struct
  module Graphics' = struct
    include Graphics
    let initialized = ref(false)
    let key_was_down = ref None
    let button_was_down = ref None
    let key_pressed () =
      let r = !key_was_down in
      key_was_down := None; r
    let button_pressed () =
      let r = !button_was_down in
      button_was_down := None; r
    let init () =
      initialized := true;
      ignore @@
      (* thread to deal with events/input *)
      Thread.create
        (loop_while (fun () ->
             try
               let stat = wait_next_event [Button_down; Key_pressed] in
               (if stat.button
                then
                  ( button_was_down := Some(stat.mouse_x, stat.mouse_y)
                  ; Cont(()))
                else
                  ( key_was_down := Some(stat.key)
                  ; Cont(())))
             with
             (* if the window is closed or gui is shutdown or whatnot *)
             | _ -> Break(())
           )) ()
    let deinit () =
      if !initialized
      then
        (close_graph ();
        initialized := false)
      else ()

  end
  open Graphics'
  let init () =
    (* open a _window_ *)
    (* not a "graph" *)
    open_graph "";
    set_window_title "Hnefatafl";
    (* manually sync back buffer and front buffer *)
    auto_synchronize false;
    init ()

  let deinit () = deinit ()

  (* points for the polygons to display pieces *)
  (* represent as if on a square from (0,0) to (1,1) *)
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

    (* colors *)
    let board_color = 0x808080
    let grid_color  = 0x909090
    let white_color = white
    let black_color = black

  (* [cs] is a list of points by fractions of a box
   * [x', y', w, h] describes a box
   * [shift_points cs x' y' w h] gives the points when shfited to that box *)
  let shift_points cs x' y' w h =
    let (x',y',w,h) =
      ( float_of_int x'
      , float_of_int y'
      , float_of_int w
      , float_of_int h) in
    List.map (fun (x,y) -> (int_of_float(x'+.w*.x), int_of_float(y'+.h*.y))) cs

  let draw_piece p x y w h =
    let pawn_poly = Array.of_list @@ shift_points pawn_poly x y w h in
    let king_poly' = Array.of_list @@ shift_points king_poly x y w h in
    match p with
    | WPawn ->
      set_color white_color;
      fill_poly pawn_poly;
      set_color black
    | BPawn ->
      set_color black_color;
      fill_poly pawn_poly;
      set_color black
    | WKing ->
      set_color white_color;
      fill_poly king_poly';
      set_color black

  type guistate = { selected : (piece * coord) option
                  ; board    : board}


  let board b =
    let (w,h) = b.dims in
    let size = max w h in
    let turn_indic_len = 20 in
    (* length of one side of the drawing area.
     * the margin is 1 / 20 of the smallest side
     * leave room at the bottom for drawing whose turn it is *)
    let draw_len () =
      min
        (size_x () * 19 / 20)
        ((size_y () - (turn_indic_len + 20)) * 19 / 20) in
    (* side length for board squares *)
    let side_len () = draw_len () / size in
    (* bottom corner of the drawing area *)
    let start_draw_x () = (size_x () - draw_len ()) /2 in
    let start_draw_y () = (size_y () - draw_len ()) /2 in
    (* calculate screen position for a board square *)
    let calc_x x = start_draw_x () + side_len () * x in
    let calc_y x = start_draw_y () + side_len () * x in
    (* calculate board square from a screen position *)
    let calc_inv_x x = (x - start_draw_x ()) / side_len () in
    let calc_inv_y y = (y - start_draw_y ()) / side_len () in

    let draw_board b' =
      (* draw board square *)
      set_color 0x808080;
      fill_rect (start_draw_x ()) (start_draw_y ()) (draw_len ()) (draw_len ());
      (* draw grid *)
      set_color 0x909090;
      List.iter (fun (i,j) ->
          draw_rect
            (calc_x i)
            (calc_y j)
            (side_len ())
            (side_len ())
        ) (prod (0 -- (w-1)) (0 -- (h-1)));
      set_color black;
      (* draw pieces *)
      List.iter (fun (p, (i,j)) -> draw_piece p (calc_x i) (calc_y j) (side_len ()) (side_len ())) b'.pieces;
      (* draw current turn *)
      let piece_of_player = function
        | White -> WPawn
        | Black -> BPawn in
      moveto (start_draw_x ()) (start_draw_y () - turn_indic_len);
      draw_string "Turn: ";
      draw_piece (piece_of_player b.turn) (current_x ()) (current_y () - 5) (turn_indic_len) (turn_indic_len) in

    (* handle click part of click-and-drag of pieces. Also quitting *)
    let handle_selection s =
      (* finish drawing the board *)
      synchronize ();
      match key_pressed () with
      | Some('q') | Some('\x1B') -> Break(Quit)
      | Some(_) | None ->
        (* if the user isn't trying to quit, process mouse clicks *)
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

    (* board b *)
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
        let (bw,bh) = (bw + 10, bh + 10) in
        (* position the box in the middle of the screen *)
        let bx = ((size_x () - bw) / 2) in

        (* from here on my, mh refer to the dimensions of the menu,
         * which we need but have not yet calculated *)

        (* position it slightly below the previous item *)
        let by my mh = (my + mh - (spacing + bh)*(i+1)) in
        (* string goes in the center of the box*)
        let sx = bx + (bw-sw)/2 in
        let sy my mh = by my mh + (bh-sh)/2 in
        let color_set () = set_color (if i = 0
                                      then title_color
                                      else
                                      if i = sel + 1
                                      then selected_color
                                      else item_color) in
        (* draw box under menu item *)
        let draw_box my mh =
          color_set ();
          (* fill_rect bx (by my mh) bw bh; *)
          fill_ellipse
            ((2 * bx + bw) / 2)
            ((2 * by my mh + bh) / 2)
            (bw /2)
            (bh /2);
          set_color black in
        (* draw whole menu item *)
        let draw_item my mh =
          draw_box my mh;
          moveto sx (sy my mh);
          draw_string s in
        (* return box dimensions and function to draw items *)
        ((bx,by,bw,bh), (fun my mh -> draw_item my mh))
      )(title::strs)
    |> List.split
    |> (fun (boxes, draws) ->
        (* dimensions of the menu *)
        let mw = spacing + List.fold_left
                   (* max width of all items *)
                   (fun acc (_,_,w,_) -> max acc w) 0 boxes in
        let mh = spacing + List.fold_left
                   (* total height of all items and spacing *)
                   (fun acc (_,_,_,h) -> acc + h + spacing) 0 boxes in
        let mx = (size_x () - mw)/2 in
        let my = (size_y () - mh)/2 in
        (* evaluate boxes *)
        let boxes' = List.map
            (fun (bx,by,bw,bh) -> (bx,(by my mh),bw,bh))
            boxes in
        (* evaluate funcions *)
        let draws' = List.map (fun f () -> f my mh) draws in
        (* actually draw everything *)
        set_color menu_color;
        fill_rect mx my mw mh;
        List.iter (fun f -> f ()) draws';
        synchronize ();
        (* return a function
         * that will tell you which menu item a coordinate is in *)
        let rec identify bs i (x,y) =
          match bs with
          | [] -> None
          | (bx,by,bw,bh)::bs' ->
            if i != 0 && (* ignore title *)
               bx < x &&
               by < y &&
               x < bx + bw &&
               y < by + bh
            then Some(i-1) (* title shift (-1) *)
            else identify bs' (i+1) (x,y) in
        identify boxes' 0)

  let menu title options default =
    loop_while (fun s ->
        match key_pressed () with
        | Some('q') | Some('\x1B') -> Break(default)
        | Some(_) | None ->
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
