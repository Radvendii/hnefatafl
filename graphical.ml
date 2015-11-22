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

  let draw_len () = min (size_x () * 19 / 20) (size_y () * 19 / 20)
  let start_draw_x () = (size_x () - draw_len ()) /2
  let start_draw_y () = (size_y () - draw_len ()) /2

  let board b =
    let (w,h) = b.dims in
    let size = max w h in
    let side_len () = draw_len () / size in
    let calc_x x = start_draw_x () + side_len () * x in
    let calc_y x = start_draw_y () + side_len () * x in
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

    loop_while (fun s ->
        clear_graph ();
        draw_board s.board;
        match s.selected with
        | None ->
          (synchronize ();
           if key_pressed () then
             match read_key () with
             | 'q' | '\x1B' -> Break(Quit)
             | _ -> Cont(s)
           else
             match button_pressed () with
             | None -> Cont(s)
             | Some(x,y) ->
               (match pop_find (fun (_,c) -> c = (calc_inv_x x, calc_inv_y y) ) b.pieces with
                | (_, None)      -> Cont(s)
                | (ps,Some(p,c)) -> Cont({ selected = Some(p,c)
                                         ; board = {b with pieces = ps}})))
        | Some(p,c) ->
          let (x,y) = mouse_pos () in
          (if button_down ()
           then
             ((draw_piece p x y (side_len ()) (side_len ()));
              synchronize ();
              Cont(s))
           else
             Break(Move(c, (calc_inv_x x, calc_inv_y y))))
      ) {selected = None; board=b}

  let draw_menu title strs (sel : int) =
    clear_graph ();
    let spacing = 10 in
    let menu_rect =
      [ (0.1,0.1)
      ; (0.9,0.1)
      ; (0.9,0.9)
      ; (0.1,0.9)
      ] in
    let menu_rect' = shift_points menu_rect (start_draw_x ()) (start_draw_y ()) (draw_len ()) (draw_len ()) in
    (* draw box around/behind the menu *)
    set_color 0xCCCCCC;
    fill_poly (Array.of_list menu_rect');
    (* draw the menu items (including title) *)
    let str_bound = (* bound on the size of the box for the strings *)
      List.fold_left (fun acc s ->
          let acc' = text_size s in
          (max (fst acc) (fst acc'), max (snd acc) (snd acc'))) (0,0)
        strs in
    List.mapi (fun i s ->
        let (sw,sh) = text_size s in
        (* title gets a perfectly fit box, all menu items get the same size box *)
        let (bw,bh) = if i = 0 then (sw,sh) else str_bound in
        let (bx,by) = ((fst (List.nth menu_rect' 0) + fst (List.nth menu_rect' 1)- bw) / 2), (snd (List.nth menu_rect' 2) - (spacing + (snd str_bound))*(i+1)) in
        let (sx,sy) = bx + (bw-sw)/2, by + (bh-sh)/2 in
        set_color (if i = 0
                   then 0xDDDDDD (* title has lighter background *)
                   else
                   if i = sel + 1
                   then 0x2222DD (* selected item has blue *)
                   else 0xAAAAAA (* everything else has grey *)
                  );
        (* draw box under menu item *)
        fill_rect bx by bw bh;
        (* draw menu item *)
        moveto sx sy;
        set_color black;
        draw_string s;
        (bx,by,bw,bh)
      ) (title::strs)
    |> (fun boxes ->
        synchronize ();
        let rec identify bs i (x,y) =
          match bs with
          | [] -> None
          | (bx,by,bw,bh)::bs' ->
            if bx < x &&
               by < y &&
               x < bx + bw &&
               y < by + bh &&
               i != 0 (* ignore title *)
            then Some(i-1) (* title shift *)
            else identify bs' (i+1) (x,y) in
        identify boxes 0)

  let menu title options default =
    (* let i_of_xy = draw_menu title (List.map fst options) (-1) in *)
    (* loop_while (fun s -> *)
    (*    let stat = wait_next_event [Button_down] in *)
    (*    match i_of_xy (stat.mouse_x, stat.mouse_y) with *)
    (*    | None -> Cont(-1) *)
    (*    | Some(n) -> Break(snd (List.nth options n)) *)
    (* ) (-1) *)
    loop_while (fun s ->
        if button_pressed () <> None && s >= 0
        then
          Break(snd (List.nth options s))
        else
          (
            let i_of_xy = draw_menu title (List.map fst options) s in
            match i_of_xy (mouse_pos ()) with
            | None -> Cont(-1)
            | Some(i) -> Cont(i)
          )
      ) 0

  let display_win p = ()
end
