open Helpers
open Game_types
open GUI

module GUI : GUI = struct
  module Sdl' = struct
    let foi = float_of_int
    let cursor_pos = ref (0.0,0.0)
    let window = ref None
    let key_was_down = ref None
    let button_was_down = ref None
    let button_is_down = ref false
    let mouse_was_pos = ref (0,0)
    let key_pressed () =
      let r = !key_was_down in
      key_was_down := None; r
    let button_pressed () =
      let r = !button_was_down in
      button_was_down := None; r
    let mouse_pos () = !mouse_was_pos
    let mouse_down () = !button_is_down
    let deinit () =
      Sdl.quit ()
    let normal_vertex3 (n,v) =
      GlDraw.normal3 n;
      GlDraw.vertex3 v
    let init w h t () =
      Sdl.init [`VIDEO];
      ignore (Sdlvideo.set_video_mode w h [`DOUBLEBUF; `OPENGL]);
          ignore @@
          Thread.create
            (loop_while (fun () ->
                 let open Sdlevent in
                 match wait_event () with
                 | QUIT -> deinit (); Break(())
                 | KEYDOWN {keysym;_} -> key_was_down := Some(keysym)
                                       ; Cont(())
                 | MOUSEMOTION {mme_xrel; mme_yrel; _}
                   -> mouse_was_pos := (mme_xrel, mme_yrel)
                    ; Cont(())
                 | MOUSEBUTTONDOWN {mbe_x; mbe_y; _} ->
                   button_was_down := Some(mbe_x, mbe_y);
                   button_is_down := true;
                   Cont(())
                 | MOUSEBUTTONUP(_)->
                   button_is_down := false;
                   Cont(())
                 | _ -> Cont(())
                 )) ()

    let clear_3D () =
      GlClear.clear[`color; `depth];
      Gl.enable `depth_test;
      GlMat.mode `modelview;
      GlMat.load_identity ();
      Gl.enable `lighting;
      Gl.enable `light0;
      Gl.enable `light1;
      Gl.enable `light2;
      GlLight.light ~num:0 (`diffuse (0.2,0.2,0.2,1.0));
      GlLight.light ~num:0 (`position(3.0, -3.0, -6.0, 1.0));
      GlLight.light ~num:1 (`diffuse (0.2,0.2,0.2,1.0));
      GlLight.light ~num:1 (`position(-3.0, 3.0, -6.0, 1.0));
      (* spotlight on selected piece *)
      GlLight.light ~num:2 (`diffuse (0.0,0.0,1.0,1.0));
      GlLight.light ~num:2 (`position(
          fst !cursor_pos,
          snd !cursor_pos,
          -2.0,
          1.0));
      GlLight.light ~num:2 (`spot_cutoff 3.0);
      GlLight.light ~num:2 (`spot_direction (0.0,0.0,1.0));
      Gl.enable `color_material;
      GlLight.color_material `both `ambient_and_diffuse;
      GlMat.mode `projection;
      GlMat.load_identity ();
      GluMat.perspective ~fovy:60.0 ~aspect:1.33 ~z:(0.5,100.0);
      GluMat.look_at ~eye:(0., 1.5, -1.5) ~center:(0., 0., 0.) ~up:(0., -1., 0.);
      (* GluMat.look_at ~eye:(0., 0.0, -1.5) ~center:(0., 0., 0.) ~up:(0., -1., 0.); *)
      ()
  end

  open Sdl'

  let pos_of_coord (xc,yc) (w,h) =
    (* it's just a linear function mapping
     * (0,w) -> (-1,1) and (0,h) -> (-1,1) *)
    -1.0 +. foi (xc * 2) /. foi w,
    -1.0 +. foi (yc * 2) /. foi h
  let coord_of_pos (xp,yp) (w,h) =
    (* linear function mapping
     * (-1,1) -> (0,w) and (0,h)
     * combined with a flooring *)
    int_of_float ((xp+.1.0) /. 2.0 *. foi w),
    int_of_float ((yp+.1.0) /. 2.0 *. foi h)

  type guistate = { selected : coord option
                  ; board    : board}

  let init () =
    init 800 600 "Hnefatafl" ()

  let deinit =
    deinit

  let white_color = (0.4, 0.4, 0.4)
  let black_color = (0.08, 0.08, 0.08)

  let draw_piece p =
    GlDraw.begins `triangles;
    (
      match p with
      | WPawn ->
        GlDraw.color white_color;
        List.iter normal_vertex3 Pawn.Data.vertex_list
      | BPawn ->
        GlDraw.color black_color;
        List.iter normal_vertex3 Pawn.Data.vertex_list
      | WKing ->
        GlDraw.color white_color;
        List.iter normal_vertex3 King.Data.vertex_list
    );
      GlDraw.ends ()

  let draw_board b =
    let (w,h) = b.dims in
    let size = max w h in
    (* draw game board *)
    GlMat.mode `modelview;
    GlMat.load_identity ();
    GlDraw.begins `quads;
    (* GlDraw.color (0.511, 0.328, 0.08); *)
    GlDraw.color (0.5, 0.5, 0.5);
    List.iter normal_vertex3 Game_board.Data.vertex_list;
    GlDraw.ends ();

    (* draw grid on board *)
    GlDraw.color (0.6, 0.6, 0.6);
    (* GlDraw.color (0.611, 0.418, 0.15); *)
    let board_height =
      -.
      (List.fold_left
        (fun acc (_,(_,_,z)) -> max acc (-.z))
        0.0
        Game_board.Data.vertex_list) in
    (* lift them up from the board so they don't disapear inside it *)
    let board_height = board_height -. 0.001 in
    let strip_width = 0.003 in
    let draw_v2s v2s =
      GlDraw.begins `quads;
      List.iter (fun (x,y) -> normal_vertex3 ((0.,0.,-1.0), (x,y,board_height))) v2s;
      GlDraw.ends () in

    List.iter (fun (i,j) ->
        let (x,y) = pos_of_coord (i,j) b.dims in
        let (nx,ny) = pos_of_coord (i+1,j+1) b.dims in
        draw_v2s
          (
            (if i = w then [] else
               [ x,  y-.strip_width
               ; x,  y+.strip_width
               ; nx, y+.strip_width
               ; nx, y-.strip_width
               ]) @
            (if j = h then [] else
               [ x-.strip_width, y
               ; x+.strip_width, y
               ; x+.strip_width, ny
               ; x-.strip_width, ny
               ])
          )
      ) (prod (0 -- w) (0 -- h));

    (* draw pieces *)
    List.iter (fun (p, (i,j)) ->
        GlMat.load_identity ();
        let scale = 2. /. foi size in
        let (x,y) = pos_of_coord (i, j) b.dims in
        GlMat.translate3 (x, y, 0.);
        GlMat.translate3 (scale /. 2., scale /. 2., 0.);
        GlMat.scale3 (scale, scale, scale);
        draw_piece p
      ) b.pieces;
    Gl.flush ();
    Sdlgl.swap_buffers ()

  type guiaction =
    | GUIMoveC  of (float * float)
    | GUISelect of coord
    | GUIMove   of (coord * coord)
    | GUIQuit
    | GUINop

  let guiaction_of_key k s =
    let (w,h) = s.board.dims in
    let (cx,cy) = !cursor_pos in
    (* unit shift in x and y directions
     * (pos_of_coord 2 - pos_of_coord 1) *)
    let (ux,uy) = 2.0 /. foi w, 2.0 /. foi h in
    let (cx',cy') = coord_of_pos (cx,cy) s.board.dims in
    match k with
    | Sdlkey.KEY_h
    | Sdlkey.KEY_LEFT  ->
      if cx' > 0
      then GUIMoveC(cx-.ux, cy)
      else GUINop
    | Sdlkey.KEY_j
    | Sdlkey.KEY_DOWN  ->
      if cy' < (h-1)
      then GUIMoveC(cx, cy+.uy)
      else GUINop
    | Sdlkey.KEY_k
    | Sdlkey.KEY_UP    ->
      if cy' > 0
      then GUIMoveC(cx, cy-.uy)
      else GUINop
    | Sdlkey.KEY_l
    | Sdlkey.KEY_RIGHT ->
      if cx' < (w-1)
      then GUIMoveC(cx+.ux, cy)
      else GUINop
    | Sdlkey.KEY_q
    | Sdlkey.KEY_ESCAPE -> GUIQuit
    | Sdlkey.KEY_SPACE ->
      (match s.selected with
       | None -> GUISelect(cx',cy')
       | Some(sx, sy) -> GUIMove((sx,sy),(cx',cy')))
    | _   -> GUINop

  let process_guiaction a s =
    match a with
    | GUIMoveC(cx, cy) ->
      cursor_pos := (cx, cy);
      Cont(s)
    | GUISelect(x,y) ->
      Cont({s with
            selected = Some(x,y)
           })
    | GUIMove(c1,c2) -> Break(Move(c1,c2))
    | GUIQuit -> Break(Quit)
    | GUINop -> Cont(s)

  (* center selector light on middle of board square *)
  let center_cursor b =
    (* snap to grid *)
    let (nx,ny) = pos_of_coord (coord_of_pos !cursor_pos b.dims) b.dims in
    (* shift to center *)
    cursor_pos := (nx+.(1.0 /. foi (fst b.dims)), ny+.(1.0 /. foi (snd b.dims)))

  let board b =
    center_cursor b;
    loop_while (fun s ->
        clear_3D ();
        draw_board s.board;
        match key_pressed () with
        | None -> Cont(s)
        | Some(k) -> process_guiaction (guiaction_of_key k s) s
      ) {selected = None; board=b}

  let menu title options default =
    if title = "Hnefatafl"
    then snd (List.hd options)
    else default

  let display_win p = menu (string_of_player p ^ " wins!") ["play again", ()] ()
end
