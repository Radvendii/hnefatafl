(*
   NAME: 3D
*)
open Helpers
open Game_types
open GUI

module GUI : GUI = struct
  module Sdl' = struct
    let initialized = ref(false)
    let foi = float_of_int
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
    let normal_vertex3 (n,v) =
      GlDraw.normal3 n;
      GlDraw.vertex3 v

    let deinit () =
      (* prevents from deinitializing more than once *)
      if !initialized
      then
        (* give the event-processing thread a chance to
         * close *)
        ( Sdlevent.add [Sdlevent.QUIT]
        ; Sdltimer.delay 150
        ; Sdl.quit ()
        ; initialized := false)
      else ()

    let init w h t () =
      initialized := true;
      Sdl.init [`VIDEO];
      ignore (Sdlvideo.set_video_mode w h [`DOUBLEBUF; `OPENGL]);
      ignore @@
      (* thread to handle events/input *)
      Thread.create
        (loop_while (fun () ->
             let open Sdlevent in
             (* I'm not sure if this try-with block is necessary
              * I put it in a whole bunch of measures to ensure
              * this thread doesn't fail *)
             try
               (
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
               )
             with _ -> deinit (); Break(())
           )) ()

    let clear_3D () =
      GlClear.clear[`color; `depth];
      Gl.enable `depth_test;
      GlMat.mode `modelview;
      GlMat.load_identity ();
      Gl.enable `lighting;
      Gl.enable `light0; (* lighting *)
      Gl.enable `light1; (* lighting *)
      Gl.enable `light2; (* under cursor *)
      GlLight.light ~num:0 (`diffuse (0.2,0.2,0.2,1.0));
      GlLight.light ~num:0 (`position(3.0, -3.0, -6.0, 1.0));
      GlLight.light ~num:1 (`diffuse (0.2,0.2,0.2,1.0));
      GlLight.light ~num:1 (`position(-3.0, 3.0, -6.0, 1.0));
      Gl.enable `color_material;
      GlLight.color_material `both `ambient_and_diffuse;
      GlMat.mode `projection;
      GlMat.load_identity ();
      (* position camera *)
      GluMat.perspective ~fovy:60.0 ~aspect:1.33 ~z:(0.5,100.0);
      GluMat.look_at ~eye:(0., 1.5, -1.5) ~center:(0., 0., 0.) ~up:(0., -1., 0.)

    (* clear for menu
     * didn't get it working though *)
    (* let clear_2D () = *)
    (*   GlClear.clear[`color; `depth]; *)
    (*   Gl.disable `lighting; *)
    (*   Gl.disable `color_material; *)
    (*   GlMat.mode `projection; *)
    (*   GlMat.load_identity (); *)
    (*   () *)
  end

  open Sdl'

  let cursor = ref(0,0)

  (* it's just a linear function mapping
   * (0,w) -> (-1,1) and (0,h) -> (-1,1) *)
  let pos_of_coord (xc,yc) (w,h) =
    -1.0 +. foi (xc * 2) /. foi w,
    -1.0 +. foi (yc * 2) /. foi h

  (* linear function mapping
   * (-1,1) -> (0,w) and (0,h)
   * combined with a flooring *)
  let coord_of_pos (xp,yp) (w,h) =
    int_of_float ((xp+.1.0) /. 2.0 *. foi w),
    int_of_float ((yp+.1.0) /. 2.0 *. foi h)

  type guistate = { selected : coord option
                  ; board    : board
                  }

  let init () =
    init 800 600 "Hnefatafl" ()

  let deinit =
    deinit

  (* colors *)
  let white_color = (0.4, 0.4, 0.4)
  let black_color = (0.08, 0.08, 0.08)

  let draw_piece ?(alpha=1.0) p =
    GlDraw.begins `triangles;
    (
      match p with
      | WPawn ->
        GlDraw.color ~alpha:alpha white_color;
        List.iter normal_vertex3 Pawn.Data.vertex_list
      | BPawn ->
        GlDraw.color ~alpha:alpha black_color;
        List.iter normal_vertex3 Pawn.Data.vertex_list
      | WKing ->
        GlDraw.color ~alpha:alpha white_color;
        List.iter normal_vertex3 King.Data.vertex_list
    );
    GlDraw.ends ()

  let draw_board ?(selected=None) b =
    GlClear.color
      (match b.turn with
        | White -> (0.8,0.8,0.8)
        | Black -> black_color);
    clear_3D ();
    let (w,h) = b.dims in
    let size = max w h in
    (* draw 2D squares slightly above the board and pointing up *)
    let draw_v2s v2s =
      GlDraw.begins `quads;
      List.iter (fun (x,y) -> normal_vertex3 ((0.,0.,-1.0), (x,y,-.0.001))) v2s;
      GlDraw.ends () in
    (* draw illuminating square for cursor *)
    let draw_cursor () =
      (* spotlight on cursor piece *)
      GlLight.light ~num:2 (`diffuse (0.0,0.0,1.0,1.0));
      GlLight.light ~num:2 (`position(0.0,0.0,-10.0,1.0));
      GlLight.light ~num:2 (`spot_cutoff 4.0);
      GlLight.light ~num:2 (`spot_direction (0.0,0.0,1.0));
      (* highlighted square under the cursor *)
      GlDraw.color (0.5,0.5,0.5);
      GlLight.material `both (`emission (0.0,0.0,1.0,1.0));
      draw_v2s
        (
          [ -0.5, -0.5
          ; -0.5,  0.5
          ;  0.5,  0.5
          ;  0.5, -0.5
          ]
        );
      GlLight.material `both (`emission (0.0,0.0,0.0,0.0)) in

    let transform_to_square (i,j) =
      GlMat.mode `modelview;
      GlMat.load_identity ();
      let scale = 2. /. foi size in
      let (x,y) = pos_of_coord (i, j) b.dims in
      GlMat.translate3 (x, y, 0.);
      GlMat.translate3 (scale /. 2., scale /. 2., 0.);
      GlMat.scale3 (scale, scale, scale) in

    (* draw game board *)
    GlMat.mode `modelview;
    GlMat.load_identity ();
    GlDraw.begins `quads;
    (* GlDraw.color (0.511, 0.328, 0.08); *) (* alternative brown color *)
    GlDraw.color (0.5, 0.5, 0.5);
    List.iter normal_vertex3 Game_board.Data.vertex_list;
    GlDraw.ends ();

    (* draw grid on board *)
    GlDraw.color (0.6, 0.6, 0.6);
    (* GlDraw.color (0.611, 0.418, 0.15); *) (* alternative brown color *)
    let strip_width = 0.005 in

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


    transform_to_square !cursor;
    draw_cursor ();

    (match selected with
    | None -> Gl.disable `blend;
    | Some(c) ->
      Gl.enable `blend;
      GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
      match piece_at c b with
      | None -> ()
      | Some(p) ->
        (
          (* indicate if the given move is valid *)
          if MODE_list.valid_move c !cursor b || c = !cursor
          then GlLight.material `both (`emission (0.0,0.1,0.0,1.0))
          else GlLight.material `both (`emission (0.2,0.0,-0.3,1.0))
        );
        draw_piece ~alpha:0.8 p);
    GlLight.material `both (`emission (0.0,0.0,0.0,0.0));

    (* draw pieces *)
    List.iter (fun (p, (i,j)) ->
        transform_to_square (i,j);
        (if selected = Some(i,j)
         then draw_piece ~alpha:0.5
         else draw_piece ~alpha:1.0) p;
        GlLight.material `both (`shininess 0.0)
      ) b.pieces;
    Gl.flush ();
    Sdlgl.swap_buffers ()

  (* type for actions taken within the GUI *)
  type guiaction =
    | GUIMoveC  of coord
    | GUISelect of coord
    | GUIMove   of (coord * coord)
    | GUIQuit
    | GUINop

  (* process input *)
  let guiaction_of_key k s =
    let (w,h) = s.board.dims in
    let (cx,cy) = !cursor in
    match k with
    | Sdlkey.KEY_h
    | Sdlkey.KEY_LEFT  ->
      if cx > 0
      then GUIMoveC(cx-1, cy)
      else GUINop
    | Sdlkey.KEY_j
    | Sdlkey.KEY_DOWN  ->
      if cy < (h-1)
      then GUIMoveC(cx, cy+1)
      else GUINop
    | Sdlkey.KEY_k
    | Sdlkey.KEY_UP    ->
      if cy > 0
      then GUIMoveC(cx, cy-1)
      else GUINop
    | Sdlkey.KEY_l
    | Sdlkey.KEY_RIGHT ->
      if cx < (w-1)
      then GUIMoveC(cx+1, cy)
      else GUINop
    | Sdlkey.KEY_q
    | Sdlkey.KEY_ESCAPE -> GUIQuit
      (* space selects or moves the piece,
       * depending on if there is already a selected piece *)
    | Sdlkey.KEY_SPACE ->
      (match s.selected with
       | None -> GUISelect(cx,cy)
       | Some(sx, sy) -> GUIMove((sx,sy),(cx,cy)))
    | _   -> GUINop

  let process_guiaction a s =
    match a with
    | GUIMoveC(cx, cy) ->
      cursor := (cx,cy);
      Cont(s)
    | GUISelect(x,y) ->
      Cont({s with
            selected = Some(x,y)
           })
    | GUIMove(c1,c2) -> Break(Move(c1,c2))
    | GUIQuit -> Break(Quit)
    | GUINop -> Cont(s)

  let board b =
    loop_while (fun s ->
        draw_board ~selected:s.selected s.board;
        match key_pressed () with
        | None -> Cont(s)
        | Some(k) -> process_guiaction (guiaction_of_key k s) s
      ) {selected = None; board=b}

  (* displaying text in OpenGL is hard;
   * I'm just going to let Graphical.GUI do it.
   * TODO: Make this not flicker back to this GUI
   * whenever the menu is changed *)
  let menu title options default =
    deinit ();
    Graphical.GUI.init ();
    let r = Graphical.GUI.menu title options default in
    Graphical.GUI.deinit ();
    init ();
    r

  let display_win p = menu (string_of_player p ^ " wins!") ["play again", ()] ()

  (* OCaml can't figure out optional parameters don't restrict the type,
   * so make OCaml happy *)
  let draw_board b= draw_board b
end
