open Helpers
open Game_types
open GUI

module GUI : GUI = struct
  module Sdl' = struct
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

    let clear () =
      GlClear.clear[`color; `depth];
      Gl.enable `depth_test;
      GlMat.mode `projection;
      GlMat.load_identity ();
      Gl.enable `lighting;
      Gl.enable `light0;
      Gl.enable `light1;
      GlLight.light ~num:0 (`diffuse (0.5,0.5,0.5,1.0));
      GlLight.light ~num:0 (`position(3.0, -3.0, -6.0, 1.0));
      GlLight.light ~num:1 (`diffuse (0.2,0.2,0.2,1.0));
      GlLight.light ~num:1 (`position(-3.0, 3.0, -6.0, 1.0));
      Gl.enable `color_material;
      GlLight.color_material `both `ambient_and_diffuse;
      GluMat.perspective ~fovy:60.0 ~aspect:1.2 ~z:(0.5,100.0);
      GluMat.look_at ~eye:(0., -1.5, -2.0) ~center:(0., 0., 0.) ~up:(0., 1., 0.);
      ()


  end
  open Sdl'
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
    let foi = float_of_int in
    let (w,h) = b.dims in
    let size = max w h in
    let calc_x i = -1.0 +. 2.0 *. foi i /. foi w in
    let calc_y j = -1.0 +. 2.0 *. foi j /. foi h in
    clear ();
    (* draw game board *)
    GlMat.mode `modelview;
    GlMat.load_identity ();
    GlMat.scale3 (1. *. foi w /. foi h , 1. *. foi h /. foi w, 1.);
    GlDraw.begins `quads;
    GlDraw.color (0.511, 0.328, 0.08);
    (* GlDraw.color (0.5, 0.5, 0.5); *)
    List.iter normal_vertex3 Game_board.Data.vertex_list;
    GlDraw.ends ();

    (* draw grid on board *)
    (* GlDraw.color (0.511, 0.328, 0.08); *)
    let board_height =
      -.
      (List.fold_left
        (fun acc (_,(_,_,z)) -> max acc (-.z))
        0.0
        Game_board.Data.vertex_list) in
    let board_height = board_height -. 0.01 in
    let strip_width = 0.01 in
    let draw_v2s v2s =
      GlDraw.begins `quads;
      List.iter (fun (x,y) -> normal_vertex3 ((0.,0.,-1.0), (x,y,board_height))) v2s;
      GlDraw.ends () in
    let draw_strip_x y =
      draw_v2s
        [ -1.0, y-.strip_width
        ; -1.0, y+.strip_width
        ;  1.0, y+.strip_width
        ;  1.0, y-.strip_width
        ] in

    let draw_strip_y x =
      draw_v2s
        [ x-.strip_width, -1.0
        ; x+.strip_width, -1.0
        ; x+.strip_width,  1.0
        ; x-.strip_width,  1.0
        ] in
    List.iter (fun j -> draw_strip_x (-1.0 +. (foi (j * 2) /. foi h))) (0 -- h);
    List.iter (fun i -> draw_strip_y (-1.0 +. (foi (i * 2) /. foi w))) (0 -- w);

    (* draw pieces *)
    List.iter (fun (p, (i,j)) ->
        GlMat.load_identity ();
        let scale = 2. /. foi size in
        GlMat.scale3 (1. *. foi w /. foi h , 1. *. foi h /. foi w, 1.);
        GlMat.translate3 (calc_x i, calc_y j, 0.);
        GlMat.translate3 (scale /. 2., scale /. 2., 0.);
        (* undo stretch-scale *)
        GlMat.scale3 (1. *. foi h /. foi w , 1. *. foi w /. foi h, 1.);
        GlMat.scale3 (scale, scale, scale);
        draw_piece p
      ) b.pieces;
    Gl.flush ();
    Sdlgl.swap_buffers ()

  let board b =
    draw_board b;
    Nop

  let menu title options default =
    if title = "Hnefatafl"
    then snd (List.hd options)
    else default

  let display_win p = menu (string_of_player p ^ " wins!") ["play again", ()] ()
end
