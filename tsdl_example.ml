open Helpers

let main () =
  Sdl.init [`VIDEO];
  at_exit Sdl.quit;
  let screen = Sdlvideo.set_video_mode 200 200 [`DOUBLEBUF; `OPENGL] in
  ignore(screen);
  let angle t = 10. *. t *. t in
  let render () =
    GlClear.clear [ `color ];
    GlMat.mode `modelview;
    GlMat.load_identity ();
    GlMat.rotate ~angle: (angle (Sys.time ())) ~z:1. ();
    GlDraw.begins `triangles;
    List.iter GlDraw.vertex2 [-1., -1.; 0., 1.; 1., -1.];
    GlDraw.ends () in
  render ();
  (* loop_while (fun () -> *)
  (*     let x = false in *)
  (*     if x then Break(()) *)
  (*     else (render (); Cont(()))) (); *)
  Sdlgl.swap_buffers ();
  Sdltimer.delay 2000;
  exit 0

let () = main ()
