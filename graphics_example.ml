open Graphics

let f () =
  open_graph "";
  (* resize_window 1000 1000; *)
  auto_synchronize false;
  set_window_title "Test";
  ()

let rec loop f () =
  match f () with
  | true -> loop f ()
  | false -> ()

let _ =
  f ();
  loop (fun () ->
  clear_graph ();
  fill_rect 0 0 100 100;
  synchronize ();
  true
  ) ();
  close_graph ()
