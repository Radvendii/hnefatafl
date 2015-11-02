open Curses
(* http://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function *)
let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;
let prod l1 l2 =
  List.flatten @@ List.map (fun x -> List.map (fun y -> (x,y)) l2) l1
let w  = initscr ()
let putch i1 i2 c =
  let _ = mvaddch i1 i2 (int_of_char c) in ()
let () = List.iter (fun y -> putch y 0 '|'; putch y 12 '|') (0--12)
let () = List.iter (fun x -> putch 0 x '-'; putch 12 x '-') (0--12)
let () = List.iter (fun (x,y) -> putch y x 'X') @@ (prod (4--8) [1;11])
                                                 @ (prod [1;11] (4--8))
                                                 @ (prod [2;10] [6])
                                                 @ (prod [6] [2;10])
let () = List.iter (fun (x,y) -> putch y x 'O') @@ (prod (5--7) (5--7))
                                                 @ (prod [4;8] [6])
                                                 @ (prod [6] [4;8])
let () = putch 6 6 '@'
let _  = refresh ()
let () = Unix.sleep 5
let () = endwin ()
