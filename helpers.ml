(* http://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function *)
let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

let prod l1 l2 =
  List.flatten @@ List.map (fun x -> List.map (fun y -> (x,y)) l2) l1

let in_range (xd,yd) (x,y) =
  0 <= x   &&
  0 <= y   &&
  x < xd &&
  y < yd

let rec loop_while f s =
  match f s with
  | `Cont(s') -> loop_while f s'
  | `Break(v) -> v
