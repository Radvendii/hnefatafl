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

type ('a, 'b) looptype =
  | Cont of 'a
  | Break of 'b

let rec loop_while f s =
  match f s with
  | Cont(s') -> loop_while f s'
  | Break(v) -> v

(*split input string into a char list
 * inverse of join
 *)
let rec char_list_of_string str =
    if str = ""
    then []
    else let open String in
        (get str 0) :: (char_list_of_string (sub str 1 ((length str) - 1)))

(*join input char list into a string
 * inverse of explode
 *)
let rec string_of_char_list xs =
  List.fold_left (^) "" (List.map Char.escaped xs)

let rec split_words s =
  List.fold_right
    (fun c acc ->
       if c = ' '
       then ""::acc
       else
         let s = Char.escaped c in
         (match acc with
          | [] -> [s]
          | w::ws -> (w^s)::ws)
    ) (char_list_of_string s) []


type direction = Up | Down | Left | Right
let step_dir dir (x,y) =
  match dir with
  | Up -> (x,y+1)
  | Down -> (x, y-1)
  | Left -> (x-1, y)
  | Right -> (x+1, y)

let step_two_dir dir c = step_dir dir (step_dir dir c)

let rec pop_find f = function
  | [] -> [], None
  | x::xs ->
    if f x
    then (xs, Some x)
    else let (xs', x') = pop_find f xs in (x::xs', x')

