open Game_types
open Ascii.AsciiGUI

(* TODO? read in inital positions and rules from textfile *)
let init_board =
  { dims = (11,11)
  ; pieces =
      List.flatten @@ (* TODO: this double-list is a bit of a kludge; fix it.*)
      List.map (List.map (fun c -> BPawn, c))
        [ (prod (3--7) [0;10])
        ; (prod [0;10] (3--7))
        ; (prod [1;9] [5])
        ; (prod [5] [1;9])
        ]

      @
      List.map (List.map (fun c -> WPawn, c))
        [ (prod (4--6) (4--6))
        ; (prod [3;7] [5])
        ; (prod [5] [3;7])
        ]

      @
      [[WKing, (5,5)]]
  }

let rec pop_find f = function
  | [] -> [], None
  | x::xs ->
    if f x
    then (xs, Some x)
    else let (xs', x') = pop_find f xs in (x::xs', x')

let () =
  init () ;
  loop_while (fun b ->
      draw_board b ;
      match user_input () with
      | Move(c1, c2) ->
        (`Cont { b with
                 pieces =
                   let ps, p1 = pop_find (fun (_,c) -> c = c1) b.pieces in
                   let ps', _ = pop_find (fun (_,c) -> c = c2) ps in
                   match p1 with
                   | None -> ps (* if someone tries to move nothing, nothing happens*)
                   | Some(p1') -> (fst p1', c2)::ps' (* move the piece!*)
               })
      | Quit -> `Break(())
      | Nop -> `Cont(b)
    ) init_board ;
  deinit ()
