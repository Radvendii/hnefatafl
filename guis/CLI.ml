open Helpers
open Game_types
open GUI

module GUI : GUI = struct
  let char_of_piece = function
    | BPawn -> 'X'
    | WPawn -> 'O'
    | WKing -> '@'

  let init () = ()
  let deinit () = ()

  let draw_board b =
    let (w,h) = b.dims in
    (* print numbering along the sides *)
    print_string "\nBoard:\n";
    print_string "   ";
    for i = 0 to w-1 do
      print_int i
    done;
    print_string "\n";
    print_string "   ";
    for i = 0 to w-1 do
      print_string "-"
    done;
    print_string "\n";
    (* print the board *)
    for j = 0 to h-1 do
      print_int j;
      if j < 10 then print_string " " else ();
      print_string "|";
      for i = 0 to w do
        print_char @@
        match piece_at (i,j) b with
        | None -> ' '
        | Some(p) -> char_of_piece p
      done;
      print_string "\n";
    done;
    print_string "\n";
    (* print prompt *)
    print_string ((string_of_player b.turn) ^ "'s move: ")

  (* list of commands that will 'quit' *)
  let quit_words =
    ["quit" ; "q" ; "Quit" ; "Q" ; "Exit"; "exit"; "back"; "b"; "Back"]

  let user_input () =
    let inp = read_line () in
    if List.mem inp quit_words then Quit
    else try
        let is = List.map int_of_string (split_words inp) in
        Move((List.nth is 0, List.nth is 1), (List.nth is 2, List.nth is 3))
      (* if anything failed (not ints, too few ints) *)
      with _ -> print_string
                  ( "\nPlease enter a space-separated " ^
                    "list of integers interpreted as:\n" ^
                    "[ix iy fx fy] where the move you want " ^
                    "to execute takes the piece at (ix,iy) " ^
                    "to (fx,fy)\nOr 'Q' to quit to the menu\n\n");
        Nop

  let board b =
    draw_board b;
    user_input ()

  let menu title options default =
    loop_while (fun () ->
        print_string (title ^ ": \n");
        (* print the menu with numbers along the side *)
        List.iteri
          (fun i (s,_) ->
             print_string ("(" ^ string_of_int i ^ "): " ^ s ^ "\n"))
          options;
        (* prompt *)
        print_string "Select (or b/q to go back/quit): ";
        let inp = read_line () in
        if List.mem inp quit_words then Break(default)
        else try
            let i = int_of_string inp in
            Break(snd (List.nth options i))
          (* if anything failed (not an int, not a menu item number) *)
          with _ -> print_string
                      ( "\nPlease enter the number of a menu " ^
                        "item listed above, or 'Q' to quit.");
            Cont(())) ()


  let display_win p =
    print_string ("\n\n" ^ string_of_player p ^ " won!\n\n")

end
