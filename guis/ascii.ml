open Helpers
open Game_types
open GUI

module GUI : GUI = struct
    (* extends termbox to keep track of screen, cursor*)
  module Termbox' = struct
    include Termbox
    (* we shouldn't have boards bigger than 400x400 accross *)
    let screen_w = 400
    let screen_h = 400
    let out_of_bounds (x,y) =
      not
        (0 <= x       &&
         0 <= y       &&
         x < screen_w &&
         y < screen_h)
    let screen = Array.make_matrix screen_w screen_h None
    let cursor = ref((1,1))

    (* records cursor position *)
    let set_cursor x y =
      cursor := (x,y) ;
      Termbox.set_cursor x y
    let get_cursor () = !cursor

    (* records screen cells position*)
    let set_cell_char ?fg:(fg=Default) ?bg:(bg=Default) x y c =
      if out_of_bounds (x,y)
      then failwith "out of bounds" (* TODO: This is kludgy *)
      else ( set_cell_char ~fg:fg ~bg:bg x y c
           ; screen.(x).(y) <- Some c
           )
    let get_cell_char x y = screen.(x).(y)

    (* convenience function to print whole strings *)
    let set_cell_str ?fg:(fg=Default) ?bg:(bg=Default) x y s =
      let rec char_list_helper x' y' cs =
        match cs with
        | [] -> ()
        | c::cs' ->
          match c with
          | '\n' -> char_list_helper x (y'+1) cs'
          | c    ->
            set_cell_char ~fg:fg ~bg:bg x' y' c ;
            char_list_helper (x'+1) y' cs' in
      char_list_helper x y (char_list_of_string s)


    let clear () =
      Termbox.clear ();
      for x = 0 to screen_w - 1 do
        for y = 0 to screen_h - 1 do
          screen.(x).(y) <- None
        done
      done
  end

  open Termbox'

  (* actions taken within the GUI *)
  type guiaction =
    | GUIMoveC  of coord
    | GUISelect of coord
    | GUIMove   of (coord * coord)
    | GUIQuit
    | GUINop

  (* this is a record because we might want
   * to add fields at a later point *)
  type guistate = {selected : coord option}

  let char_of_piece = function
    | BPawn -> 'X'
    | WPawn -> 'O'
    | WKing -> '@'

  let init () = ignore(init ())
  let deinit = shutdown

  (* what to do on an input event *)
  let guiaction_of_event e s =
    let (cx,cy) = get_cursor () in
    match e with
    | Utf8 _ | Resize _ -> GUINop
    | Key k ->
      (match k with
       | Arrow_left  -> GUIMoveC(cx-1, cy)
       | Arrow_down  -> GUIMoveC(cx,   cy+1)
       | Arrow_up    -> GUIMoveC(cx,   cy-1)
       | Arrow_right -> GUIMoveC(cx+1, cy)
       | _ -> GUINop)
    | Ascii c ->
      (match c with
       | 'h' -> GUIMoveC(cx-1, cy)
       | 'j' -> GUIMoveC(cx,   cy+1)
       | 'k' -> GUIMoveC(cx,   cy-1)
       | 'l' -> GUIMoveC(cx+1, cy)
       | 'q' | '\x1B' -> GUIQuit
         (* space bar either selects or moves,
          * depending on if there is already a selected piece *)
       | ' ' ->
         (match s.selected with
          | None -> GUISelect(cx,cy)
          | Some(sx, sy) -> GUIMove((sx,sy),(cx,cy)))
       | _   -> GUINop)

  let draw_board b =
    clear ();

    (* make sure the cursor is drawn *)
    let (x,y) = (get_cursor ()) in
    set_cursor x y;

    (* draw the border *)
    List.iter
      (fun x ->
         set_cell_char x 0 '-';
         set_cell_char x (snd b.dims + 1) '-')
      (0--(fst b.dims + 1)) ;
    List.iter
      (fun y ->
         set_cell_char 0 y '|';
         set_cell_char (fst b.dims + 1) y '|')
      (0--(snd b.dims + 1)) ;

    (* draw the pieces *)
    List.iter
      (fun (p,c) ->
         (* shift by one (+1) to avoid border *)
         set_cell_char (fst c + 1) (snd c + 1) (char_of_piece p))
      b.pieces ;

    (* print who's turn it is *)
    set_cell_str 0 (snd b.dims + 3) ("It is " ^ string_of_player b.turn ^ "'s turn");
    present ()


  (* colors *)
  let title_color = Green
  let selected_color = Red
  (* for selected tile of the board *)
  let board_selected_color = Blue
  let item_color = Default

  (* resets the color of the given tile *)
  let reset_color i j =
    set_cell_char ~bg:Default i j
      (* reset the color of the selectd tile *)
      (match get_cell_char i j with
       | None -> ' '
       | Some c -> c)

  (* user input for when there is a board displayed
   * i.e. what to do with a guiaction *)
  let user_input () =
    loop_while (fun s ->
        match guiaction_of_event (poll_event ()) s with
        | GUIMoveC(x,y) ->
          (* don't allow movement onto the border *)
          if List.mem (get_cell_char x y) [Some('|'); Some('-')] then Cont(s)
          else (set_cursor x y
               ; present ()
               ; Cont(s))
        | GUISelect(x,y) ->
          (match get_cell_char x y with
           | None -> Cont(s)
           | Some c ->
             set_cell_char ~bg:board_selected_color x y c
           ; present ()
           ; Cont({selected = Some(x,y)}))
        | GUIMove((x1,y1),(x2,y2)) ->
          ( reset_color x1 y1
          ; Break(Move((x1-1,y1-1),(x2-1,y2-1))))
        | GUINop -> Cont(s)
        | GUIQuit ->
          match s.selected with
          | None -> Break(Quit)
          | Some(x,y) ->
            ( reset_color x y
            ; present ()
            ; Cont({selected = None}))
      )
      {selected = None}

  let board b =
    draw_board b;
    user_input ()

  let draw_menu title strs (sel : int) =
    hide_cursor ();
    clear ();
    set_cell_str ~fg:title_color 0 0 title ;
    List.iteri (fun i ->
        let fg = if i = sel then selected_color else item_color in
        set_cell_str ~fg:fg 2 ((i+1)*2)) strs;
    present ()

  let menuaction_of_event e =
    match e with
    | Utf8 _ | Resize _ -> `MenuNop
    | Key k ->
      (match k with
       | Arrow_down  -> `MenuDown
       | Arrow_up    -> `MenuUp
       | _ -> `MenuNop)
    | Ascii c ->
      (match c with
       | 'j' -> `MenuDown
       | 'k' -> `MenuUp
       | 'q' | '\x1B' -> `MenuQuit
       | '\x0D' -> `MenuSelect
       | ' ' -> `MenuSelect
       | _   -> `MenuNop)

  (* bounding functions clip the value into the range *)
  let bound_below n b =
    if n < b then b else n
  let bound_above n b =
    if n > b then b else n

  let menu title options default =
    let len = (List.length options)-1 in
    loop_while (fun s ->
        draw_menu title (List.map fst options) s ;
        match menuaction_of_event (poll_event ()) with
        | `MenuNop -> Cont(s)
        | `MenuDown -> Cont(bound_above (s+1) len)
        | `MenuUp -> Cont(bound_below (s-1) 0)
        | `MenuSelect -> Break(snd @@ List.nth options s)
        | `MenuQuit -> Break(default)) 0

  let display_win p =
    menu (string_of_player p ^ " won!") ["back", ()] ()

end
