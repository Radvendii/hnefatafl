open Game_types
open GUI

module AsciiGUI : GUI = struct
  module Termbox' = struct (* extends termbox to keep track of screen, cursor*)
    (* HERE BE DRAGONS *)
    include Termbox
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
    let set_cursor x y =
      cursor := (x,y) ;
      Termbox.set_cursor x y
    let get_cursor () = !cursor
    let set_cell_char ?fg:(fg=Default) ?bg:(bg=Default) x y c =
      if out_of_bounds (x,y)
      then failwith "out of bounds" (* TODO: This is kludgy *)
      else ( set_cell_char ~fg:fg ~bg:bg x y c
           ; screen.(x).(y) <- Some c
           )
    let get_cell_char x y = screen.(x).(y)

    let clear () =
      Termbox.clear ();
      for x = 0 to screen_w - 1 do
        for y = 0 to screen_h - 1 do
          screen.(x).(y) <- None
        done
      done
  end

  open Termbox'

  type guiaction =
    | GUIMoveC  of coord
    | GUISelect of coord
    | GUIMove   of (coord * coord)
    | GUIQuit
    | GUINop

  type guistate = {selected : coord option}

  let init () = let _ = init () in ()
  let deinit = shutdown

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
       | ' ' ->
         (match s.selected with
          | None -> GUISelect(cx,cy)
          | Some(sx, sy) -> GUIMove((sx,sy),(cx,cy)))
       | _   -> GUINop)

  let draw_board b =
    (* draw the border *)
    clear () ;
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
         set_cell_char (fst c + 1) (snd c + 1) (char_of_piece p)) (* shift by one to avoid border *)
      b.pieces ;
    present ()

  let user_input () =
    loop_while (fun s ->
        match guiaction_of_event (poll_event ()) s with
        | GUIMoveC(x, y) ->
          if not @@ List.mem (get_cell_char x y) [Some('|'); Some('-')] (* don't allow movement onto the border *)
          then (set_cursor x y
               ; present ()
               ; `Cont(s))
          else `Cont(s)
        | GUISelect(x, y) ->
          set_cell_char ~bg:Blue x y
            (match get_cell_char x y with
             | None -> ' '
             | Some c -> c)
        ; `Cont({selected = Some (x,y)})
        | GUIMove((x1,y1),(x2,y2)) ->
          set_cell_char x1 y1
            (match get_cell_char x1 y1 with
             | None -> ' '
             | Some c -> c) (* reset the color *)
        ; `Break(Move((x1-1,y1-1),(x2-1,y2-1)))
        | GUINop -> `Cont(s)
        | GUIQuit -> `Break(Quit))
      {selected = None}

end
