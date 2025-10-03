(** Drawing chord diagrams for guitar chords *)


(** [draw_chord_diagram ctx voicing chord_name] draws a guitar chord diagram
    showing finger positions on the fretboard.

    The diagram shows:
    - Vertical lines for strings (6 strings for standard guitar)
    - Horizontal lines for frets
    - Dots or numbers showing where to place fingers
    - 'x' for muted strings
    - 'o' for open strings
    - Fret number indicator on the left if not starting at fret 0
*)
let draw_chord_diagram ctx row_offset col_offset voicing chord_name =
  let open LTerm_draw in
  let open LTerm_style in
  let open LTerm_geom in

  let num_strings = Array.length voicing.Theory.positions in
  let diagram_height = 6 in  (* 5 frets shown *)

  let { rows; cols } = size ctx in

  (* Helper to draw a character at position *)
  let draw_at row col ch style =
    if row >= 0 && row < rows && col >= 0 && col < cols then
      draw_char ctx row col (Zed_char.unsafe_of_char ch) ~style
  in

  (* Helper to draw a string of text *)
  let draw_string_at row col text style =
    for i = 0 to String.length text - 1 do
      draw_at row (col + i) text.[i] style
    done
  in

  (* Draw chord name at top - offset to align with diagram *)
  let title_style = { none with bold = Some true; foreground = Some lcyan } in
  let title_col = col_offset + 35 in  (* Shift chord name far right *)
  draw_string_at row_offset title_col chord_name title_style;

  let diagram_row = row_offset + 2 in
  let diagram_col = col_offset + 41 in  (* Diagram starts after chord name with padding *)

  (* Draw fret number if not at the nut (fret 0) *)
  if voicing.Theory.start_fret > 0 then begin
    let fret_label = Fmt.str "%dfr" voicing.Theory.start_fret in
    draw_string_at diagram_row diagram_col fret_label
      { none with foreground = Some white }
  end;

  let string_col_start = diagram_col + 4 in

  (* Draw open/muted indicators above the diagram *)
  for string_idx = 0 to num_strings - 1 do
    (* Reverse string order: low strings (high idx) on left, high strings (low idx) on right *)
    let reversed_idx = num_strings - 1 - string_idx in
    let col = string_col_start + (reversed_idx * 2) in
    match voicing.Theory.positions.(string_idx) with
    | None ->
      (* Muted string *)
      draw_at (diagram_row - 1) col 'x' { none with foreground = Some lred }
    | Some (_, fret) when fret = 0 ->
      (* Open string *)
      draw_at (diagram_row - 1) col 'o' { none with foreground = Some lgreen }
    | _ ->
      (* Fretted note - no indicator needed *)
      ()
  done;

  (* Draw the fretboard grid *)
  (* Horizontal lines (frets) *)
  for fret_line = 0 to 5 do
    let row = diagram_row + fret_line in
    for string_idx = 0 to num_strings - 1 do
      let reversed_idx = num_strings - 1 - string_idx in
      let col = string_col_start + (reversed_idx * 2) in
      let char = if fret_line = 0 && voicing.Theory.start_fret = 0 then '=' else '-' in
      draw_at row col char { none with foreground = Some white }
    done
  done;

  (* Vertical lines (strings) *)
  for string_idx = 0 to num_strings - 1 do
    let reversed_idx = num_strings - 1 - string_idx in
    let col = string_col_start + (reversed_idx * 2) in
    for fret_space = 0 to 4 do
      let row = diagram_row + fret_space in
      (* Don't overwrite horizontal lines *)
      if fret_space < 5 then
        draw_at (row + 1) col '|' { none with foreground = Some white }
    done
  done;

  (* Draw finger positions *)
  for string_idx = 0 to num_strings - 1 do
    match voicing.Theory.positions.(string_idx) with
    | Some (_, fret) when fret > 0 ->
      let relative_fret = fret - voicing.Theory.start_fret in
      if relative_fret > 0 && relative_fret <= 5 then begin
        let reversed_idx = num_strings - 1 - string_idx in
        let col = string_col_start + (reversed_idx * 2) in
        (* Position in the fret space (between fret lines) *)
        (* For fret N, place finger between line N-1 and line N *)
        let row = diagram_row + relative_fret in
        draw_at row col '*' { none with foreground = Some lyellow; bold = Some true }
      end
    | _ -> ()
  done;

  (* Draw fret numbers on the right side of the diagram *)
  (* Show fret numbers for each fret line in the diagram *)
  for fret_line = 1 to 5 do
    let actual_fret = voicing.Theory.start_fret + fret_line in
    let row = diagram_row + fret_line in
    let col = string_col_start + (num_strings * 2) + 2 in
    let fret_str = string_of_int actual_fret in
    draw_string_at row col fret_str { none with foreground = Some lblue }
  done;

  (* Return the height used *)
  diagram_height + 3


(** [get_diagram_dimensions ()] returns (width, height) needed for a chord diagram *)
let get_diagram_dimensions num_strings =
  let width = num_strings * 2 + 10 in  (* Extra space for labels *)
  let height = 10 in  (* Name + spacing + diagram *)
  (width, height)
