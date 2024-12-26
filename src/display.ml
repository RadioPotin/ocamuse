  let view_flat ctx ocamuse_context event =
    let open Types in
    let make_struc ctx fretboard color : Types.flat_view_draw_struc  =
      let offset_for_frets_numbers =
          match !(ocamuse_context.display_mode) with
          | Flat Plain _ -> 0
          | Flat Fretted _ -> 1
          | Flat Interline _ -> 1
          | Pattern _ -> assert false
      in
      let number_of_strings = Array.length fretboard in
      let cursor_j = ref 0 in
      let offset = ref offset_for_frets_numbers in
      {
        ctx;
        color;
        offset;
        cursor_j;
        fretboard;
        string = ref 0;
        number_of_strings;
        guitar_string = ref fretboard.(0);
      }
  in
  let color = Color.event_to_color_flat_view event in
  let struc = make_struc ctx ocamuse_context.fretboard color in
  match event with
  | Fretted _ ->
    Draw.PLAIN.rows_with_no_interline struc
  | Interline _ ->
    Draw.PLAIN.rows_with_interlines struc
  | Plain _ ->
    Draw.PLAIN.rows struc

let view_pattern ctx view ocamuse_context mode =
  let open Types in
  let make_pattern_struc
      ctx
      mode
      view
      notes_to_degree_tbl
      degree_to_color_tbl
      ocamuse_context
    : Types.pattern_view_draw_struc
    =
    let cursor_i = ref 0 in
    let cursor_j = ref 0 in
    let offset_for_frets_numbers =
      match !(ocamuse_context.display_mode) with
      | Pattern (Plain _, _) -> 0
      | Pattern (Fretted _, _) -> 1
      | Pattern (Interline _, _) -> 1
      | Flat _ -> assert false
    in
    let fretboard = ocamuse_context.fretboard in
    let offset = ref offset_for_frets_numbers in
    let number_of_strings = Array.length ocamuse_context.fretboard in
    let color = Color.event_to_base_color !(ocamuse_context.base_colour) in
    let number_of_frets = Array.length ocamuse_context.fretboard.(0) in
    {
      ctx;
      mode;
      view;
      color;
      offset;
      cursor_i;
      cursor_j;
      fretboard;
      string = ref 0;
      number_of_frets;
      number_of_strings;
      notes_to_degree_tbl;
      degree_to_color_tbl;
    }
  in
  begin
    let tonality = Ocamuse.build_tonality mode { base = C; alteration = 0} in
    (* make table with Types.notes as keys, and int falseas value (key degrees) *)
    let notes_to_degree_tbl = Ocamuse.build_degree_tbl tonality in
    (* make table with int (key degrees) as keys, and Types.color_plain_view_event as value  *)
    let degree_to_colour_tbl = Ocamuse.build_degree_colour_tbl tonality in
    let struc =
      make_pattern_struc
        ctx
        mode
        view
        notes_to_degree_tbl
        degree_to_colour_tbl
        ocamuse_context
    in
    Draw.PATTERN.pattern struc
  end
