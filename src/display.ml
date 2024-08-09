let make_struc ctx fretboard color : Types.flat_view_draw_struc  =
  let offset_for_frets_numbers = 4 in
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

let view_plain_rows ctx fretboard color =
  let struc = make_struc ctx fretboard color in
  let update_field field i = field := i in
  let i = ref 0 in
  while !i < struc.number_of_strings do
    update_field struc.string !i;
    update_field struc.guitar_string struc.fretboard.(!i) ;
    update_field struc.cursor_j (!(struc.cursor_j) + 1);
    Draw_line.write_plain_frets struc;
    i := !i + 1
  done

let view_rows_with_interlines ctx fretboard color =
  let struc = make_struc ctx fretboard color in
  let update_field field i = field := i in
  let move_cursor (struc : Types.flat_view_draw_struc) =
    update_field struc.cursor_j (!(struc.cursor_j) + 1)
  in
  (* top fret numbers *)
  Draw_line.write_fret_numbers struc;
  move_cursor struc;
  (* interline *)
  Draw_line.write_interline struc;
  move_cursor struc;
  (* iterate over rest of strings *)
  Draw_line.write_frets struc;
  move_cursor struc;
  Draw_line.write_interline struc;
  move_cursor struc;
  let i = ref 1 in
  while !i < struc.number_of_strings do
    update_field struc.guitar_string struc.fretboard.(!i);
    (* frets and notes *)
    Draw_line.write_frets struc;
    move_cursor struc;
    update_field struc.string !i;
    (* interline *)
    Draw_line.write_interline struc;
    move_cursor struc;
    incr i
  done

let view_rows_with_no_interline ctx fretboard color =
  let open LTerm_text in
  let struc = make_struc ctx fretboard color in
  Array.iteri (fun string_nb string ->
    let offset = !(struc.offset) in
    if string_nb = 0 then
      begin
        let fret_line =
          Pp.FRETBOARD.FMT.stringify_frets_numbers (string_nb, string)
        in
        LTerm_draw.draw_styled struc.ctx
          (offset - 1)
          offset
          (eval [B_fg struc.color ; S fret_line; E_fg])
      end;
    let string_line =
      Pp.FRETBOARD.FMT.stringify_frets (string_nb, string)
    in
    LTerm_draw.draw_styled struc.ctx
      (string_nb + offset)
      offset
      (eval [B_fg struc.color; S string_line; E_fg]);
  ) struc.fretboard

(* This module will hold all functions that aim to build and highlight a
   given pattern on the fretboard *)
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
    let offset_for_frets_numbers = 4 in
    let fretboard = ocamuse_context.fretboard in
    let offset = ref offset_for_frets_numbers in
    let number_of_strings = Array.length ocamuse_context.fretboard in
    let color = Pp.COLOR.event_to_color !(ocamuse_context.base_colour) in
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
      guitar_string = ref fretboard.(0);
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
    Draw_cell.write_pattern struc
  end
