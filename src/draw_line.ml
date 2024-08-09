open LTerm_text

let write_interline (struc :Types.flat_view_draw_struc) =
  let open LTerm_draw in
  let guitar_string = struc.guitar_string in
  let string_line =
    Pp.FRETBOARD.FMT.stringify_interline (struc.string, !guitar_string)
  in
  draw_styled struc.ctx
    !(struc.cursor_j)
    !(struc.offset)
    (eval [B_fg struc.color; S string_line; E_fg])

let write_frets (struc :Types.flat_view_draw_struc) =
  let open LTerm_draw in
  let guitar_string = struc.guitar_string in
  let string_line =
    Pp.FRETBOARD.FMT.stringify_frets (struc.string, !guitar_string)
  in
  draw_styled struc.ctx
    !(struc.cursor_j)
    !(struc.offset)
    (eval [B_fg struc.color; S string_line; E_fg])

let write_fret_numbers (struc :Types.flat_view_draw_struc) =
  let guitar_string = struc.guitar_string in
  let open LTerm_draw in
  let fret_line =
    Pp.FRETBOARD.FMT.stringify_frets_numbers
      (struc.string, !guitar_string)
  in
  draw_styled struc.ctx
    !(struc.cursor_j)
    !(struc.offset)
    (eval [B_fg struc.color ; S fret_line; E_fg])

let write_plain_frets (struc :Types.flat_view_draw_struc) =
  let open LTerm_draw in
  let string_line =
    Pp.FRETBOARD.FMT.stringify_plain_string (!(struc.string), !(struc.guitar_string))
  in
  draw_styled struc.ctx
    !(struc.cursor_j)
    !(struc.offset)
    (eval [B_fg struc.color; S string_line; E_fg])
