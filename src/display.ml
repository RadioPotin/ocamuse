let select_view ctx ocamuse_context =
  let open Types in
  let bubble_view = function
    | Flat view -> view
    | Pattern (view, _mode) -> view
  in
  let make_struc ctx mode view notes_to_degree_tbl degree_to_color_tbl
      ocamuse_context : Types.pattern_view_draw_struc =
    let cursor_i = ref 0 in
    let cursor_j = ref 0 in
    let offset_for_frets_numbers =
      match view with
      | Plain _ -> 0
      | Fretted _ -> 2
      | Interline _ -> 1
    in
    let fretboard = ocamuse_context.fretboard in
    let offset = ref offset_for_frets_numbers in
    let number_of_strings = Array.length ocamuse_context.fretboard in
    let color = Color.event_to_base_color !(ocamuse_context.base_colour) in
    let number_of_frets = Array.length ocamuse_context.fretboard.(0) in
    { ctx
    ; mode
    ; view
    ; color
    ; offset
    ; cursor_i
    ; cursor_j
    ; fretboard
    ; string = ref 0
    ; number_of_frets
    ; number_of_strings
    ; notes_to_degree_tbl
    ; degree_to_color_tbl
    }
  in
  LTerm_draw.clear ctx;
  let tonality = Theory.build_tonality ocamuse_context.mode { base = C; alteration = 0 } in
  (* make table with Types.notes as keys, and int falseas value (key degrees) *)
  let notes_to_degree_tbl = Theory.build_degree_tbl tonality in
  (* make table with int (key degrees) as keys, and Types.color_plain_view_event as value  *)
  let degree_to_colour_tbl = Theory.build_degree_colour_tbl tonality in
  let display_mode = !(ocamuse_context.display_mode) in
  let view = bubble_view display_mode in
  begin
    match display_mode with
    | Flat _base_colour ->

      let struc =
        make_struc ctx C_mode view notes_to_degree_tbl degree_to_colour_tbl
          ocamuse_context
      in
      Draw.PATTERN.pattern struc

    | Pattern (view, mode) ->

      let struc =
        make_struc ctx mode view notes_to_degree_tbl degree_to_colour_tbl
          ocamuse_context
      in
      Draw.PATTERN.pattern struc
  end
