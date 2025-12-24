module INTERLINE = struct
  module WRITE = struct
    let writerate (struc : Types.pattern_view_draw_struc) style string =
      Draw_primitives.draw_string struc.ctx
        (!(struc.cursor_j) + !(struc.offset))
        (!(struc.cursor_i) + !(struc.offset))
        Draw_primitives.Standard style string

    let writerate_for_interlines (struc : Types.pattern_view_draw_struc) style
      string =
      Draw_primitives.draw_string struc.ctx
        (!(struc.cursor_j) + !(struc.offset))
        (!(struc.cursor_i) + !(struc.offset))
        Draw_primitives.ForInterlines style string

    let writerate_for_frets (struc : Types.pattern_view_draw_struc) style string =
      Draw_primitives.draw_string struc.ctx
        (!(struc.cursor_j) + !(struc.offset))
        (!(struc.cursor_i) + !(struc.offset))
        Draw_primitives.WithPadding style string
  end

  let write_note (struc : Types.pattern_view_draw_struc) j i =
    let open LTerm_style in
    let note = struc.fretboard.(j).(i) in
    let note_string = Pp.FRETBOARD.FMT.stringify Pp.NOTES.FMT.print_note note in
    (* Note is diatonic if its pitch class exists in the degree table *)
    let pitch_class = Conv.note_to_int note in
    let bold = Hashtbl.mem struc.notes_to_degree_tbl pitch_class in
    let note_color = Color.find_color struc note in
    let style = { none with bold = Some bold; foreground = Some note_color } in
    WRITE.writerate struc style note_string;
    note_string

  let fretted_note (struc : Types.pattern_view_draw_struc) fret_j fret_i =
    let open LTerm_style in
    let module M = Pp.FRETBOARD.FMT in
    let note = struc.fretboard.(!fret_j).(!fret_i) in
    let before_note (struc : Types.pattern_view_draw_struc) fret_i =
      let before_spacing = M.stringify M.FRET.pp_before (fret_i, note) in
      let style = { none with foreground = Some struc.color } in
      WRITE.writerate struc style before_spacing;
      before_spacing
    in
    let after_note (struc : Types.pattern_view_draw_struc) fret_i =
      let after_spacing = M.stringify M.FRET.pp_after (fret_i, note) in
      let style = { none with foreground = Some struc.color } in
      WRITE.writerate struc style after_spacing;
      after_spacing
    in
    let update_cursor (struc : Types.pattern_view_draw_struc) cell =
      let length = String.length cell in
      struc.cursor_i := !(struc.cursor_i) + length
    in
    update_cursor struc @@ before_note struc !fret_i;
    update_cursor struc @@ write_note struc !fret_j !fret_i;
    update_cursor struc @@ after_note struc !fret_i

  let write_fret_number (struc : Types.pattern_view_draw_struc) fret_i =
    let module M = Pp.FRETBOARD.FMT in
    let open LTerm_style in
    let fret_nb_string = M.stringify M.FRET.NUMBERS.pp_fret_nb_fmt fret_i in
    let style = { none with bold = Some true; foreground = Some lwhite } in
    WRITE.writerate_for_frets struc style fret_nb_string;
    fret_nb_string

  let interline (struc : Types.pattern_view_draw_struc) fret_j fret_i =
    let open LTerm_style in
    let module M = Pp.FRETBOARD.FMT in
    let write_box (struc : Types.pattern_view_draw_struc) _fret_j fret_i =
      let after_spacing = M.stringify M.INTERLINE.pp_box fret_i in
      let style = { none with foreground = Some struc.color } in
      WRITE.writerate_for_interlines struc style after_spacing;
      after_spacing
    in
    let update_cursor (struc : Types.pattern_view_draw_struc) cell =
      let length = String.length cell in
      struc.cursor_i := !(struc.cursor_i) + length
    in
    update_cursor struc @@ write_box struc !fret_j !fret_i

  let fret_numbers (struc : Types.pattern_view_draw_struc) =
    let open LTerm_style in
    let module M = Pp.FRETBOARD.FMT in
    let write_before_fret_number (struc : Types.pattern_view_draw_struc) fret_i
        =
      let before_spacing = M.stringify M.FRET.NUMBERS.pp_before fret_i in
      let style = { none with foreground = Some struc.color } in
      WRITE.writerate_for_frets struc style before_spacing;
      before_spacing
    in
    let write_after_fret_number (struc : Types.pattern_view_draw_struc) fret_i =
      let after_spacing = M.stringify M.FRET.NUMBERS.pp_after fret_i in
      let style = { none with foreground = Some struc.color } in
      WRITE.writerate_for_frets struc style after_spacing;
      after_spacing
    in
    let update_cursor (struc : Types.pattern_view_draw_struc) cell =
      let length = String.length cell in
      struc.cursor_i := !(struc.cursor_i) + length
    in
    let i = ref 0 in
    while !i < struc.number_of_frets do
      update_cursor struc @@ write_before_fret_number struc !i;
      update_cursor struc @@ write_fret_number struc !i;
      update_cursor struc @@ write_after_fret_number struc !i;
      i := !i + 1
    done

  (* main view draw function*)
  let view (struc : Types.pattern_view_draw_struc) =
    let update_field field i = field := i in
    let move_cursor_j (struc : Types.pattern_view_draw_struc) =
      update_field struc.cursor_j (!(struc.cursor_j) + 1)
    in
    let j = ref 0 in
    let i = ref 0 in
    fret_numbers struc;
    while !j < struc.number_of_strings do
      (* Iterate over frets and print interline *)
      i := 0;
      update_field struc.cursor_i 0;
      while !i < struc.number_of_frets do
        interline struc j i;
        i := !i + 1
      done;
      move_cursor_j struc;

      (* Iterate over frets and print them in fret boxes *)
      i := 0;
      update_field struc.cursor_i 0;
      while !i < struc.number_of_frets do
        fretted_note struc j i;
        i := !i + 1
      done;
      move_cursor_j struc;
      j := !j + 1
    done;

    (* Iterate over frets and print interline *)
    i := 0;
    update_field struc.cursor_i 0;
    while !i < struc.number_of_frets do
      interline struc j i;
      i := !i + 1
    done
end
