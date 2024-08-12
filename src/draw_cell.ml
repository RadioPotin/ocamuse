let writerate (struc: Types.pattern_view_draw_struc) style string =
  String.iteri (fun i c ->
    LTerm_draw.draw_char struc.ctx
      (!(struc.cursor_j) + !(struc.offset))
      (!(struc.cursor_i) + i + !(struc.offset))
      (Zed_char.unsafe_of_char c)
      ~style
  ) string

let writerate_for_frets (struc: Types.pattern_view_draw_struc) style string =
  String.iteri (fun i c ->
    let j = !(struc.cursor_j) + !(struc.offset) in
    let off = !(struc.cursor_i) + !(struc.offset) in
    LTerm_draw.draw_char struc.ctx
      (j - 1)
      ((i + off) - 1)
      (Zed_char.unsafe_of_char c)
      ~style
  ) string

let write_note (struc : Types.pattern_view_draw_struc) j i =
  let open LTerm_style in
  let note = struc.fretboard.(j).(i) in
  let note_string =
    Pp.FRETBOARD.FMT.stringify
      Pp.NOTES.FMT.print_note
      note
  in
  let bold = Ocamuse.is_diatonic struc note in
  let note_color = Pp.COLOR.find_color struc note in
  let style =
    {
      none with
      bold = Some bold;
      foreground = Some (note_color)
    }
  in
  writerate struc style note_string;
  note_string

let write_plain_note (struc : Types.pattern_view_draw_struc) fret_j fret_i  =

  let write_plain_spacing (struc : Types.pattern_view_draw_struc) j i =
    let should_space =
      Fretboard.scan_column_for_alterations (i, j)
    in
    let module M = Pp.FRETBOARD.FMT in
    let note = struc.fretboard.(j).(i) in
    let note_int = Conv.note_to_int note in
    let spacing =
      if i = 0 then begin
        match note_int with
        | 1 | 4 | 6 | 9 | 11 ->
          M.stringify M.PLAIN.pp_fret ()
        | _ ->
          M.stringify M.PLAIN.pp_space () ^
            M.stringify M.PLAIN.pp_fret ()
      end
      else if i < 10 then begin
        match note_int with
        | 1 | 4 | 6 | 9 | 11 ->
          M.stringify M.PLAIN.pp_sep ()
        | _ ->
          if should_space then
            M.stringify M.PLAIN.pp_sep () ^
              M.stringify M.PLAIN.pp_sep ()
          else
            M.stringify M.PLAIN.pp_sep ()
      end
      else begin
        match note_int with
        | 1 | 4 | 6 | 9 | 11 ->
          M.stringify M.PLAIN.pp_fret ()
        | _ ->
          M.stringify M.PLAIN.pp_sep ()
      end
    in
    writerate struc (LTerm_style.({
      none with
      foreground = Some struc.color
    })) spacing;
    spacing
  in

  let update_cursor (struc: Types.pattern_view_draw_struc) cell =
    let length = String.length cell in
    struc.cursor_i := (!(struc.cursor_i) + length)
  in
  update_cursor struc @@ write_note struc !fret_j !fret_i;
  update_cursor struc @@ write_plain_spacing struc !fret_j !fret_i

let write_fret_number (struc : Types.pattern_view_draw_struc) fret_i =
  let module M = Pp.FRETBOARD.FMT in
  let open LTerm_style in
  let fret_nb_string =
    M.stringify M.FRET.NUMBERS.pp_fret_nb_fmt fret_i
  in
  let style =
    {
      none with
      bold = Some true;
      foreground = Some (struc.color)
    }
  in
  writerate_for_frets struc style fret_nb_string;
  fret_nb_string

let write_fret_numbers (struc : Types.pattern_view_draw_struc) =
  let open LTerm_style in
  let module M = Pp.FRETBOARD.FMT in
  let write_before_fret_number (struc: Types.pattern_view_draw_struc) fret_i =
    let before_spacing =
      M.stringify M.FRET.NUMBERS.pp_before fret_i
    in
    let style =
      {
        none with
        foreground = Some (struc.color)
      }
    in
    writerate_for_frets struc style before_spacing;
    before_spacing
  in

  let write_after_fret_number (struc: Types.pattern_view_draw_struc) fret_i =
    let after_spacing =
      M.stringify M.FRET.NUMBERS.pp_after fret_i
    in
    let style =
      {
        none with
        foreground = Some (struc.color)
      }
    in
    writerate_for_frets struc style after_spacing;
    after_spacing
  in

  let update_cursor (struc: Types.pattern_view_draw_struc) cell =
    let length = String.length cell in
    struc.cursor_i := (!(struc.cursor_i) + length)
  in
  let i = ref 0 in
  while !i < struc.number_of_frets do
    update_cursor struc @@ write_before_fret_number struc !i;
    update_cursor struc @@ write_fret_number struc !i;
    update_cursor struc @@ write_after_fret_number struc !i;
    i := !i + 1;
  done


module PLAIN = struct
  let view (struc : Types.pattern_view_draw_struc) =
    let update_field field i = field := i in
    let j = ref 0 in
    let i = ref 0 in
    while !j < struc.number_of_strings do
      i := 0;
      update_field struc.cursor_j !j;
      update_field struc.cursor_i !i;
      while !i < struc.number_of_frets do
        write_plain_note struc j i;
        i := !i + 1
      done;
      j := !j + 1
    done
end

let write_fretted_note (struc : Types.pattern_view_draw_struc) fret_j fret_i  =
  let open LTerm_style in
  let module M = Pp.FRETBOARD.FMT in
  let note = struc.fretboard.(!fret_j).(!fret_i) in
  let before_note (struc: Types.pattern_view_draw_struc) fret_i =
    let before_spacing =
      M.stringify M.FRET.pp_before (fret_i, note)
    in
    let style =
      {
        none with
        foreground = Some (struc.color)
      }
    in
    writerate struc style before_spacing;
    before_spacing

  in
  let after_note (struc: Types.pattern_view_draw_struc) fret_i =
    let after_spacing =
      M.stringify M.FRET.pp_after (fret_i, note)
    in
    let style =
      {
        none with
        foreground = Some (struc.color)
      }
    in
    writerate struc style after_spacing;
    after_spacing
  in
  let update_cursor (struc: Types.pattern_view_draw_struc) cell =
    let length = String.length cell in
    struc.cursor_i := (!(struc.cursor_i) + length)
  in
  update_cursor struc @@ before_note struc !fret_i;
  update_cursor struc @@ write_note struc !fret_j !fret_i;
  update_cursor struc @@ after_note struc !fret_i

module FRETTED = struct
  let view (struc : Types.pattern_view_draw_struc) =
    let update_field field i = field := i in
    let j = ref 0 in
    let i = ref 0 in
    write_fret_numbers struc;
    while !j < struc.number_of_strings do
      i := 0;
      update_field struc.cursor_j !j;
      update_field struc.cursor_i !i;
      while !i < struc.number_of_frets do
        write_fretted_note struc j i;
        i := !i + 1
      done;
      j := !j + 1
    done
end

let write_interline (struc : Types.pattern_view_draw_struc) fret_j fret_i  =
  let open LTerm_style in
  let module M = Pp.FRETBOARD.FMT in
  let write_box (struc: Types.pattern_view_draw_struc) _fret_j fret_i =
    let after_spacing =
      M.stringify M.INTERLINE.pp_box fret_i
    in
    let style =
      {
        none with
        foreground = Some (struc.color)
      }
    in
    writerate struc style after_spacing;
    after_spacing
  in
  let update_cursor (struc: Types.pattern_view_draw_struc) cell =
    let length = String.length cell in
    struc.cursor_i := (!(struc.cursor_i) + length)
  in
  let i = ref 0 in
  while !i < struc.number_of_frets do
    update_cursor struc @@ write_box struc !fret_j !fret_i;
    i := !i + 1
  done

module INTERLINE = struct
  let view (struc : Types.pattern_view_draw_struc) =
    let update_field field i = field := i in
    let j = ref 0 in
    let i = ref 0 in
    write_fret_numbers struc;
    while !j < struc.number_of_strings do
      i := 0;
      update_field struc.cursor_j !j;
      update_field struc.cursor_i !i;
      while !i < struc.number_of_frets do
        write_fretted_note struc j i;
        i := !i + 1
      done;
      write_interline struc j i;
      j := !j + 1
    done
end

let write_pattern (struc : Types.pattern_view_draw_struc) =
  match struc.view with
  | Plain _ -> PLAIN.view struc
  | Fretted _ -> FRETTED.view struc
  | Interline _ -> INTERLINE.view struc
