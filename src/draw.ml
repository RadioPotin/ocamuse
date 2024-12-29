module PATTERN = struct
  module WRITE = struct
    let writerate (struc : Types.pattern_view_draw_struc) style string =
      String.iteri
        (fun i c ->
            LTerm_draw.draw_char struc.ctx
              (!(struc.cursor_j) + !(struc.offset))
              (!(struc.cursor_i) + i + !(struc.offset))
              (Zed_char.unsafe_of_char c)
              ~style )
        string

    let writerate_for_frets (struc : Types.pattern_view_draw_struc) style string =
      String.iteri
        (fun i c ->
            let j = !(struc.cursor_j) + !(struc.offset) in
            let off = !(struc.cursor_i) + !(struc.offset) in
            LTerm_draw.draw_char struc.ctx (j - 1)
              (i + off - 1)
              (Zed_char.unsafe_of_char c)
              ~style )
        string

    let writerate_for_interlines (struc : Types.pattern_view_draw_struc) style
      string =
      String.iteri
        (fun i c ->
            let j = !(struc.cursor_j) + !(struc.offset) in
            let off = !(struc.cursor_i) + !(struc.offset) + i in
            LTerm_draw.draw_char struc.ctx j off (Zed_char.unsafe_of_char c) ~style )
        string
    let writerate_for_frets_with_padding (struc : Types.pattern_view_draw_struc) style string =
      String.iteri
        (fun i c ->
            let j = !(struc.cursor_j) + !(struc.offset) in
            let off = !(struc.cursor_i) + !(struc.offset) in
            LTerm_draw.draw_char struc.ctx (j - 2)
              (i + off - 1)
              (Zed_char.unsafe_of_char c)
              ~style )
        string

    let writerate_for_padding (struc : Types.pattern_view_draw_struc) style string =
      String.iteri
        (fun i c ->
            let j = !(struc.cursor_j) + !(struc.offset) in
            let off = !(struc.cursor_i) + !(struc.offset) in
            LTerm_draw.draw_char struc.ctx (j - 1)
              (i + off - 1)
              (Zed_char.unsafe_of_char c)
              ~style )
        string
  end

  let write_note (struc : Types.pattern_view_draw_struc) j i =
    let open LTerm_style in
    let note = struc.fretboard.(j).(i) in
    let note_string = Pp.FRETBOARD.FMT.stringify Pp.NOTES.FMT.print_note note in
    let bold = Theory.is_diatonic struc note in
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

  module PLAIN = struct

    let write_plain_note (struc : Types.pattern_view_draw_struc) fret_j fret_i =
      let pp_before_spacing (_struc : Types.pattern_view_draw_struc) _j i =
        let module M = Pp.FRETBOARD.FMT in
        let spacing =
          if i = 0 then begin
            (M.stringify M.PLAIN.pp_fret ())
          end
          else ""
        in
        WRITE.writerate struc
          LTerm_style.{ none with foreground = Some struc.color }
          spacing;
        spacing
      in
      let pp_after_spacing (struc : Types.pattern_view_draw_struc) j i =
        let module M = Pp.FRETBOARD.FMT in
        let note = struc.fretboard.(j).(i) in
        let note_int = Conv.note_to_int note in
        let spacing =
          if i = 0 then begin
            (M.stringify M.PLAIN.pp_fret ())
          end
          else begin
            match note_int with
            | 1 | 4 | 6 | 9 | 11 -> M.stringify M.PLAIN.pp_fret ()
            | _ ->
              Fmt.str "%s%s"
                (M.stringify M.PLAIN.pp_space ())
                (M.stringify M.PLAIN.pp_fret ())
          end
        in
        WRITE.writerate struc
          LTerm_style.{ none with foreground = Some struc.color }
          spacing;
        spacing
      in
      let update_cursor (struc : Types.pattern_view_draw_struc) cell =
        let length = String.length cell in
        struc.cursor_i := !(struc.cursor_i) + length
      in
      update_cursor struc @@ pp_before_spacing struc !fret_j !fret_i;
      update_cursor struc @@ write_note struc !fret_j !fret_i;
      update_cursor struc @@ pp_after_spacing struc !fret_j !fret_i

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
          incr i
        done;
        incr j
      done
  end

  module FRETTED = struct

    let fret_numbers_padding (struc : Types.pattern_view_draw_struc) =
      let module M = Pp.FRETBOARD.FMT in
      let write_fret_number_padding (struc : Types.pattern_view_draw_struc) fret_i =
        let open LTerm_style in
        let fret_nb_spacing_string = M.stringify M.FRET.NUMBERS.pp_fret_nb_padding_fmt fret_i in
        let style = { none with bold = Some true; foreground = Some lwhite } in
        WRITE.writerate_for_padding struc style fret_nb_spacing_string;
        fret_nb_spacing_string
      in
      let update_cursor (struc : Types.pattern_view_draw_struc) cell =
        let length = String.length cell in
        struc.cursor_i := !(struc.cursor_i) + length
      in
      let i = ref 0 in
      while !i < struc.number_of_frets do
        update_cursor struc @@ write_fret_number_padding struc !i;
        i := !i + 1
      done

    let padded_fret_numbers (struc : Types.pattern_view_draw_struc) =
      let open LTerm_style in
      let module M = Pp.FRETBOARD.FMT in
      let write_before_fret_number (struc : Types.pattern_view_draw_struc) fret_i
        =
        let before_spacing = M.stringify M.FRET.NUMBERS.pp_before fret_i in
        let style = { none with foreground = Some struc.color } in
        WRITE.writerate_for_frets_with_padding struc style before_spacing;
        before_spacing
      in
      let write_fret_number (struc : Types.pattern_view_draw_struc) fret_i =
        let module M = Pp.FRETBOARD.FMT in
        let open LTerm_style in
        let fret_nb_string = M.stringify M.FRET.NUMBERS.pp_fret_nb_fmt fret_i in
        let style = { none with bold = Some true; foreground = Some lwhite } in
        WRITE.writerate_for_frets_with_padding struc style fret_nb_string;
        fret_nb_string
      in
      let write_after_fret_number (struc : Types.pattern_view_draw_struc) fret_i =
        let after_spacing = M.stringify M.FRET.NUMBERS.pp_after fret_i in
        let style = { none with foreground = Some struc.color } in
        WRITE.writerate_for_frets_with_padding struc style after_spacing;
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

    let view (struc : Types.pattern_view_draw_struc) =
      let update_field field i = field := i in
      let j = ref 0 in
      let i = ref 0 in
      padded_fret_numbers struc;
      fret_numbers_padding struc;
      while !j < struc.number_of_strings do
        i := 0;
        update_field struc.cursor_j !j;
        update_field struc.cursor_i !i;
        while !i < struc.number_of_frets do
          fretted_note struc j i;
          i := !i + 1
        done;
        j := !j + 1
      done
  end


  module INTERLINE = struct

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

    let view (struc : Types.pattern_view_draw_struc) =
      let update_field field i = field := i in
      let move_cursor_j (struc : Types.pattern_view_draw_struc) =
        update_field struc.cursor_j (!(struc.cursor_j) + 1)
      in
      let j = ref 0 in
      let i = ref 0 in
      fret_numbers struc;
      while !j < struc.number_of_strings do
        i := 0;
        update_field struc.cursor_i 0;
        while !i < struc.number_of_frets do
          interline struc j i;
          i := !i + 1
        done;
        move_cursor_j struc;

        i := 0;
        update_field struc.cursor_i 0;
        while !i < struc.number_of_frets do
          fretted_note struc j i;
          i := !i + 1
        done;
        move_cursor_j struc;
        j := !j + 1
      done;
      i := 0;
      update_field struc.cursor_i 0;
      while !i < struc.number_of_frets do
        interline struc j i;
        i := !i + 1
      done
  end

  let pattern (struc : Types.pattern_view_draw_struc) =
    match struc.view with
    | Plain _ -> PLAIN.view struc
    | Fretted _ -> FRETTED.view struc
    | Interline _ -> INTERLINE.view struc
end
