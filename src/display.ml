module MATRIX = struct
  open Types
  module DRAW = struct
    module LINE = struct
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

      (* ***************************** *)
      (* main current writing function *)
      (* ***************************** *)
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

      let write_plain_rows ctx fretboard color =
        let struc = make_struc ctx fretboard color in
        let update_field field i = field := i in
        let i = ref 0 in
        while !i < struc.number_of_strings do
          update_field struc.string !i;
          update_field struc.guitar_string struc.fretboard.(!i) ;
          update_field struc.cursor_j (!(struc.cursor_j) + 1);
          write_plain_frets struc;
          i := !i + 1
        done

      let write_rows_with_interlines ctx fretboard color =
        let struc = make_struc ctx fretboard color in
        let update_field field i = field := i in
        let move_cursor (struc : Types.flat_view_draw_struc) =
          update_field struc.cursor_j (!(struc.cursor_j) + 1)
        in
        (* top fret numbers *)
        write_fret_numbers struc;
        move_cursor struc;
        (* interline *)
        write_interline struc;
        move_cursor struc;
        (* iterate over rest of strings *)
        write_frets struc;
        move_cursor struc;
        write_interline struc;
        move_cursor struc;
        let i = ref 1 in
        while !i < struc.number_of_strings do
          update_field struc.guitar_string struc.fretboard.(!i);
          (* frets and notes *)
          write_frets struc;
          move_cursor struc;
          update_field struc.string !i;
          (* interline *)
          write_interline struc;
          move_cursor struc;
          incr i
        done

      let write_rows_with_no_interline ctx fretboard color =
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
    end

    module CELL = struct

      (* This module will hold all functions that aim to write a PP fret directly to the keyboard according to view *)

    (*
+     For now, the deal is to implement displays similar to the ones provided
+     by the Plain, Fretted and Interline view modes.
+
+     Instead of [LTerm_draw.draw_styled], we would use the [LTerm_draw.draw_char] to achieve that.
+
+     One way to go would be to print each fretboard view once and save
+     the context-world coordinates of each important note to a Hashtbl with
+     their relative role (Root, Tonic, Major 3rd, etc) for the given type of
+     Pattern then mapping a given role, in the context to a specific color to
+     go style directly in the context.
+
+     We could then later aim at making the color selection customisable that way
+   *)

      let writerate struc style string =
        String.iteri (fun i c ->
          LTerm_draw.draw_char struc.ctx
            (!(struc.cursor_j) + !(struc.offset))
            (!(struc.cursor_i) + i + !(struc.offset))
            (Zed_char.unsafe_of_char c)
            ~style
        ) string

      let write_note (struc : Types.pattern_view_draw_struc) j i =
        let open Types in
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
        writerate struc style note_string ; note_string

      let write_plane_spacing (struc : Types.pattern_view_draw_struc) j i =
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

      let write_plain_note (struc : Types.pattern_view_draw_struc) fret_j fret_i  =
        let open Types in
        let update_cursor struc cell =
          let length = String.length cell in
          struc.cursor_i := (!(struc.cursor_i) + length)
        in
        update_cursor struc @@ write_note struc !fret_j !fret_i;
        update_cursor struc @@ write_plane_spacing struc !fret_j !fret_i
    end

    let write_pattern (struc : Types.pattern_view_draw_struc) =
      let update_field field i = field := i in
      match struc.view with
      | Plain _ ->
        let j = ref 0 in
        let i = ref 0 in
        while !j < struc.number_of_strings do
          i := 0;
          update_field struc.cursor_j !j;
          update_field struc.cursor_i !i;
          while !i < struc.number_of_frets do
            CELL.write_plain_note struc j i;
            i := !i + 1
          done;
          j := !j + 1
        done;
      | _ -> assert false


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
        write_pattern struc
      end

  end
end
