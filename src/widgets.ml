(** Widget library for interactive panels *)

(** Helper to draw a string using LTerm *)
let draw_text_line ctx row_pos col_pos text style =
  let open LTerm_geom in
  let { cols; _ } = LTerm_draw.size ctx in
  let available = max 0 (cols - col_pos) in
  let len = min (String.length text) available in
  for i = 0 to len - 1 do
    LTerm_draw.draw_char ctx row_pos (col_pos + i)
      (Zed_char.unsafe_of_char text.[i]) ~style
  done

(** Context information panel - displays current musical context *)
class context_panel (ocamuse_ctx : Types.ocamuse_structure) (app_state : App_state.t) =
  object
    inherit LTerm_widget.t "context_panel"

    method! draw ctx _focused =
      let open LTerm_draw in
      let open LTerm_style in
      let open LTerm_geom in
      clear ctx;

      (* Debug mode: draw border *)
      if !(app_state.debug_mode) then begin
        let sz = size ctx in
        draw_frame ctx { row1 = 0; col1 = 0; row2 = sz.rows - 1; col2 = sz.cols - 1 }
          ~style:{ none with foreground = Some lred } Light
      end;

      let { rows; cols } = size ctx in
      if rows < 3 || cols < 20 then () (* Too small to render *)
      else
        let row = ref 0 in

        (* Helper to draw a line *)
        let draw_line text style =
          if !row < rows && cols > 2 then begin
            draw_text_line ctx !row 1 text style;
            incr row
          end
        in

        (* Title *)
        draw_line "CONTEXT" { none with bold = Some true; foreground = Some lcyan };
        incr row;

        (* Tuning *)
        let tuning_str =
          String.concat " "
            (List.map Pp.NOTES.FMT.sprint_note ocamuse_ctx.tuning)
        in
        draw_line (Fmt.str "Tuning: %s" tuning_str)
          { none with foreground = Some lgreen };

        (* Root note and mode *)
        let root_str = Pp.NOTES.FMT.sprint_note ocamuse_ctx.root_note in
        let mode_str = match ocamuse_ctx.mode with
          | Types.C_mode -> "Ionian (Major)"
          | Types.D_mode -> "Dorian"
          | Types.E_mode -> "Phrygian"
          | Types.F_mode -> "Lydian"
          | Types.G_mode -> "Mixolydian"
          | Types.A_mode -> "Aeolian (Minor)"
          | Types.B_mode -> "Locrian"
        in
        draw_line (Fmt.str "Key: %s %s" root_str mode_str)
          { none with foreground = Some lyellow };

        (* Tonality *)
        let tonality = Theory.build_tonality ocamuse_ctx.mode ocamuse_ctx.root_note in
        let tonality_str =
          String.concat " "
            (List.map (fun note -> Pp.NOTES.FMT.sprint_note note) tonality)
        in
        draw_line (Fmt.str "Scale: %s" tonality_str)
          { none with foreground = Some lwhite };

        incr row;

        (* Highlight source *)
        let highlight_str = match ocamuse_ctx.highlight_source with
          | Types.Tonality (_, _) -> "Highlighting: Tonality"
          | Types.Chord (root, chord_type) ->
              let chord_name = Theory.name_chord root chord_type in
              Fmt.str "Highlighting: Chord (%s)" chord_name
          | Types.Arpeggio (root, chord_type) ->
              let chord_name = Theory.name_chord root chord_type in
              Fmt.str "Highlighting: Arpeggio (%s)" chord_name
        in
        draw_line highlight_str { none with foreground = Some lcyan };

        (* Color theme *)
        let theme_str = Fmt.str "Color Theme: %s" (Color_theme.theme_name ocamuse_ctx.color_theme) in
        draw_line theme_str { none with foreground = Some lmagenta };

        incr row;

        (* Diatonic chords *)
        draw_line "Diatonic Chords:" { none with bold = Some true; foreground = Some lmagenta };
        let chords = Theory.build_diatonic_triads_sequence ocamuse_ctx.mode ocamuse_ctx.root_note in
        List.iteri (fun i (note, chord) ->
          let roman = match i with
            | 0 -> "I" | 1 -> "ii" | 2 -> "iii" | 3 -> "IV"
            | 4 -> "V" | 5 -> "vi" | 6 -> "vii°" | _ -> ""
          in
          let chord_name = Theory.name_chord note
            (match chord with
             | Types.Major -> Types.Major
             | Types.Minor -> Types.Minor
             | Types.Diminished -> Types.Dimin)
          in
          draw_line (Fmt.str "  %s: %s" roman chord_name)
            { none with foreground = Some white }
        ) chords;

        (* Display mode indicator *)
        incr row;
        let display_str = match !(ocamuse_ctx.display_mode) with
          | Types.Flat (Types.Plain _) -> "View: Plain"
          | Types.Flat (Types.Fretted _) -> "View: Fretted"
          | Types.Flat (Types.Interline _) -> "View: Interline"
          | Types.Pattern (Types.Plain _, m) ->
              Fmt.str "View: Pattern (Plain, %a)" Pp.OCAMUSE.pp_mode m
          | Types.Pattern (Types.Fretted _, m) ->
              Fmt.str "View: Pattern (Fretted, %a)" Pp.OCAMUSE.pp_mode m
          | Types.Pattern (Types.Interline _, m) ->
              Fmt.str "View: Pattern (Interline, %a)" Pp.OCAMUSE.pp_mode m
        in
        draw_line display_str { none with foreground = Some lblue }

    method! can_focus = false
  end

(** Help panel - displays quick help info *)
class help_panel (app_state : App_state.t) =
  object
    inherit LTerm_widget.t "help_panel"

    method! draw ctx _focused =
      let open LTerm_draw in
      let open LTerm_style in
      let open LTerm_geom in
      clear ctx;

      (* Debug mode: draw border *)
      if !(app_state.debug_mode) then begin
        let sz = size ctx in
        draw_frame ctx { row1 = 0; col1 = 0; row2 = sz.rows - 1; col2 = sz.cols - 1 }
          ~style:{ none with foreground = Some lgreen } Light
      end;

      let { rows; cols } = size ctx in
      if rows < 3 || cols < 30 then ()
      else
        let row = ref 0 in

        let draw_line text style =
          if !row < rows && cols > 2 then begin
            draw_text_line ctx !row 1 text style;
            incr row
          end
        in

        (* Title *)
        draw_line "HELP" { none with bold = Some true; foreground = Some lcyan };
        incr row;

        (* Show current mode *)
        let mode_str = App_state.mode_name app_state in
        draw_line (Fmt.str "Current Mode: %s" mode_str)
          { none with bold = Some true; foreground = Some lgreen };
        incr row;

        (* Quick help message *)
        draw_line "Press '?' or 'h' to view all keybindings"
          { none with foreground = Some lyellow };
        incr row;

        (* Essential shortcuts only *)
        draw_line "Essential:" { none with bold = Some true; foreground = Some lmagenta };
        draw_line "  ?/h        - Keybindings modal" { none with foreground = Some white };
        draw_line "  r          - Reset highlighting" { none with foreground = Some white };
        draw_line "  Escape     - Exit/Cancel" { none with foreground = Some lred }

    method! can_focus = false
  end

(** Keybindings modal - displays all keyboard shortcuts in a centered modal *)
class keybindings_modal pop_layer_fn =
  object (self)
    inherit LTerm_widget.modal_frame

    initializer
      let vbox = new LTerm_widget.vbox in
      self#set vbox;

      (* Handle ? and h to close modal *)
      self#on_event (fun event ->
        let open LTerm_event in
        match event with
        | Key { code = Char c; _ } when Uchar.to_int c = Char.code '?' || Uchar.to_int c = Char.code 'h' ->
          pop_layer_fn ();
          true
        | _ -> false
      );

      let draw_line text =
        let label = new LTerm_widget.label text in
        vbox#add label
      in

      (* Title *)
      draw_line "=== KEYBOARD SHORTCUTS ===";
      vbox#add (new LTerm_widget.hline);

      (* Global shortcuts *)
      draw_line "Global:";
      draw_line "  ?/h - Toggle this modal";
      draw_line "  r - Reset highlighting";
      draw_line "  Esc - Exit/Cancel";
      vbox#add (new LTerm_widget.hline);

      (* Navigation *)
      draw_line "Navigation:";
      draw_line "  PgUp/Dn - Change color";
      draw_line "  Enter - Cycle view";
      draw_line "  Tab - Cycle focus";
      vbox#add (new LTerm_widget.hline);

      (* Mode selection *)
      draw_line "Modes:";
      draw_line "  t - Change tonality";
      draw_line "  u - Change tuning";
      draw_line "  c - Chord diagrams";
      draw_line "  a - Arpeggio mode";
      draw_line "  m - Cycle modes";
      draw_line "  k - Color theme";
      draw_line "  d - Debug borders";
      vbox#add (new LTerm_widget.hline);

      (* Mode-specific *)
      draw_line "Chord Mode:";
      draw_line "  1-7 - Select chord";
      draw_line "  i - Inversion";
      draw_line "  v - Voicing";
      vbox#add (new LTerm_widget.hline);

      draw_line "Arpeggio Mode:";
      draw_line "  1-7 - Select chord";
      vbox#add (new LTerm_widget.hline);

      draw_line "Press ? or h to close"
  end


(** Simple status line widget *)
class status_line message =
  object
    inherit LTerm_widget.t "status_line"

    method! draw ctx _focused =
      let open LTerm_style in
      draw_text_line ctx 0 0 message
        { none with foreground = Some lwhite; background = Some lblack }

    method! can_focus = false
  end
