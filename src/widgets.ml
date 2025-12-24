(** Widget library for interactive panels - simplified *)

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

        (* Root note and scale *)
        let root_str = Pp.NOTES.FMT.sprint_note ocamuse_ctx.root_note in
        let scale_str = Display.scale_name ocamuse_ctx.scale in
        draw_line (Fmt.str "Key: %s %s" root_str scale_str)
          { none with foreground = Some lyellow };

        incr row;

        (* Highlight source - simplified *)
        let highlight_str = match ocamuse_ctx.highlight_source with
          | Types.Tonality (_, _) -> "Highlighting: Tonality"
          | Types.Chord (_, _) -> "Highlighting: Chord"
          | Types.Arpeggio (_, _) -> "Highlighting: Arpeggio"
        in
        draw_line highlight_str { none with foreground = Some lcyan };

        (* Color theme *)
        let theme_str = Fmt.str "Color Theme: %s" (Color_theme.theme_name ocamuse_ctx.color_theme) in
        draw_line theme_str { none with foreground = Some lmagenta };

        (* Diatonic chords - only for 7-note scales *)
        incr row;
        let diatonic = Display.diatonic_triads ocamuse_ctx.scale ocamuse_ctx.root_note in
        if List.length diatonic > 0 then begin
          draw_line "Diatonic Chords:" { none with bold = Some true; foreground = Some lcyan };
          let chord_strs = List.map (fun (note, triad, _chord, degree) ->
            let roman = Display.degree_to_roman degree triad in
            let note_str = Pp.NOTES.FMT.sprint_note note in
            Fmt.str "%s:%s" roman note_str
          ) diatonic in
          let line1 = String.concat " " (List.filteri (fun i _ -> i < 4) chord_strs) in
          let line2 = String.concat " " (List.filteri (fun i _ -> i >= 4) chord_strs) in
          draw_line line1 { none with foreground = Some lgreen };
          if String.length line2 > 0 then
            draw_line line2 { none with foreground = Some lgreen }
        end;

        (* Display mode indicator *)
        incr row;
        let display_str = match !(ocamuse_ctx.display_mode) with
          | Types.Flat (Types.Plain _) -> "View: Plain"
          | Types.Flat (Types.Fretted _) -> "View: Fretted"
          | Types.Flat (Types.Interline _) -> "View: Interline"
          | Types.Pattern (Types.Plain _, s) ->
              Fmt.str "View: Pattern (Plain, %a)" Pp.OCAMUSE.pp_scale_type s
          | Types.Pattern (Types.Fretted _, s) ->
              Fmt.str "View: Pattern (Fretted, %a)" Pp.OCAMUSE.pp_scale_type s
          | Types.Pattern (Types.Interline _, s) ->
              Fmt.str "View: Pattern (Interline, %a)" Pp.OCAMUSE.pp_scale_type s
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
        | Key { code = Escape; _ } ->
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
      draw_line "  v - Toggle Harmony View";
      draw_line "  r - Reset highlighting";
      draw_line "  Esc - Exit/Cancel";
      vbox#add (new LTerm_widget.hline);

      (* Navigation *)
      draw_line "Navigation:";
      draw_line "  PgUp/Dn - Change color";
      draw_line "  Enter - Cycle view mode";
      vbox#add (new LTerm_widget.hline);

      (* Mode selection *)
      draw_line "Modes:";
      draw_line "  t - Change tonality";
      draw_line "  u - Change tuning";
      draw_line "  m - Cycle scales";
      draw_line "  c - Select chord";
      draw_line "  k - Select color theme";
      draw_line "  1-7 - Highlight diatonic chord";
      draw_line "  d - Debug borders";
      vbox#add (new LTerm_widget.hline);

      draw_line "Press ? or h or Esc to close"
  end
