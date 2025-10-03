(** Interactive selector modes for tonality and tuning *)

(** Helper to draw text - reuse from widgets *)
let draw_text_line ctx row_pos col_pos text style =
  let open LTerm_geom in
  let { cols; _ } = LTerm_draw.size ctx in
  let available = max 0 (cols - col_pos) in
  let len = min (String.length text) available in
  for i = 0 to len - 1 do
    LTerm_draw.draw_char ctx row_pos (col_pos + i)
      (Zed_char.unsafe_of_char text.[i]) ~style
  done

(** Handle tonality selection input *)
let handle_tonality_input state event =
  let open LTerm_event in
  let open Types in
  match !(state.App_state.mode) with
  | App_state.TonalitySelection tstate -> begin
    match event with
    | Key { code = Escape; _ } ->
      App_state.return_to_normal state;
      true
    | Key { code = Char c; _ } ->
      (* Convert Uchar to char - get first byte for ASCII *)
      let ch_code = Uchar.to_int c in
      if ch_code > 127 then false (* Non-ASCII, ignore *)
      else begin
      let ch = Char.chr ch_code in
      (* Handle character input based on which field is currently active *)
      (* Root field accepts a-g *)
      if (tstate.selection_step = App_state.SelectingRoot ||
          tstate.selection_step = App_state.SelectingAlteration) &&
         ((ch >= 'a' && ch <= 'g') || (ch >= 'A' && ch <= 'G')) then begin
        match Conv.base_of_char ch with
        | Ok base ->
          let new_state =
            { tstate with
              temp_root = Some { base; alteration = 0 };
              selection_step = App_state.SelectingAlteration  (* Advance to alteration step *)
            }
          in
          state.mode := App_state.TonalitySelection new_state;
          true
        | Error _ -> false
      end
      (* Alteration accepts # and b *)
      else if tstate.selection_step = App_state.SelectingAlteration && ch = '#' then begin
        match tstate.temp_root with
        | Some note ->
          let new_state =
            { tstate with temp_root = Some { note with alteration = note.alteration + 1 } }
          in
          state.mode := App_state.TonalitySelection new_state;
          true
        | None -> false
      end
      else if tstate.selection_step = App_state.SelectingAlteration && ch = 'b' then begin
        match tstate.temp_root with
        | Some note ->
          let new_state =
            { tstate with temp_root = Some { note with alteration = note.alteration - 1 } }
          in
          state.mode := App_state.TonalitySelection new_state;
          true
        | None -> false
      end
      (* Mode field accepts 1-7 *)
      else if tstate.selection_step = App_state.SelectingMode && ch >= '1' && ch <= '7' then begin
        let mode_idx = Char.code ch - Char.code '1' in
        let mode = match mode_idx with
          | 0 -> C_mode | 1 -> D_mode | 2 -> E_mode | 3 -> F_mode
          | 4 -> G_mode | 5 -> A_mode | 6 -> B_mode | _ -> C_mode
        in
        let new_state = { tstate with temp_mode = Some mode } in
        state.mode := App_state.TonalitySelection new_state;
        true
      end
      else false
      end
    | Key { code = Enter; _ } -> begin
      match (tstate.temp_root, tstate.temp_mode) with
      | (Some root, Some mode) ->
        (* Both fields filled - apply the selection *)
        state.context.root_note <- root;
        state.context.mode <- mode;
        (* Update highlight source to new tonality *)
        state.context.highlight_source <- Types.Tonality (mode, root);
        (* Ensure we're in Pattern mode to see the highlighting *)
        let view = match !(state.context.display_mode) with
          | Types.Flat v -> v
          | Types.Pattern (v, _) -> v
        in
        state.context.display_mode := Types.Pattern (view, mode);
        App_state.return_to_normal state;
        true
      | (Some _root, None) ->
        (* Root is set but mode is not - advance to mode field *)
        let new_state = { tstate with selection_step = App_state.SelectingMode } in
        state.mode := App_state.TonalitySelection new_state;
        true
      | (None, _) ->
        (* Root not set - stay on root field *)
        false
    end
    | Key { code = Up; _ } -> begin
      (* Navigate backwards through selection steps *)
      let new_step = match tstate.selection_step with
        | App_state.SelectingMode -> App_state.SelectingAlteration
        | App_state.SelectingAlteration -> App_state.SelectingRoot
        | App_state.SelectingRoot -> App_state.SelectingRoot  (* Stay at first *)
      in
      let new_state = { tstate with selection_step = new_step } in
      state.mode := App_state.TonalitySelection new_state;
      true
    end
    | Key { code = Down; _ } -> begin
      (* Navigate forwards through selection steps *)
      let new_step = match tstate.selection_step with
        | App_state.SelectingRoot -> App_state.SelectingAlteration
        | App_state.SelectingAlteration ->
            (* Only advance to Mode if we have a root selected *)
            (match tstate.temp_root with
             | Some _ -> App_state.SelectingMode
             | None -> App_state.SelectingAlteration)
        | App_state.SelectingMode -> App_state.SelectingMode  (* Stay at last *)
      in
      let new_state = { tstate with selection_step = new_step } in
      state.mode := App_state.TonalitySelection new_state;
      true
    end
    | _ -> false
  end
  | _ -> false

(** Handle tuning selection input *)
let handle_tuning_input state event =
  let open LTerm_event in
  match !(state.App_state.mode) with
  | App_state.TuningSelection tstate -> begin
    match event with
    | Key { code = Escape; _ } ->
      App_state.return_to_normal state;
      true
    | Key { code = Char c; _ } ->
      let ch_code = Uchar.to_int c in
      if ch_code > 127 then false
      else begin
      let ch = Char.chr ch_code in
      if ch >= '1' && ch <= '9' then begin
      let idx = Char.code ch - Char.code '1' in
      if idx < List.length tstate.available_tunings then begin
        let new_state = { tstate with selected_index = idx } in
        state.mode := App_state.TuningSelection new_state;
        true
      end
      else false
      end
      else false
      end
    | Key { code = Up; _ } ->
      let new_idx =
        if tstate.selected_index > 0 then tstate.selected_index - 1
        else List.length tstate.available_tunings - 1
      in
      let new_state = { tstate with selected_index = new_idx } in
      state.mode := App_state.TuningSelection new_state;
      true
    | Key { code = Down; _ } ->
      let new_idx = (tstate.selected_index + 1) mod List.length tstate.available_tunings in
      let new_state = { tstate with selected_index = new_idx } in
      state.mode := App_state.TuningSelection new_state;
      true
    | Key { code = Enter; _ } ->
      let (_, selected_tuning) = List.nth tstate.available_tunings tstate.selected_index in
      (* Apply the tuning *)
      state.context.tuning <- selected_tuning;
      (* Rebuild fretboard with new tuning *)
      let new_fretboard =
        Fretboard.init ~tuning:selected_tuning ~range:Config.default_fret_range ()
      in
      state.context.fretboard <- new_fretboard;
      App_state.return_to_normal state;
      true
    | _ -> false
  end
  | _ -> false

(** Tuning selector widget - displays available tunings *)
class tuning_selector_widget (tstate : App_state.tuning_state) (app_state : App_state.t) =
  object
    inherit LTerm_widget.t "tuning_selector"

    method! draw ctx _focused =
      let open LTerm_draw in
      let open LTerm_style in
      let open LTerm_geom in
      clear ctx;

      (* Debug mode: draw border *)
      if !(app_state.debug_mode) then begin
        let sz = size ctx in
        draw_frame ctx { row1 = 0; col1 = 0; row2 = sz.rows - 1; col2 = sz.cols - 1 }
          ~style:{ none with foreground = Some lblue } Light
      end;

      let { rows; cols } = size ctx in
      let row = ref 0 in

      let draw_line text style =
        if !row < rows && cols > 2 then begin
          draw_text_line ctx !row 1 text style;
          incr row
        end
      in

      draw_line "SELECT TUNING" { none with bold = Some true; foreground = Some lcyan };
      incr row;

      List.iteri (fun i (name, tuning) ->
        let tuning_str =
          String.concat " "
            (List.map (fun note -> Pp.NOTES.FMT.sprint_note note) tuning)
        in
        let prefix = if i = tstate.selected_index then "> " else "  " in
        let style =
          if i = tstate.selected_index then
            { none with bold = Some true; foreground = Some lgreen }
          else
            { none with foreground = Some white }
        in
        draw_line (Fmt.str "%s%d. %s: %s" prefix (i + 1) name tuning_str) style
      ) tstate.available_tunings;

      incr row;
      draw_line "Press 1-6 or Up/Down to select, Enter to confirm" { none with foreground = Some lblue }

    method! can_focus = false
  end

(** Tonality selector widget - displays current selection state *)
class tonality_selector_widget (tstate : App_state.tonality_state) (app_state : App_state.t) =
  object
    inherit LTerm_widget.t "tonality_selector"

    method! draw ctx _focused =
      let open LTerm_draw in
      let open LTerm_style in
      let open LTerm_geom in
      clear ctx;

      (* Debug mode: draw border *)
      if !(app_state.debug_mode) then begin
        let sz = size ctx in
        draw_frame ctx { row1 = 0; col1 = 0; row2 = sz.rows - 1; col2 = sz.cols - 1 }
          ~style:{ none with foreground = Some lyellow } Light
      end;

      let { rows; cols } = size ctx in
      let row = ref 0 in

      let draw_line text style =
        if !row < rows && cols > 2 then begin
          draw_text_line ctx !row 1 text style;
          incr row
        end
      in

      draw_line "SELECT TONALITY" { none with bold = Some true; foreground = Some lcyan };
      incr row;

      (* Show current selection *)
      let root_str = match tstate.temp_root with
        | Some note -> Pp.NOTES.FMT.sprint_note note
        | None -> "..."
      in
      let mode_str = match tstate.temp_mode with
        | Some Types.C_mode -> "Ionian (Major)"
        | Some Types.D_mode -> "Dorian"
        | Some Types.E_mode -> "Phrygian"
        | Some Types.F_mode -> "Lydian"
        | Some Types.G_mode -> "Mixolydian"
        | Some Types.A_mode -> "Aeolian (Minor)"
        | Some Types.B_mode -> "Locrian"
        | None -> "..."
      in

      (* Highlight active field *)
      let root_style = match tstate.selection_step with
        | App_state.SelectingRoot | App_state.SelectingAlteration ->
            { none with bold = Some true; foreground = Some lgreen }
        | _ -> { none with foreground = Some white }
      in
      let mode_style = match tstate.selection_step with
        | App_state.SelectingMode ->
            { none with bold = Some true; foreground = Some lyellow }
        | _ -> { none with foreground = Some white }
      in
      draw_line (Fmt.str "Root: %s" root_str) root_style;
      draw_line (Fmt.str "Mode: %s" mode_str) mode_style;

      incr row;

      (* Show instructions based on step *)
      begin match tstate.selection_step with
      | App_state.SelectingRoot ->
        draw_line "> Press a-g to set root note" { none with foreground = Some lblue }
      | App_state.SelectingAlteration ->
        draw_line "> Press a-g (root), # (sharp), or b (flat)"
          { none with foreground = Some lblue }
      | App_state.SelectingMode ->
        draw_line "> Press 1-7 to set mode"
          { none with foreground = Some lblue };
        incr row;
        draw_line "  1:Ionian 2:Dorian 3:Phrygian 4:Lydian"
          { none with foreground = Some white };
        draw_line "  5:Mixolydian 6:Aeolian 7:Locrian"
          { none with foreground = Some white }
      end;
      incr row;
      draw_line "Use Up/Down to navigate, Enter to confirm" { none with foreground = Some lcyan }

    method! can_focus = false
  end

(** Chord mode widget - displays chord diagram *)
class chord_widget (cstate : App_state.chord_state) (app_state : App_state.t) (_context : Types.ocamuse_structure) =
  object
    inherit LTerm_widget.t "chord_widget"

    method! draw ctx _focused =
      let open LTerm_draw in
      let open LTerm_style in
      let open LTerm_geom in
      clear ctx;

      (* Debug mode: draw border *)
      if !(app_state.debug_mode) then begin
        let sz = size ctx in
        draw_frame ctx { row1 = 0; col1 = 0; row2 = sz.rows - 1; col2 = sz.cols - 1 }
          ~style:{ none with foreground = Some lmagenta } Light
      end;

      let { rows; cols } = size ctx in
      let row = ref 0 in

      let draw_line text style =
        if !row < rows && cols > 2 then begin
          draw_text_line ctx !row 1 text style;
          incr row
        end
      in

      draw_line "CHORD MODE" { none with bold = Some true; foreground = Some lcyan };
      incr row;

      (* Show controls on two lines *)
      draw_line "Controls: 1-7=select | i=inversion | v=voicing"
        { none with foreground = Some lblue };
      draw_line "          r=reset | Esc=exit"
        { none with foreground = Some lblue };
      incr row;

      match cstate.selected_chord with
      | None ->
        draw_line "Press 1-7 to select a diatonic chord" { none with foreground = Some white }
      | Some (root, chord_type) ->
        let chord_name = Theory.name_chord root
          (match chord_type with
           | Types.Major -> Types.Major
           | Types.Minor -> Types.Minor
           | Types.Diminished -> Types.Dimin)
        in
        draw_line (Fmt.str "Chord: %s" chord_name) { none with bold = Some true; foreground = Some lgreen };
        incr row;

        (* Show inversion info *)
        let inversion_name = match cstate.inversion with
          | 0 -> "Root Position"
          | 1 -> "1st Inversion"
          | 2 -> "2nd Inversion"
          | _ -> "Root Position"
        in
        draw_line (Fmt.str "Inversion: %s (press 'i' to cycle)" inversion_name)
          { none with foreground = Some lcyan };
        incr row;

        (* Build and show inverted notes *)
        (* Convert diatonic_triad to chord for theory functions *)
        let chord_type_full : Types.chord = match chord_type with
          | Types.Major -> Types.Major
          | Types.Minor -> Types.Minor
          | Types.Diminished -> Types.Dimin
        in
        let base_notes = Theory.build_triad_notes root chord_type_full in
        let inverted_notes = Theory.invert_chord_notes base_notes cstate.inversion in
        let notes_str = String.concat " - "
          (List.map (fun note -> Pp.NOTES.FMT.sprint_note note) inverted_notes) in
        draw_line (Fmt.str "Notes: %s" notes_str) { none with foreground = Some lwhite };
        incr row;

        (* Show chord intervals *)
        let intervals_str = match chord_type with
          | Types.Major -> "Intervals: 1 3 5 (Major triad)"
          | Types.Minor -> "Intervals: 1 b3 5 (Minor triad)"
          | Types.Diminished -> "Intervals: 1 b3 b5 (Diminished triad)"
        in
        draw_line intervals_str { none with foreground = Some lyellow };

        (* Draw chord diagram if we have voicings *)
        if List.length cstate.voicings > 0 then begin
          let voicing = List.nth cstate.voicings cstate.voicing_index in
          (* Position diagram: row offset slightly up, column offset more to the right *)
          let diagram_row_offset = max 0 (!row - 1) in
          let diagram_col_offset = 3 in
          let diagram_rows_used =
            Draw_chord_diagram.draw_chord_diagram ctx diagram_row_offset diagram_col_offset voicing chord_name
          in
          row := diagram_row_offset + diagram_rows_used;
          incr row;

          (* Show voicing navigation info *)
          let voicing_info = Fmt.str "Voicing %d/%d (press 'v' to cycle)"
            (cstate.voicing_index + 1) (List.length cstate.voicings)
          in
          draw_line voicing_info { none with foreground = Some lblue }
        end
        else begin
          incr row;
          draw_line "No voicings found for this chord" { none with foreground = Some lred }
        end

    method! can_focus = false
  end

(** Arpeggio mode widget - displays arpeggio pattern *)
class arpeggio_widget (astate : App_state.arpeggio_state) (app_state : App_state.t) (_context : Types.ocamuse_structure) =
  object
    inherit LTerm_widget.t "arpeggio_widget"

    method! draw ctx _focused =
      let open LTerm_draw in
      let open LTerm_style in
      let open LTerm_geom in
      clear ctx;

      (* Debug mode: draw border *)
      if !(app_state.debug_mode) then begin
        let sz = size ctx in
        draw_frame ctx { row1 = 0; col1 = 0; row2 = sz.rows - 1; col2 = sz.cols - 1 }
          ~style:{ none with foreground = Some lcyan } Light
      end;

      let { rows; cols } = size ctx in
      let row = ref 0 in

      let draw_line text style =
        if !row < rows && cols > 2 then begin
          draw_text_line ctx !row 1 text style;
          incr row
        end
      in

      draw_line "ARPEGGIO MODE" { none with bold = Some true; foreground = Some lcyan };
      incr row;

      (* Show controls on two lines *)
      draw_line "Controls: 1-7=select chord"
        { none with foreground = Some lblue };
      draw_line "          r=reset | Esc=exit"
        { none with foreground = Some lblue };
      incr row;

      match astate.arp_chord with
      | None ->
        draw_line "Press 1-7 to select a chord" { none with foreground = Some white }
      | Some (root, chord_type) ->
        let chord_name = Theory.name_chord root chord_type in
        draw_line (Fmt.str "Chord: %s" chord_name) { none with bold = Some true; foreground = Some lgreen };
        incr row;

        (* Show chord intervals *)
        let intervals_str = match chord_type with
          | Types.Major -> "Intervals: 1 3 5 (Major triad)"
          | Types.Minor -> "Intervals: 1 b3 5 (Minor triad)"
          | Types.Dimin -> "Intervals: 1 b3 b5 (Diminished triad)"
          | _ -> "Intervals: Complex chord"
        in
        draw_line intervals_str { none with foreground = Some lyellow }

    method! can_focus = false
  end
