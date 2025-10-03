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
      match tstate.selection_step with
      | App_state.SelectingRoot ->
        (match Conv.base_of_char ch with
        | Ok base ->
          let new_state =
            { tstate with
              temp_root = Some { base; alteration = 0 }
            ; selection_step = App_state.SelectingAlteration
            }
          in
          state.mode := App_state.TonalitySelection new_state;
          true
        | Error _ -> false)
      | App_state.SelectingAlteration ->
        if ch = '#' then begin
          match tstate.temp_root with
          | Some note ->
            let new_state =
              { tstate with
                temp_root = Some { note with alteration = note.alteration + 1 }
              }
            in
            state.mode := App_state.TonalitySelection new_state;
            true
          | None -> false
        end
        else if ch = 'b' then begin
          match tstate.temp_root with
          | Some note ->
            let new_state =
              { tstate with
                temp_root = Some { note with alteration = note.alteration - 1 }
              }
            in
            state.mode := App_state.TonalitySelection new_state;
            true
          | None -> false
        end
        else if ch >= '1' && ch <= '7' then begin
          let mode_idx = Char.code ch - Char.code '1' in
          let mode = match mode_idx with
            | 0 -> C_mode | 1 -> D_mode | 2 -> E_mode | 3 -> F_mode
            | 4 -> G_mode | 5 -> A_mode | 6 -> B_mode | _ -> C_mode
          in
          let new_state =
            { tstate with temp_mode = Some mode; selection_step = App_state.SelectingMode }
          in
          state.mode := App_state.TonalitySelection new_state;
          true
        end
        else false
      | App_state.SelectingMode ->
        if ch >= '1' && ch <= '7' then begin
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
        (* Apply the selection *)
        state.context.root_note <- root;
        state.context.mode <- mode;
        (* Rebuild fretboard with new context if needed *)
        App_state.return_to_normal state;
        true
      | _ -> false
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
      if ch >= '1' && ch <= '5' then begin
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
        let prefix = if i = tstate.selected_index then "→ " else "  " in
        let style =
          if i = tstate.selected_index then
            { none with bold = Some true; foreground = Some lgreen }
          else
            { none with foreground = Some white }
        in
        draw_line (Fmt.str "%s%d. %s: %s" prefix (i + 1) name tuning_str) style
      ) tstate.available_tunings;

      incr row;
      draw_line "Press 1-5 or ↑↓ to select, Enter to confirm" { none with foreground = Some lblue }

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

      draw_line (Fmt.str "Root: %s" root_str)
        { none with bold = Some true; foreground = Some lgreen };
      draw_line (Fmt.str "Mode: %s" mode_str)
        { none with bold = Some true; foreground = Some lyellow };

      incr row;

      (* Show instructions based on step *)
      begin match tstate.selection_step with
      | App_state.SelectingRoot ->
        draw_line "→ Press a-g to select root note" { none with foreground = Some lblue }
      | App_state.SelectingAlteration ->
        draw_line "→ Press # for sharp, b for flat, or 1-7 for mode"
          { none with foreground = Some lblue }
      | App_state.SelectingMode ->
        draw_line "→ Press 1-7 to select mode, Enter to confirm"
          { none with foreground = Some lblue };
        incr row;
        draw_line "  1:Ionian 2:Dorian 3:Phrygian 4:Lydian"
          { none with foreground = Some white };
        draw_line "  5:Mixolydian 6:Aeolian 7:Locrian"
          { none with foreground = Some white }
      end

    method! can_focus = false
  end
