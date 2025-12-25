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
      let ch_code = Uchar.to_int c in
      if ch_code > 127 then false
      else begin
      let ch = Char.chr ch_code in
      (* Root field accepts a-g *)
      if (tstate.selection_step = App_state.SelectingRoot ||
          tstate.selection_step = App_state.SelectingAlteration) &&
         ((ch >= 'a' && ch <= 'g') || (ch >= 'A' && ch <= 'G')) then begin
        match Conv.base_of_char ch with
        | Ok base ->
          let new_state =
            { tstate with
              temp_root = Some { base; alteration = 0 };
              selection_step = App_state.SelectingAlteration
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
      else false
      end
    | Key { code = Enter; _ } -> begin
      match tstate.selection_step with
      | App_state.SelectingRoot | App_state.SelectingAlteration ->
        (* Advance to category selection if root is set *)
        (match tstate.temp_root with
        | Some _ ->
          let new_state = { tstate with selection_step = App_state.SelectingCategory } in
          state.mode := App_state.TonalitySelection new_state;
          true
        | None -> false)
      | App_state.SelectingCategory ->
        (* Advance to scale selection within category *)
        let new_state = { tstate with selection_step = App_state.SelectingScale } in
        state.mode := App_state.TonalitySelection new_state;
        true
      | App_state.SelectingScale ->
        (* Apply the selection *)
        (match tstate.temp_root with
        | Some root ->
          let scales = Display.scales_in_category tstate.temp_category in
          let scale = List.nth scales tstate.scale_index in
          state.context.root_note <- root;
          state.context.scale <- scale;
          state.context.highlight_source <- Types.Tonality (scale, root);
          let view = match !(state.context.display_mode) with
            | Types.Flat v -> v
            | Types.Pattern (v, _) -> v
          in
          state.context.display_mode := Types.Pattern (view, scale);
          (* Rebuild fretboard with correct enharmonic spelling for this key *)
          let spelling_tbl = Display.build_spelling_table scale root in
          let new_fretboard = Fretboard.init
            ~tuning:state.context.tuning
            ~range:state.context.fret_range
            ~spelling_tbl () in
          state.context.fretboard <- new_fretboard;
          App_state.return_to_normal state;
          true
        | None -> false)
    end
    | Key { code = Up; _ } -> begin
      match tstate.selection_step with
      | App_state.SelectingRoot -> true  (* Stay at top *)
      | App_state.SelectingAlteration ->
        let new_state = { tstate with selection_step = App_state.SelectingRoot } in
        state.mode := App_state.TonalitySelection new_state;
        true
      | App_state.SelectingCategory ->
        (* Move to previous category *)
        let num_cats = List.length Display.all_categories in
        let new_idx = if tstate.category_index > 0 then tstate.category_index - 1 else num_cats - 1 in
        let new_cat = List.nth Display.all_categories new_idx in
        let new_state = { tstate with
          category_index = new_idx;
          temp_category = new_cat;
          scale_index = 0  (* Reset scale selection when changing category *)
        } in
        state.mode := App_state.TonalitySelection new_state;
        true
      | App_state.SelectingScale ->
        (* Move to previous scale in category *)
        let scales = Display.scales_in_category tstate.temp_category in
        let num_scales = List.length scales in
        let new_idx = if tstate.scale_index > 0 then tstate.scale_index - 1 else num_scales - 1 in
        let new_state = { tstate with scale_index = new_idx } in
        state.mode := App_state.TonalitySelection new_state;
        true
    end
    | Key { code = Down; _ } -> begin
      match tstate.selection_step with
      | App_state.SelectingRoot ->
        let new_state = { tstate with selection_step = App_state.SelectingAlteration } in
        state.mode := App_state.TonalitySelection new_state;
        true
      | App_state.SelectingAlteration ->
        (match tstate.temp_root with
        | Some _ ->
          let new_state = { tstate with selection_step = App_state.SelectingCategory } in
          state.mode := App_state.TonalitySelection new_state;
          true
        | None -> true)
      | App_state.SelectingCategory ->
        (* Move to next category *)
        let num_cats = List.length Display.all_categories in
        let new_idx = (tstate.category_index + 1) mod num_cats in
        let new_cat = List.nth Display.all_categories new_idx in
        let new_state = { tstate with
          category_index = new_idx;
          temp_category = new_cat;
          scale_index = 0
        } in
        state.mode := App_state.TonalitySelection new_state;
        true
      | App_state.SelectingScale ->
        (* Move to next scale in category *)
        let scales = Display.scales_in_category tstate.temp_category in
        let num_scales = List.length scales in
        let new_idx = (tstate.scale_index + 1) mod num_scales in
        let new_state = { tstate with scale_index = new_idx } in
        state.mode := App_state.TonalitySelection new_state;
        true
    end
    | Key { code = Left; _ } -> begin
      match tstate.selection_step with
      | App_state.SelectingCategory | App_state.SelectingScale ->
        (* Go back a step *)
        let new_step = match tstate.selection_step with
          | App_state.SelectingScale -> App_state.SelectingCategory
          | App_state.SelectingCategory -> App_state.SelectingAlteration
          | step -> step
        in
        let new_state = { tstate with selection_step = new_step } in
        state.mode := App_state.TonalitySelection new_state;
        true
      | _ -> false
    end
    | Key { code = Right; _ } -> begin
      match tstate.selection_step with
      | App_state.SelectingAlteration ->
        (match tstate.temp_root with
        | Some _ ->
          let new_state = { tstate with selection_step = App_state.SelectingCategory } in
          state.mode := App_state.TonalitySelection new_state;
          true
        | None -> false)
      | App_state.SelectingCategory ->
        let new_state = { tstate with selection_step = App_state.SelectingScale } in
        state.mode := App_state.TonalitySelection new_state;
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
      (* Rebuild fretboard with new tuning and current key's enharmonic spelling *)
      let spelling_tbl = Display.build_spelling_table state.context.scale state.context.root_note in
      let new_fretboard = Fretboard.init
        ~tuning:selected_tuning
        ~range:state.context.fret_range
        ~spelling_tbl () in
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

    method! size_request =
      (* Title + blank + tunings + blank + instructions *)
      let num_tunings = List.length tstate.available_tunings in
      { LTerm_geom.rows = 2 + num_tunings + 2; cols = 60 }

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

    method! size_request =
      (* More rows for category-based selection *)
      { LTerm_geom.rows = 16; cols = 70 }

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
      let category_str = Display.category_name tstate.temp_category in
      let scales = Display.scales_in_category tstate.temp_category in
      let scale = List.nth scales tstate.scale_index in
      let scale_str = Display.scale_name scale in

      (* Highlight active field *)
      let root_style = match tstate.selection_step with
        | App_state.SelectingRoot | App_state.SelectingAlteration ->
            { none with bold = Some true; foreground = Some lgreen }
        | _ -> { none with foreground = Some white }
      in
      let category_style = match tstate.selection_step with
        | App_state.SelectingCategory ->
            { none with bold = Some true; foreground = Some lyellow }
        | _ -> { none with foreground = Some white }
      in
      let scale_style = match tstate.selection_step with
        | App_state.SelectingScale ->
            { none with bold = Some true; foreground = Some lmagenta }
        | _ -> { none with foreground = Some white }
      in
      draw_line (Fmt.str "Root: %s" root_str) root_style;
      draw_line (Fmt.str "Category: %s" category_str) category_style;
      draw_line (Fmt.str "Scale: %s" scale_str) scale_style;

      incr row;

      (* Show instructions based on step *)
      begin match tstate.selection_step with
      | App_state.SelectingRoot ->
        draw_line "> Press a-g to set root note" { none with foreground = Some lblue }
      | App_state.SelectingAlteration ->
        draw_line "> Press a-g (root), # (sharp), or b (flat)"
          { none with foreground = Some lblue };
        draw_line "  Press Enter/Right to continue" { none with foreground = Some white }
      | App_state.SelectingCategory ->
        draw_line "> Use Up/Down to browse categories" { none with foreground = Some lblue };
        incr row;
        (* Show all categories *)
        List.iteri (fun i cat ->
          let name = Display.category_name cat in
          let prefix = if i = tstate.category_index then "> " else "  " in
          let style = if i = tstate.category_index then
            { none with bold = Some true; foreground = Some lyellow }
          else
            { none with foreground = Some white }
          in
          draw_line (Fmt.str "%s%s" prefix name) style
        ) Display.all_categories;
        draw_line "  Press Enter/Right to select scales" { none with foreground = Some white }
      | App_state.SelectingScale ->
        draw_line "> Use Up/Down to browse scales" { none with foreground = Some lblue };
        incr row;
        (* Show scales in current category *)
        List.iteri (fun i s ->
          let name = Display.scale_name s in
          let prefix = if i = tstate.scale_index then "> " else "  " in
          let style = if i = tstate.scale_index then
            { none with bold = Some true; foreground = Some lmagenta }
          else
            { none with foreground = Some white }
          in
          draw_line (Fmt.str "%s%s" prefix name) style
        ) scales;
        draw_line "  Press Enter to apply, Left to go back" { none with foreground = Some white }
      end;
      incr row;
      draw_line "Esc to cancel" { none with foreground = Some lcyan }

    method! can_focus = false
  end

(** Handle theme selection input *)
let handle_theme_input state event =
  let open LTerm_event in
  match !(state.App_state.mode) with
  | App_state.ThemeSelection tstate -> begin
    match event with
    | Key { code = Escape; _ } ->
      App_state.return_to_normal state;
      true
    | Key { code = Up; _ } ->
      let num_themes = List.length tstate.available_themes in
      let new_idx =
        if tstate.theme_index > 0 then tstate.theme_index - 1
        else num_themes - 1
      in
      let new_state = { tstate with theme_index = new_idx } in
      (* Apply preview *)
      let selected_theme = List.nth tstate.available_themes new_idx in
      state.context.color_theme <- selected_theme;
      state.mode := App_state.ThemeSelection new_state;
      true
    | Key { code = Down; _ } ->
      let num_themes = List.length tstate.available_themes in
      let new_idx = (tstate.theme_index + 1) mod num_themes in
      let new_state = { tstate with theme_index = new_idx } in
      (* Apply preview *)
      let selected_theme = List.nth tstate.available_themes new_idx in
      state.context.color_theme <- selected_theme;
      state.mode := App_state.ThemeSelection new_state;
      true
    | Key { code = Enter; _ } ->
      (* Theme already applied via preview, just exit *)
      App_state.return_to_normal state;
      true
    | Key { code = Char c; _ } ->
      let ch_code = Uchar.to_int c in
      if ch_code > 127 then false
      else begin
        let ch = Char.chr ch_code in
        (* Allow number keys 1-9 for quick selection *)
        if ch >= '1' && ch <= '9' then begin
          let idx = Char.code ch - Char.code '1' in
          if idx < List.length tstate.available_themes then begin
            let new_state = { tstate with theme_index = idx } in
            let selected_theme = List.nth tstate.available_themes idx in
            state.context.color_theme <- selected_theme;
            state.mode := App_state.ThemeSelection new_state;
            true
          end
          else false
        end
        else false
      end
    | _ -> false
  end
  | _ -> false

(** Theme selector widget - displays available themes with descriptions *)
class theme_selector_widget (tstate : App_state.theme_state) (app_state : App_state.t) =
  object
    inherit LTerm_widget.t "theme_selector"

    method! size_request =
      let num_themes = List.length tstate.available_themes in
      { LTerm_geom.rows = 4 + num_themes + 2; cols = 60 }

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

      draw_line "SELECT COLOR THEME" { none with bold = Some true; foreground = Some lcyan };
      incr row;

      (* Show theme type explanation *)
      draw_line "Built-in themes:" { none with foreground = Some lyellow };

      List.iteri (fun i theme ->
        let name = Color_theme.theme_name theme in
        let desc = match theme with
          | Types.ChromaticGradient -> "Colors by pitch class (C=red, G=blue...)"
          | Types.DiatonicDegrees -> "Colors by scale degree (1=red, 5=yellow...)"
          | Types.CustomPalette pname ->
            (match Config.Palettes.find_by_name pname with
             | Some p -> p.Config.Palettes.description
             | None -> "Custom palette")
        in
        let prefix = if i = tstate.theme_index then "> " else "  " in
        let style =
          if i = tstate.theme_index then
            { none with bold = Some true; foreground = Some lgreen }
          else
            { none with foreground = Some white }
        in
        draw_line (Fmt.str "%s%d. %s" prefix (i + 1) name) style;
        (* Show description for selected theme *)
        if i = tstate.theme_index then
          draw_line (Fmt.str "     %s" desc) { none with foreground = Some lblue }
      ) tstate.available_themes;

      incr row;
      draw_line "Up/Down or 1-9 to select, Enter to confirm, Esc to cancel"
        { none with foreground = Some lcyan }

    method! can_focus = false
  end

(** Handle chord lookup input *)
let handle_chord_lookup_input state event =
  let open LTerm_event in
  let open Types in
  match !(state.App_state.mode) with
  | App_state.ChordLookup cstate -> begin
    match event with
    | Key { code = Escape; _ } ->
      App_state.return_to_normal state;
      true
    | Key { code = Char c; _ } ->
      let ch_code = Uchar.to_int c in
      if ch_code > 127 then false
      else begin
      let ch = Char.chr ch_code in
      (* Root field accepts a-g *)
      if (cstate.chord_step = App_state.ChordSelectingRoot ||
          cstate.chord_step = App_state.ChordSelectingAlteration) &&
         ((ch >= 'a' && ch <= 'g') || (ch >= 'A' && ch <= 'G')) then begin
        match Conv.base_of_char ch with
        | Ok base ->
          let new_state =
            { cstate with
              chord_root = Some { base; alteration = 0 };
              chord_step = App_state.ChordSelectingAlteration
            }
          in
          state.mode := App_state.ChordLookup new_state;
          true
        | Error _ -> false
      end
      (* Alteration accepts # and b *)
      else if cstate.chord_step = App_state.ChordSelectingAlteration && ch = '#' then begin
        match cstate.chord_root with
        | Some note ->
          let new_state =
            { cstate with chord_root = Some { note with alteration = note.alteration + 1 } }
          in
          state.mode := App_state.ChordLookup new_state;
          true
        | None -> false
      end
      else if cstate.chord_step = App_state.ChordSelectingAlteration && ch = 'b' then begin
        match cstate.chord_root with
        | Some note ->
          let new_state =
            { cstate with chord_root = Some { note with alteration = note.alteration - 1 } }
          in
          state.mode := App_state.ChordLookup new_state;
          true
        | None -> false
      end
      else false
      end
    | Key { code = Enter; _ } -> begin
      match cstate.chord_step with
      | App_state.ChordSelectingRoot | App_state.ChordSelectingAlteration ->
        (* Advance to category selection if root is set *)
        (match cstate.chord_root with
        | Some _ ->
          let new_state = { cstate with chord_step = App_state.ChordSelectingCategory } in
          state.mode := App_state.ChordLookup new_state;
          true
        | None -> false)
      | App_state.ChordSelectingCategory ->
        (* Advance to chord type selection within category *)
        let new_state = { cstate with chord_step = App_state.ChordSelectingType } in
        state.mode := App_state.ChordLookup new_state;
        true
      | App_state.ChordSelectingType ->
        (* Apply the chord selection *)
        (match cstate.chord_root with
        | Some root ->
          let chords = Display.chords_in_category cstate.chord_category in
          let chord = List.nth chords cstate.chord_index in
          state.context.highlight_source <- Types.Chord (root, chord);
          App_state.return_to_normal state;
          true
        | None -> false)
    end
    | Key { code = Up; _ } -> begin
      match cstate.chord_step with
      | App_state.ChordSelectingRoot -> true  (* Stay at top *)
      | App_state.ChordSelectingAlteration ->
        let new_state = { cstate with chord_step = App_state.ChordSelectingRoot } in
        state.mode := App_state.ChordLookup new_state;
        true
      | App_state.ChordSelectingCategory ->
        (* Move to previous category *)
        let num_cats = List.length Display.all_chord_categories in
        let new_idx = if cstate.chord_category_index > 0 then cstate.chord_category_index - 1 else num_cats - 1 in
        let new_cat = List.nth Display.all_chord_categories new_idx in
        let new_state = { cstate with
          chord_category_index = new_idx;
          chord_category = new_cat;
          chord_index = 0  (* Reset chord selection when changing category *)
        } in
        state.mode := App_state.ChordLookup new_state;
        true
      | App_state.ChordSelectingType ->
        (* Move to previous chord in category *)
        let chords = Display.chords_in_category cstate.chord_category in
        let num_chords = List.length chords in
        let new_idx = if cstate.chord_index > 0 then cstate.chord_index - 1 else num_chords - 1 in
        let new_state = { cstate with chord_index = new_idx } in
        state.mode := App_state.ChordLookup new_state;
        true
    end
    | Key { code = Down; _ } -> begin
      match cstate.chord_step with
      | App_state.ChordSelectingRoot ->
        let new_state = { cstate with chord_step = App_state.ChordSelectingAlteration } in
        state.mode := App_state.ChordLookup new_state;
        true
      | App_state.ChordSelectingAlteration ->
        (match cstate.chord_root with
        | Some _ ->
          let new_state = { cstate with chord_step = App_state.ChordSelectingCategory } in
          state.mode := App_state.ChordLookup new_state;
          true
        | None -> true)
      | App_state.ChordSelectingCategory ->
        (* Move to next category *)
        let num_cats = List.length Display.all_chord_categories in
        let new_idx = (cstate.chord_category_index + 1) mod num_cats in
        let new_cat = List.nth Display.all_chord_categories new_idx in
        let new_state = { cstate with
          chord_category_index = new_idx;
          chord_category = new_cat;
          chord_index = 0
        } in
        state.mode := App_state.ChordLookup new_state;
        true
      | App_state.ChordSelectingType ->
        (* Move to next chord in category *)
        let chords = Display.chords_in_category cstate.chord_category in
        let num_chords = List.length chords in
        let new_idx = (cstate.chord_index + 1) mod num_chords in
        let new_state = { cstate with chord_index = new_idx } in
        state.mode := App_state.ChordLookup new_state;
        true
    end
    | Key { code = Left; _ } -> begin
      match cstate.chord_step with
      | App_state.ChordSelectingCategory | App_state.ChordSelectingType ->
        (* Go back a step *)
        let new_step = match cstate.chord_step with
          | App_state.ChordSelectingType -> App_state.ChordSelectingCategory
          | App_state.ChordSelectingCategory -> App_state.ChordSelectingAlteration
          | step -> step
        in
        let new_state = { cstate with chord_step = new_step } in
        state.mode := App_state.ChordLookup new_state;
        true
      | _ -> false
    end
    | Key { code = Right; _ } -> begin
      match cstate.chord_step with
      | App_state.ChordSelectingAlteration ->
        (match cstate.chord_root with
        | Some _ ->
          let new_state = { cstate with chord_step = App_state.ChordSelectingCategory } in
          state.mode := App_state.ChordLookup new_state;
          true
        | None -> false)
      | App_state.ChordSelectingCategory ->
        let new_state = { cstate with chord_step = App_state.ChordSelectingType } in
        state.mode := App_state.ChordLookup new_state;
        true
      | _ -> false
    end
    | _ -> false
  end
  | _ -> false

(** Chord lookup widget - displays current chord lookup state *)
class chord_lookup_widget (cstate : App_state.chord_state) (app_state : App_state.t) =
  object
    inherit LTerm_widget.t "chord_lookup"

    method! size_request =
      { LTerm_geom.rows = 18; cols = 70 }

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

      draw_line "CHORD LOOKUP" { none with bold = Some true; foreground = Some lcyan };
      incr row;

      (* Show current selection *)
      let root_str = match cstate.chord_root with
        | Some note -> Pp.NOTES.FMT.sprint_note note
        | None -> "..."
      in
      let category_str = Display.chord_category_name cstate.chord_category in
      let chords = Display.chords_in_category cstate.chord_category in
      let chord = List.nth chords cstate.chord_index in
      let chord_str = Display.chord_name chord in

      (* Highlight active field *)
      let root_style = match cstate.chord_step with
        | App_state.ChordSelectingRoot | App_state.ChordSelectingAlteration ->
            { none with bold = Some true; foreground = Some lgreen }
        | _ -> { none with foreground = Some white }
      in
      let category_style = match cstate.chord_step with
        | App_state.ChordSelectingCategory ->
            { none with bold = Some true; foreground = Some lyellow }
        | _ -> { none with foreground = Some white }
      in
      let chord_style = match cstate.chord_step with
        | App_state.ChordSelectingType ->
            { none with bold = Some true; foreground = Some lmagenta }
        | _ -> { none with foreground = Some white }
      in
      draw_line (Fmt.str "Root: %s" root_str) root_style;
      draw_line (Fmt.str "Category: %s" category_str) category_style;
      draw_line (Fmt.str "Chord: %s" chord_str) chord_style;

      incr row;

      (* Show instructions based on step *)
      begin match cstate.chord_step with
      | App_state.ChordSelectingRoot ->
        draw_line "> Press a-g to set root note" { none with foreground = Some lblue }
      | App_state.ChordSelectingAlteration ->
        draw_line "> Press a-g (root), # (sharp), or b (flat)"
          { none with foreground = Some lblue };
        draw_line "  Press Enter/Right to continue" { none with foreground = Some white }
      | App_state.ChordSelectingCategory ->
        draw_line "> Use Up/Down to browse categories" { none with foreground = Some lblue };
        incr row;
        (* Show all categories *)
        List.iteri (fun i cat ->
          let name = Display.chord_category_name cat in
          let prefix = if i = cstate.chord_category_index then "> " else "  " in
          let style = if i = cstate.chord_category_index then
            { none with bold = Some true; foreground = Some lyellow }
          else
            { none with foreground = Some white }
          in
          draw_line (Fmt.str "%s%s" prefix name) style
        ) Display.all_chord_categories;
        draw_line "  Press Enter/Right to select chords" { none with foreground = Some white }
      | App_state.ChordSelectingType ->
        draw_line "> Use Up/Down to browse chords" { none with foreground = Some lblue };
        incr row;
        (* Show chords in current category *)
        List.iteri (fun i c ->
          let name = Display.chord_name c in
          let prefix = if i = cstate.chord_index then "> " else "  " in
          let style = if i = cstate.chord_index then
            { none with bold = Some true; foreground = Some lmagenta }
          else
            { none with foreground = Some white }
          in
          draw_line (Fmt.str "%s%s" prefix name) style
        ) chords;
        draw_line "  Press Enter to apply, Left to go back" { none with foreground = Some white }
      end;
      incr row;
      draw_line "Esc to cancel" { none with foreground = Some lcyan }

    method! can_focus = false
  end

(** Handle chord progression input *)
let handle_progression_input state event =
  let open LTerm_event in
  match !(state.App_state.mode) with
  | App_state.ChordProgression pstate -> begin
    match pstate.prog_input_mode with
    | App_state.ProgNavigating -> begin
      match event with
      | Key { code = Escape; _ } ->
        App_state.return_to_normal state;
        true
      | Key { code = Left; _ } ->
        (* Move cursor left *)
        if pstate.prog_cursor > 0 then begin
          let new_state = { pstate with prog_cursor = pstate.prog_cursor - 1 } in
          state.mode := App_state.ChordProgression new_state;
          (* Update fretboard to show this chord *)
          if new_state.prog_cursor < List.length pstate.prog_chords then begin
            let chord = List.nth pstate.prog_chords new_state.prog_cursor in
            state.context.highlight_source <- Types.Chord (chord.prog_root, chord.prog_chord)
          end
        end;
        true
      | Key { code = Right; _ } ->
        (* Move cursor right *)
        let max_pos = List.length pstate.prog_chords in
        if pstate.prog_cursor < max_pos - 1 then begin
          let new_state = { pstate with prog_cursor = pstate.prog_cursor + 1 } in
          state.mode := App_state.ChordProgression new_state;
          (* Update fretboard to show this chord *)
          let chord = List.nth pstate.prog_chords new_state.prog_cursor in
          state.context.highlight_source <- Types.Chord (chord.prog_root, chord.prog_chord)
        end;
        true
      | Key { code = Enter; _ } ->
        (* Play current chord (update fretboard) *)
        if pstate.prog_cursor < List.length pstate.prog_chords then begin
          let chord = List.nth pstate.prog_chords pstate.prog_cursor in
          state.context.highlight_source <- Types.Chord (chord.prog_root, chord.prog_chord)
        end;
        true
      | Key { code = Char c; _ } ->
        let ch_code = Uchar.to_int c in
        if ch_code > 127 then false
        else begin
        let ch = Char.chr ch_code in
        match ch with
        | 'a' ->
          (* Enter AddingChord mode *)
          let add_state =
            { App_state.chord_root = None
            ; chord_type = None
            ; chord_category = Types.Triads
            ; chord_category_index = 0
            ; chord_index = 0
            ; chord_step = App_state.ChordSelectingRoot
            }
          in
          let new_state = { pstate with
            prog_input_mode = App_state.ProgAddingChord;
            prog_add_state = Some add_state
          } in
          state.mode := App_state.ChordProgression new_state;
          true
        | 'r' ->
          (* Enter RomanInput mode *)
          let new_state = { pstate with
            prog_input_mode = App_state.ProgRomanInput;
            prog_roman_buffer = ""
          } in
          state.mode := App_state.ChordProgression new_state;
          true
        | 'l' ->
          (* Enter BrowsingLibrary mode *)
          let new_state = { pstate with
            prog_input_mode = App_state.ProgBrowsingLibrary;
            prog_library_index = 0
          } in
          state.mode := App_state.ChordProgression new_state;
          true
        | 'd' ->
          (* Delete chord at cursor *)
          if pstate.prog_cursor < List.length pstate.prog_chords then begin
            let new_chords = List.filteri (fun i _ -> i <> pstate.prog_cursor) pstate.prog_chords in
            let new_cursor = min pstate.prog_cursor (max 0 (List.length new_chords - 1)) in
            let new_state = { pstate with prog_chords = new_chords; prog_cursor = new_cursor } in
            state.mode := App_state.ChordProgression new_state;
            (* Update fretboard if chords remain *)
            if new_cursor < List.length new_chords then begin
              let chord = List.nth new_chords new_cursor in
              state.context.highlight_source <- Types.Chord (chord.prog_root, chord.prog_chord)
            end
          end;
          true
        | 'c' ->
          (* Clear entire progression *)
          let new_state = { pstate with prog_chords = []; prog_cursor = 0 } in
          state.mode := App_state.ChordProgression new_state;
          true
        | '1' | '2' | '3' | '4' | '5' | '6' | '7' ->
          (* Quick add diatonic chord *)
          let degree = Char.code ch - Char.code '0' in
          let diatonic = Display.diatonic_triads state.context.scale state.context.root_note in
          if degree <= List.length diatonic then begin
            let (note, triad, chord, deg) = List.nth diatonic (degree - 1) in
            let roman = Display.degree_to_roman deg triad in
            let prog_chord =
              { App_state.prog_root = note
              ; prog_chord = chord
              ; prog_roman = Some roman
              }
            in
            (* Insert at cursor position *)
            let before, after =
              let rec split i acc = function
                | [] -> (List.rev acc, [])
                | x :: xs ->
                  if i = 0 then (List.rev acc, x :: xs)
                  else split (i - 1) (x :: acc) xs
              in
              split pstate.prog_cursor [] pstate.prog_chords
            in
            let new_chords = before @ [prog_chord] @ after in
            let new_state = { pstate with
              prog_chords = new_chords;
              prog_cursor = pstate.prog_cursor + 1
            } in
            state.mode := App_state.ChordProgression new_state;
            state.context.highlight_source <- Types.Chord (note, chord);
            true
          end
          else false
        | _ -> false
        end
      | _ -> false
    end
    | App_state.ProgAddingChord -> begin
      (* Reuse chord lookup logic but capture result *)
      match pstate.prog_add_state with
      | None -> false
      | Some cstate -> begin
        match event with
        | Key { code = Escape; _ } ->
          (* Cancel and return to navigating *)
          let new_state = { pstate with
            prog_input_mode = App_state.ProgNavigating;
            prog_add_state = None
          } in
          state.mode := App_state.ChordProgression new_state;
          true
        | Key { code = Char c; _ } ->
          let ch_code = Uchar.to_int c in
          if ch_code > 127 then false
          else begin
          let ch = Char.chr ch_code in
          (* Root selection *)
          if (cstate.chord_step = App_state.ChordSelectingRoot ||
              cstate.chord_step = App_state.ChordSelectingAlteration) &&
             ((ch >= 'a' && ch <= 'g') || (ch >= 'A' && ch <= 'G')) then begin
            match Conv.base_of_char ch with
            | Ok base ->
              let new_cstate =
                { cstate with
                  chord_root = Some { Types.base; alteration = 0 };
                  chord_step = App_state.ChordSelectingAlteration
                }
              in
              let new_state = { pstate with prog_add_state = Some new_cstate } in
              state.mode := App_state.ChordProgression new_state;
              true
            | Error _ -> false
          end
          else if cstate.chord_step = App_state.ChordSelectingAlteration && ch = '#' then begin
            match cstate.chord_root with
            | Some note ->
              let new_cstate =
                { cstate with chord_root = Some { note with alteration = note.Types.alteration + 1 } }
              in
              let new_state = { pstate with prog_add_state = Some new_cstate } in
              state.mode := App_state.ChordProgression new_state;
              true
            | None -> false
          end
          else if cstate.chord_step = App_state.ChordSelectingAlteration && ch = 'b' then begin
            match cstate.chord_root with
            | Some note ->
              let new_cstate =
                { cstate with chord_root = Some { note with alteration = note.Types.alteration - 1 } }
              in
              let new_state = { pstate with prog_add_state = Some new_cstate } in
              state.mode := App_state.ChordProgression new_state;
              true
            | None -> false
          end
          else false
          end
        | Key { code = Enter; _ } -> begin
          match cstate.chord_step with
          | App_state.ChordSelectingRoot | App_state.ChordSelectingAlteration ->
            (match cstate.chord_root with
            | Some _ ->
              let new_cstate = { cstate with chord_step = App_state.ChordSelectingCategory } in
              let new_state = { pstate with prog_add_state = Some new_cstate } in
              state.mode := App_state.ChordProgression new_state;
              true
            | None -> false)
          | App_state.ChordSelectingCategory ->
            let new_cstate = { cstate with chord_step = App_state.ChordSelectingType } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
          | App_state.ChordSelectingType ->
            (* Add chord to progression *)
            (match cstate.chord_root with
            | Some root ->
              let chords = Display.chords_in_category cstate.chord_category in
              let chord = List.nth chords cstate.chord_index in
              (* Analyze in current key *)
              let roman = Display.analyze_chord_in_key
                state.context.scale state.context.root_note root chord in
              let prog_chord =
                { App_state.prog_root = root
                ; prog_chord = chord
                ; prog_roman = roman
                }
              in
              (* Insert at cursor position *)
              let before, after =
                let rec split i acc = function
                  | [] -> (List.rev acc, [])
                  | x :: xs ->
                    if i = 0 then (List.rev acc, x :: xs)
                    else split (i - 1) (x :: acc) xs
                in
                split pstate.prog_cursor [] pstate.prog_chords
              in
              let new_chords = before @ [prog_chord] @ after in
              let new_pstate = { pstate with
                prog_chords = new_chords;
                prog_cursor = pstate.prog_cursor + 1;
                prog_input_mode = App_state.ProgNavigating;
                prog_add_state = None
              } in
              state.mode := App_state.ChordProgression new_pstate;
              state.context.highlight_source <- Types.Chord (root, chord);
              true
            | None -> false)
        end
        | Key { code = Up; _ } -> begin
          match cstate.chord_step with
          | App_state.ChordSelectingRoot -> true
          | App_state.ChordSelectingAlteration ->
            let new_cstate = { cstate with chord_step = App_state.ChordSelectingRoot } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
          | App_state.ChordSelectingCategory ->
            let num_cats = List.length Display.all_chord_categories in
            let new_idx = if cstate.chord_category_index > 0 then cstate.chord_category_index - 1 else num_cats - 1 in
            let new_cat = List.nth Display.all_chord_categories new_idx in
            let new_cstate = { cstate with
              chord_category_index = new_idx;
              chord_category = new_cat;
              chord_index = 0
            } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
          | App_state.ChordSelectingType ->
            let chords = Display.chords_in_category cstate.chord_category in
            let num_chords = List.length chords in
            let new_idx = if cstate.chord_index > 0 then cstate.chord_index - 1 else num_chords - 1 in
            let new_cstate = { cstate with chord_index = new_idx } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
        end
        | Key { code = Down; _ } -> begin
          match cstate.chord_step with
          | App_state.ChordSelectingRoot ->
            let new_cstate = { cstate with chord_step = App_state.ChordSelectingAlteration } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
          | App_state.ChordSelectingAlteration ->
            (match cstate.chord_root with
            | Some _ ->
              let new_cstate = { cstate with chord_step = App_state.ChordSelectingCategory } in
              let new_state = { pstate with prog_add_state = Some new_cstate } in
              state.mode := App_state.ChordProgression new_state;
              true
            | None -> true)
          | App_state.ChordSelectingCategory ->
            let num_cats = List.length Display.all_chord_categories in
            let new_idx = (cstate.chord_category_index + 1) mod num_cats in
            let new_cat = List.nth Display.all_chord_categories new_idx in
            let new_cstate = { cstate with
              chord_category_index = new_idx;
              chord_category = new_cat;
              chord_index = 0
            } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
          | App_state.ChordSelectingType ->
            let chords = Display.chords_in_category cstate.chord_category in
            let num_chords = List.length chords in
            let new_idx = (cstate.chord_index + 1) mod num_chords in
            let new_cstate = { cstate with chord_index = new_idx } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
        end
        | Key { code = Left; _ } -> begin
          match cstate.chord_step with
          | App_state.ChordSelectingCategory | App_state.ChordSelectingType ->
            let new_step = match cstate.chord_step with
              | App_state.ChordSelectingType -> App_state.ChordSelectingCategory
              | App_state.ChordSelectingCategory -> App_state.ChordSelectingAlteration
              | step -> step
            in
            let new_cstate = { cstate with chord_step = new_step } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
          | _ -> false
        end
        | Key { code = Right; _ } -> begin
          match cstate.chord_step with
          | App_state.ChordSelectingAlteration ->
            (match cstate.chord_root with
            | Some _ ->
              let new_cstate = { cstate with chord_step = App_state.ChordSelectingCategory } in
              let new_state = { pstate with prog_add_state = Some new_cstate } in
              state.mode := App_state.ChordProgression new_state;
              true
            | None -> false)
          | App_state.ChordSelectingCategory ->
            let new_cstate = { cstate with chord_step = App_state.ChordSelectingType } in
            let new_state = { pstate with prog_add_state = Some new_cstate } in
            state.mode := App_state.ChordProgression new_state;
            true
          | _ -> false
        end
        | _ -> false
      end
    end
    | App_state.ProgRomanInput -> begin
      match event with
      | Key { code = Escape; _ } ->
        (* Cancel and return to navigating *)
        let new_state = { pstate with
          prog_input_mode = App_state.ProgNavigating;
          prog_roman_buffer = ""
        } in
        state.mode := App_state.ChordProgression new_state;
        true
      | Key { code = Enter; _ } ->
        (* Parse roman numerals and add chords *)
        let parsed = Display.parse_roman_progression
          state.context.scale state.context.root_note pstate.prog_roman_buffer in
        if List.length parsed > 0 then begin
          (* Convert to progression_chords *)
          let new_prog_chords = List.map (fun (root, chord, roman) ->
            { App_state.prog_root = root
            ; prog_chord = chord
            ; prog_roman = Some roman
            }
          ) parsed in
          (* Insert at cursor position *)
          let before, after =
            let rec split i acc = function
              | [] -> (List.rev acc, [])
              | x :: xs ->
                if i = 0 then (List.rev acc, x :: xs)
                else split (i - 1) (x :: acc) xs
            in
            split pstate.prog_cursor [] pstate.prog_chords
          in
          let new_chords = before @ new_prog_chords @ after in
          let new_cursor = pstate.prog_cursor + List.length new_prog_chords in
          let new_state = { pstate with
            prog_chords = new_chords;
            prog_cursor = new_cursor;
            prog_input_mode = App_state.ProgNavigating;
            prog_roman_buffer = ""
          } in
          state.mode := App_state.ChordProgression new_state;
          (* Highlight last added chord *)
          if List.length new_prog_chords > 0 then begin
            let last = List.nth new_prog_chords (List.length new_prog_chords - 1) in
            state.context.highlight_source <- Types.Chord (last.prog_root, last.prog_chord)
          end
        end;
        true
      | Key { code = Backspace; _ } ->
        (* Remove last character *)
        let len = String.length pstate.prog_roman_buffer in
        if len > 0 then begin
          let new_buffer = String.sub pstate.prog_roman_buffer 0 (len - 1) in
          let new_state = { pstate with prog_roman_buffer = new_buffer } in
          state.mode := App_state.ChordProgression new_state
        end;
        true
      | Key { code = Char c; _ } ->
        let ch_code = Uchar.to_int c in
        if ch_code > 127 then false
        else begin
        let ch = Char.chr ch_code in
        (* Allow roman numeral characters and spaces *)
        if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
           (ch >= '0' && ch <= '9') || ch = ' ' || ch = '#' || ch = 'b' then begin
          let new_buffer = pstate.prog_roman_buffer ^ String.make 1 ch in
          let new_state = { pstate with prog_roman_buffer = new_buffer } in
          state.mode := App_state.ChordProgression new_state;
          true
        end
        else false
        end
      | _ -> false
    end
    | App_state.ProgBrowsingLibrary -> begin
      match event with
      | Key { code = Escape; _ } ->
        (* Cancel and return to navigating *)
        let new_state = { pstate with prog_input_mode = App_state.ProgNavigating } in
        state.mode := App_state.ChordProgression new_state;
        true
      | Key { code = Up; _ } ->
        let num_progs = List.length Display.common_progressions in
        let new_idx = if pstate.prog_library_index > 0
          then pstate.prog_library_index - 1
          else num_progs - 1 in
        let new_state = { pstate with prog_library_index = new_idx } in
        state.mode := App_state.ChordProgression new_state;
        true
      | Key { code = Down; _ } ->
        let num_progs = List.length Display.common_progressions in
        let new_idx = (pstate.prog_library_index + 1) mod num_progs in
        let new_state = { pstate with prog_library_index = new_idx } in
        state.mode := App_state.ChordProgression new_state;
        true
      | Key { code = Enter; _ } ->
        (* Load selected progression *)
        let (_, pattern, _) = List.nth Display.common_progressions pstate.prog_library_index in
        let parsed = Display.parse_roman_progression
          state.context.scale state.context.root_note pattern in
        let new_prog_chords = List.map (fun (root, chord, roman) ->
          { App_state.prog_root = root
          ; prog_chord = chord
          ; prog_roman = Some roman
          }
        ) parsed in
        let new_state = { pstate with
          prog_chords = new_prog_chords;
          prog_cursor = 0;
          prog_input_mode = App_state.ProgNavigating
        } in
        state.mode := App_state.ChordProgression new_state;
        (* Highlight first chord if any *)
        if List.length new_prog_chords > 0 then begin
          let first = List.hd new_prog_chords in
          state.context.highlight_source <- Types.Chord (first.prog_root, first.prog_chord)
        end;
        true
      | _ -> false
    end
  end
  | _ -> false

(** Chord progression widget - displays progression horizontally *)
class progression_widget (pstate : App_state.progression_state) (app_state : App_state.t) =
  object
    inherit LTerm_widget.t "progression"

    method! size_request =
      { LTerm_geom.rows = 20; cols = 90 }

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

      (* Title with mode indicator *)
      let title = match pstate.prog_input_mode with
        | App_state.ProgNavigating -> "CHORD PROGRESSION"
        | App_state.ProgAddingChord -> "CHORD PROGRESSION - Adding Chord"
        | App_state.ProgRomanInput -> "CHORD PROGRESSION - Roman Input"
        | App_state.ProgBrowsingLibrary -> "CHORD PROGRESSION - Library"
      in
      draw_line title { none with bold = Some true; foreground = Some lcyan };
      incr row;

      (* Current key context *)
      let key_str = Fmt.str "Key: %s %s"
        (Pp.NOTES.FMT.sprint_note app_state.context.root_note)
        (Display.scale_name app_state.context.scale) in
      draw_line key_str { none with foreground = Some lyellow };

      (* Draw based on mode *)
      begin match pstate.prog_input_mode with
      | App_state.ProgNavigating | App_state.ProgAddingChord ->
        (* Show progression horizontally *)
        if List.length pstate.prog_chords = 0 then
          draw_line "(empty - press 'a' to add, 'l' for library, 'r' for Roman input)"
            { none with foreground = Some white }
        else begin
          (* Build chord string *)
          let chord_strs = List.mapi (fun i chord ->
            let roman_str = match chord.App_state.prog_roman with
              | Some r -> r
              | None -> "?"
            in
            let note_str = Pp.NOTES.FMT.sprint_note chord.prog_root in
            let chord_name = Display.chord_name chord.prog_chord in
            let display = Fmt.str "%s:%s%s" roman_str note_str chord_name in
            if i = pstate.prog_cursor then
              Fmt.str "[%s]" display
            else
              display
          ) pstate.prog_chords in
          let line = String.concat " | " chord_strs in
          draw_line line { none with foreground = Some lgreen }
        end;

        (* Show add chord picker if in AddingChord mode *)
        if pstate.prog_input_mode = App_state.ProgAddingChord then begin
          incr row;
          match pstate.prog_add_state with
          | Some cstate ->
            let root_str = match cstate.chord_root with
              | Some note -> Pp.NOTES.FMT.sprint_note note
              | None -> "..."
            in
            let category_str = Display.chord_category_name cstate.chord_category in
            let chords = Display.chords_in_category cstate.chord_category in
            let chord = List.nth chords cstate.chord_index in
            let chord_str = Display.chord_name chord in

            (* Show current selection with highlighting for active field *)
            let root_style = match cstate.chord_step with
              | App_state.ChordSelectingRoot | App_state.ChordSelectingAlteration ->
                  { none with bold = Some true; foreground = Some lgreen }
              | _ -> { none with foreground = Some white }
            in
            let category_style = match cstate.chord_step with
              | App_state.ChordSelectingCategory ->
                  { none with bold = Some true; foreground = Some lyellow }
              | _ -> { none with foreground = Some white }
            in
            let chord_style = match cstate.chord_step with
              | App_state.ChordSelectingType ->
                  { none with bold = Some true; foreground = Some lmagenta }
              | _ -> { none with foreground = Some white }
            in
            draw_line (Fmt.str "Root: %s" root_str) root_style;
            draw_line (Fmt.str "Category: %s" category_str) category_style;
            draw_line (Fmt.str "Chord: %s" chord_str) chord_style;

            (* Show instructions and lists based on step *)
            begin match cstate.chord_step with
            | App_state.ChordSelectingRoot ->
              draw_line "> Press a-g to set root note" { none with foreground = Some lblue }
            | App_state.ChordSelectingAlteration ->
              draw_line "> Press # (sharp), b (flat), or Enter to continue"
                { none with foreground = Some lblue }
            | App_state.ChordSelectingCategory ->
              draw_line "> Up/Down to browse categories:" { none with foreground = Some lblue };
              List.iteri (fun i cat ->
                let name = Display.chord_category_name cat in
                let prefix = if i = cstate.chord_category_index then "> " else "  " in
                let style = if i = cstate.chord_category_index then
                  { none with bold = Some true; foreground = Some lyellow }
                else
                  { none with foreground = Some white }
                in
                draw_line (Fmt.str "%s%s" prefix name) style
              ) Display.all_chord_categories
            | App_state.ChordSelectingType ->
              draw_line "> Up/Down to browse chords, Enter to add:" { none with foreground = Some lblue };
              List.iteri (fun i c ->
                let name = Display.chord_name c in
                let prefix = if i = cstate.chord_index then "> " else "  " in
                let style = if i = cstate.chord_index then
                  { none with bold = Some true; foreground = Some lmagenta }
                else
                  { none with foreground = Some white }
                in
                draw_line (Fmt.str "%s%s" prefix name) style
              ) chords
            end
          | None -> ()
        end

      | App_state.ProgRomanInput ->
        (* Show Roman numeral input buffer *)
        draw_line (Fmt.str "> %s_" pstate.prog_roman_buffer)
          { none with foreground = Some lgreen };
        draw_line "Type: I ii III IV V vi vii (Enter to add, Esc to cancel)"
          { none with foreground = Some lblue }

      | App_state.ProgBrowsingLibrary ->
        (* Show library list - all 10 progressions *)
        incr row;
        List.iteri (fun i (name, pattern, genre) ->
          let prefix = if i = pstate.prog_library_index then "> " else "  " in
          let style = if i = pstate.prog_library_index then
            { none with bold = Some true; foreground = Some lgreen }
          else
            { none with foreground = Some white }
          in
          draw_line (Fmt.str "%s%s (%s): %s" prefix name genre pattern) style
        ) Display.common_progressions
      end;

      incr row;
      (* Instructions based on mode *)
      let instructions = match pstate.prog_input_mode with
        | App_state.ProgNavigating ->
          "</>:move  a:add  r:roman  l:library  d:del  c:clear  1-7:quick  Esc:exit"
        | App_state.ProgAddingChord ->
          "a-g:root  #/b:alter  Up/Down:browse  Enter:add  Esc:cancel"
        | App_state.ProgRomanInput ->
          "Type Roman numerals (I ii IV V...)  Enter:add  Esc:cancel"
        | App_state.ProgBrowsingLibrary ->
          "Up/Down:browse  Enter:load  Esc:cancel"
      in
      draw_line instructions { none with foreground = Some lcyan }

    method! can_focus = false
  end

