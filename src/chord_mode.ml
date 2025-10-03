(** Chord mode functionality *)

(** Handle chord mode input *)
let handle_input state event =
  let open LTerm_event in
  match !(state.App_state.mode) with
  | App_state.ChordMode cstate -> begin
    match event with
    | Key { code = Escape; _ } ->
      App_state.return_to_normal state;
      true
    | Key { code = Char c; _ } ->
      let ch_code = Uchar.to_int c in
      if ch_code > 127 then false
      else begin
        let ch = Char.chr ch_code in
        if ch >= '1' && ch <= '7' then begin
          let idx = Char.code ch - Char.code '1' in
          let chords = Theory.build_diatonic_triads_sequence state.context.mode state.context.root_note in
          if idx < List.length chords then begin
            let (root, chord_type_diatonic) = List.nth chords idx in
            (* Convert to chord type for theory functions *)
            let chord_type : Types.chord = match chord_type_diatonic with
              | Types.Major -> Types.Major
              | Types.Minor -> Types.Minor
              | Types.Diminished -> Types.Dimin
            in
            (* Build chord notes *)
            let base_notes = Theory.build_triad_notes root chord_type in
            (* Find voicings for this chord *)
            let voicings = Theory.find_chord_voicings state.context.fretboard base_notes in
            (* Set highlight_source to show chord notes on fretboard *)
            state.context.highlight_source <- Types.Chord (root, chord_type);
            (* Update display mode to show pattern *)
            let view = match !(state.context.display_mode) with
              | Types.Flat v -> v
              | Types.Pattern (v, _) -> v
            in
            state.context.display_mode := Types.Pattern (view, state.context.mode);
            (* Update state with voicings *)
            let new_state : App_state.chord_state = {
              selected_chord = Some (root, chord_type_diatonic);
              inversion = 0;
              voicings;
              voicing_index = 0;
            } in
            state.mode := App_state.ChordMode new_state;
            true
          end
          else false
        end
        else if ch = 'i' then begin
          (* Cycle through inversions *)
          let new_inversion = (cstate.inversion + 1) mod 3 in
          let new_state = { cstate with inversion = new_inversion } in
          state.mode := App_state.ChordMode new_state;
          true
        end
        else if ch = 'v' then begin
          (* Cycle through voicings *)
          if List.length cstate.voicings > 0 then begin
            let new_index = (cstate.voicing_index + 1) mod (List.length cstate.voicings) in
            let new_state = { cstate with voicing_index = new_index } in
            state.mode := App_state.ChordMode new_state;
            true
          end
          else false
        end
        else false
      end
    | _ -> false
  end
  | _ -> false
