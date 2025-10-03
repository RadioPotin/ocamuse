(** Arpeggio mode functionality *)

(** Handle arpeggio mode input *)
let handle_input state event =
  let open LTerm_event in
  match !(state.App_state.mode) with
  | App_state.ArpeggioMode astate -> begin
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
            let (root, chord_type) = List.nth chords idx in
            (* Convert diatonic_triad to chord for arpeggio *)
            let chord_for_arp : Types.chord = match chord_type with
              | Types.Major -> Major
              | Types.Minor -> Minor
              | Types.Diminished -> Dimin
            in
            (* Set highlight_source to show arpeggio notes on fretboard *)
            state.context.highlight_source <- Types.Arpeggio (root, chord_for_arp);
            (* Update display mode to show pattern *)
            let view = match !(state.context.display_mode) with
              | Types.Flat v -> v
              | Types.Pattern (v, _) -> v
            in
            state.context.display_mode := Types.Pattern (view, state.context.mode);
            let new_state = { astate with arp_chord = Some (root, chord_for_arp) } in
            state.mode := App_state.ArpeggioMode new_state;
            true
          end
          else false
        end
        else false
      end
    | _ -> false
  end
  | _ -> false
