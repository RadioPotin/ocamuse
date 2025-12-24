(** Synchronization system between panels and main fretboard *)

open Music_theory
open Panel

(** Events that can trigger synchronization *)
type sync_event =
  | KeyChanged of Scales.Scale.t
  | ChordSelected of Harmony.Chord.t
  | NoteHighlighted of Core.Note.t list
  | TuningChanged of Core.Note.t array
  | FretboardCleared

(** Main fretboard state *)
type fretboard_state = {
  current_key: Scales.Scale.t option;
  current_chord: Harmony.Chord.t option;
  highlighted_notes: Core.Note.t list;
  tuning: Core.Note.t array;
}

(** Sync context containing all state *)
type sync_context = {
  fretboard: fretboard_state;
  panels: panel list;
}

(** Create initial fretboard state *)
let initial_fretboard_state = {
  current_key = None;
  current_chord = None;
  highlighted_notes = [];
  tuning = [| Core.Note.e; Core.Note.a; Core.Note.d;
              Core.Note.g; Core.Note.b; Core.Note.e |];
}

(** Apply event to fretboard state *)
let apply_to_fretboard event fretboard =
  match event with
  | KeyChanged key ->
      { fretboard with current_key = Some key }
  | ChordSelected chord ->
      { fretboard with current_chord = Some chord }
  | NoteHighlighted notes ->
      { fretboard with highlighted_notes = notes }
  | TuningChanged tuning ->
      { fretboard with tuning }
  | FretboardCleared ->
      { fretboard with
        current_chord = None;
        highlighted_notes = [];
      }

(** Update harmony panel content from fretboard state *)
let sync_harmony_panel (fretboard : fretboard_state) (harmony_content : harmony_content) : harmony_content =
  { harmony_content with
    current_key = fretboard.current_key;
    current_chord = fretboard.current_chord;
  }

(** Update theory panel content from fretboard state *)
let sync_theory_panel fretboard theory_content =
  match fretboard.current_key with
  | None -> theory_content
  | Some key ->
      { theory_content with
        circle_position = key.root;
      }

(** Update scale panel content from fretboard state *)
let sync_scale_panel fretboard scale_content =
  { scale_content with
    scale = fretboard.current_key;
  }

(** Update chord diagram panel from fretboard state *)
let sync_diagram_panel fretboard diagram_content =
  { diagram_content with
    chord = fretboard.current_chord;
    tuning = fretboard.tuning;
  }

(** Sync panel content based on sync mode and event *)
let sync_panel_content fretboard panel =
  match panel.sync_mode with
  | Watch ->
      (* Watch mode: always mirror fretboard state *)
      (match panel.content with
      | HarmonyContent hc ->
          HarmonyContent (sync_harmony_panel fretboard hc)
      | TheoryContent _tc ->
          TheoryContent (sync_theory_panel fretboard _tc)
      | ScaleContent sc ->
          ScaleContent (sync_scale_panel fretboard sc)
      | DiagramContent dc ->
          DiagramContent (sync_diagram_panel fretboard dc)
      | ComparisonContent _ | PatternContent _ -> panel.content)
  | Locked ->
      (* Locked mode: sync when panel is focused *)
      if panel.focused then
        match panel.content with
        | HarmonyContent hc ->
            HarmonyContent (sync_harmony_panel fretboard hc)
        | _ -> panel.content
      else
        panel.content
  | Unlocked ->
      (* Unlocked mode: never auto-sync *)
      panel.content

(** Propagate event to all panels that need syncing *)
let propagate_event event ctx =
  (* Update fretboard state *)
  let new_fretboard = apply_to_fretboard event ctx.fretboard in

  (* Update panels based on their sync mode *)
  let new_panels = List.map (fun panel ->
    let new_content = sync_panel_content new_fretboard panel in
    { panel with content = new_content }
  ) ctx.panels in

  { fretboard = new_fretboard; panels = new_panels }

(** Apply panel content to fretboard (for Unlocked mode "Apply" action) *)
let apply_panel_to_fretboard panel fretboard =
  match panel.content with
  | HarmonyContent hc ->
      let fb = match hc.current_key with
        | Some key -> { fretboard with current_key = Some key }
        | None -> fretboard
      in
      (match hc.current_chord with
      | Some chord -> { fb with current_chord = Some chord }
      | None -> fb)

  | TheoryContent _tc ->
      (* Theory view might have selected a different key *)
      fretboard  (* TODO: implement when theory view can change keys *)

  | ScaleContent sc ->
      (match sc.scale with
      | Some scale -> { fretboard with current_key = Some scale }
      | None -> fretboard)

  | DiagramContent dc ->
      let fb = match dc.chord with
        | Some chord -> { fretboard with current_chord = Some chord }
        | None -> fretboard
      in
      { fb with tuning = dc.tuning }

  | _ -> fretboard

(** Get preview of what would happen if panel content was applied *)
let preview_panel_application panel fretboard =
  let preview_fretboard = apply_panel_to_fretboard panel fretboard in

  (* Generate description of changes *)
  let changes = ref [] in

  if preview_fretboard.current_key <> fretboard.current_key then
    changes := "Key change" :: !changes;

  if preview_fretboard.current_chord <> fretboard.current_chord then
    changes := "Chord change" :: !changes;

  if preview_fretboard.tuning <> fretboard.tuning then
    changes := "Tuning change" :: !changes;

  (!changes, preview_fretboard)

(** Extract events from panel content changes *)
let extract_events old_content new_content =
  let events = ref [] in

  (match (old_content, new_content) with
  | (HarmonyContent old_hc, HarmonyContent new_hc) ->
      if old_hc.current_key <> new_hc.current_key then
        Option.iter (fun key -> events := KeyChanged key :: !events) new_hc.current_key;
      if old_hc.current_chord <> new_hc.current_chord then
        Option.iter (fun chord -> events := ChordSelected chord :: !events) new_hc.current_chord;
  | _ -> ());

  !events

(** Commit panel changes to context (for Locked mode) *)
let commit_panel_changes panel old_panel ctx =
  if panel.sync_mode = Locked then (
    (* Extract events from content changes *)
    let events = extract_events old_panel.content panel.content in

    (* Apply each event *)
    List.fold_left (fun ctx event ->
      propagate_event event ctx
    ) ctx events
  ) else
    ctx

(** Toggle panel sync mode *)
let cycle_sync_mode panel =
  let new_mode = match panel.sync_mode with
    | Locked -> Unlocked
    | Unlocked -> Watch
    | Watch -> Locked
  in
  { panel with sync_mode = new_mode }

(** Get list of panels that are out of sync with fretboard *)
let get_out_of_sync_panels ctx =
  List.filter (fun panel ->
    panel.sync_mode = Unlocked &&
    panel.visible &&
    (match panel.content with
    | HarmonyContent hc ->
        hc.current_key <> ctx.fretboard.current_key ||
        hc.current_chord <> ctx.fretboard.current_chord
    | ScaleContent sc ->
        sc.scale <> ctx.fretboard.current_key
    | DiagramContent dc ->
        dc.chord <> ctx.fretboard.current_chord
    | _ -> false)
  ) ctx.panels
