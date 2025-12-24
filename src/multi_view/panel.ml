(** Multi-view panel system for ocamuse *)

open Music_theory

(** Synchronization mode for a panel *)
type sync_mode =
  | Locked      (** Changes in panel apply to main fretboard immediately *)
  | Unlocked    (** Panel has independent state, requires explicit "apply" action *)
  | Watch       (** Panel mirrors main fretboard state, read-only *)

(** Rectangle for floating panels - row, col, rows, cols *)
type rect = {
  row: int;
  col: int;
  rows: int;
  cols: int;
}

(** Panel position on screen *)
type panel_position =
  | Left of int          (** Left sidebar, percentage width (0-50) *)
  | Right of int         (** Right sidebar, percentage width (0-50) *)
  | Bottom of int        (** Bottom panel, percentage height (0-40) *)
  | Floating of rect     (** Floating panel with explicit bounds *)

(** View mode determines what content the panel displays *)
type view_mode =
  | HarmonyExplorer      (** Chord suggestions & progression builder *)
  | TheoryReference      (** Circle of fifths, key relationships *)
  | ComparisonView       (** Side-by-side scale/chord comparison *)
  | PatternBuilder       (** Arpeggio pattern designer with animation *)
  | ScaleInfo            (** Detailed scale degree information *)
  | ChordDiagram         (** Chord voicings with finger positions *)

(** Content for a harmony explorer panel *)
type harmony_content = {
  current_key: Scales.Scale.t option;
  current_chord: Harmony.Chord.t option;
  suggestions: (Harmony.Chord.t * string * int) list;  (* chord, description, strength *)
  progression: Harmony.Chord.t list;
  selected_suggestion: int option;
}

(** Content for a theory reference panel *)
type theory_content = {
  circle_position: Core.Note.t;
  show_relationships: bool;
  highlighted_keys: Core.Note.t list;
}

(** Content for a comparison view panel *)
type comparison_content = {
  left_scale: Scales.Scale.t option;
  right_scale: Scales.Scale.t option;
  show_differences: bool;
  show_common_tones: bool;
}

(** Content for a pattern builder panel *)
type pattern_content = {
  base_chord: Harmony.Chord.t option;
  pattern_type: [ `Ascending | `Descending | `Alternating | `Custom ];
  octave_range: int;
  animation_speed: float;
  current_frame: int;
}

(** Content for a scale info panel *)
type scale_content = {
  scale: Scales.Scale.t option;
  show_intervals: bool;
  show_degrees: bool;
  show_triads: bool;
  show_sevenths: bool;
}

(** Content for a chord diagram panel *)
type chord_diagram_content = {
  chord: Harmony.Chord.t option;
  voicing: int list option;  (* fret positions for each string *)
  show_fingering: bool;
  tuning: Core.Note.t array;
}

(** Panel content tagged by view mode *)
type panel_content =
  | HarmonyContent of harmony_content
  | TheoryContent of theory_content
  | ComparisonContent of comparison_content
  | PatternContent of pattern_content
  | ScaleContent of scale_content
  | DiagramContent of chord_diagram_content

(** A panel in the multi-view system *)
type panel = {
  id: string;
  mode: view_mode;
  position: panel_position;
  content: panel_content;
  sync_mode: sync_mode;
  visible: bool;
  focused: bool;
}

(** Create a new panel with default content for the given view mode *)
let create_default_content mode =
  match mode with
  | HarmonyExplorer ->
      HarmonyContent {
        current_key = None;
        current_chord = None;
        suggestions = [];
        progression = [];
        selected_suggestion = None;
      }
  | TheoryReference ->
      TheoryContent {
        circle_position = Core.Note.c;
        show_relationships = true;
        highlighted_keys = [];
      }
  | ComparisonView ->
      ComparisonContent {
        left_scale = None;
        right_scale = None;
        show_differences = true;
        show_common_tones = true;
      }
  | PatternBuilder ->
      PatternContent {
        base_chord = None;
        pattern_type = `Ascending;
        octave_range = 2;
        animation_speed = 1.0;
        current_frame = 0;
      }
  | ScaleInfo ->
      ScaleContent {
        scale = None;
        show_intervals = true;
        show_degrees = true;
        show_triads = false;
        show_sevenths = false;
      }
  | ChordDiagram ->
      DiagramContent {
        chord = None;
        voicing = None;
        show_fingering = true;
        tuning = [| Core.Note.e; Core.Note.a; Core.Note.d;
                    Core.Note.g; Core.Note.b; Core.Note.e |];
      }

(** Generate a unique panel ID *)
let next_panel_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Printf.sprintf "panel_%d" !counter

(** Create a new panel *)
let create mode position =
  {
    id = next_panel_id ();
    mode;
    position;
    content = create_default_content mode;
    sync_mode = Unlocked;  (* Default to unlocked for safety *)
    visible = true;
    focused = false;
  }

(** Set the synchronization mode of a panel *)
let set_sync_mode panel sync_mode =
  { panel with sync_mode }

(** Toggle panel visibility *)
let toggle_visibility panel =
  { panel with visible = not panel.visible }

(** Show a panel *)
let show panel =
  { panel with visible = true }

(** Hide a panel *)
let hide panel =
  { panel with visible = false }

(** Set panel focus *)
let set_focus panel focused =
  { panel with focused }

(** Update panel content *)
let set_content panel content =
  { panel with content }

(** Update panel position *)
let set_position panel position =
  { panel with position }

(** Get view mode name as string *)
let mode_name = function
  | HarmonyExplorer -> "Harmony Explorer"
  | TheoryReference -> "Theory Reference"
  | ComparisonView -> "Comparison View"
  | PatternBuilder -> "Pattern Builder"
  | ScaleInfo -> "Scale Info"
  | ChordDiagram -> "Chord Diagram"

(** Get sync mode name as string *)
let sync_mode_name = function
  | Locked -> "🔒 Locked"
  | Unlocked -> "🔓 Unlocked"
  | Watch -> "👁 Watch"

(** Check if panel can be synced (not Watch mode) *)
let can_sync panel =
  match panel.sync_mode with
  | Locked | Unlocked -> true
  | Watch -> false

(** Check if panel auto-syncs (Locked mode) *)
let auto_syncs panel =
  panel.sync_mode = Locked
