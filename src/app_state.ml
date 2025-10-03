(** Application state machine for managing interactive modes *)

(** Application modes - each mode has different keyboard shortcuts and behavior *)
type app_mode =
  | Normal  (** Default mode - fretboard display and basic controls *)
  | Help  (** Help overlay showing keyboard shortcuts *)
  | TonalitySelection of tonality_state  (** Selecting root note and mode *)
  | TuningSelection of tuning_state  (** Selecting guitar tuning *)
  | PaletteSelection of palette_state  (** Selecting color palette *)
  | ChordMode of chord_state  (** Displaying chord diagrams *)
  | ArpeggioMode of arpeggio_state  (** Showing arpeggio patterns *)
  | ProgressionMode of progression_state  (** Building chord progressions *)

(** State for tonality selection *)
and tonality_state =
  { temp_root : Types.note option  (** Temporary root note being built *)
  ; temp_mode : Types.mode option  (** Temporary mode selection *)
  ; selection_step : selection_step  (** Current step in selection process *)
  }

(** State for tuning selection *)
and tuning_state =
  { available_tunings : (string * Types.tuning) list  (** List of preset tunings *)
  ; selected_index : int  (** Currently highlighted tuning *)
  ; custom_editing : bool  (** Whether editing custom tuning *)
  }

(** State for palette selection *)
and palette_state =
  { available_palettes : Config.Palettes.palette list  (** List of available palettes *)
  ; palette_index : int  (** Currently highlighted palette *)
  }

(** State for chord mode *)
and chord_state =
  { selected_chord : (Types.note * Types.diatonic_triad) option
  ; inversion : int  (** 0 = root position, 1 = 1st inversion, 2 = 2nd inversion *)
  ; voicings : Theory.chord_voicing list  (** Available voicings for the selected chord *)
  ; voicing_index : int  (** Currently selected voicing (0-based) *)
  }

(** State for arpeggio mode *)
and arpeggio_state =
  { arp_chord : (Types.note * Types.chord) option
  ; arp_pattern : arpeggio_pattern
  ; arp_speed : int  (** Animation speed *)
  }

(** State for progression builder *)
and progression_state =
  { progression : int list  (** List of scale degrees (0-6) *)
  ; current_position : int  (** Current chord being played/edited *)
  ; preview_mode : bool  (** Whether previewing the progression *)
  }

(** Selection steps for tonality *)
and selection_step =
  | SelectingRoot  (** Choosing root note (A-G) *)
  | SelectingAlteration  (** Choosing sharp/flat *)
  | SelectingMode  (** Choosing mode (1-7) *)

(** Arpeggio patterns *)
and arpeggio_pattern =
  | Ascending
  | Descending
  | Alternating
  | Random

(** Focus state for tab navigation *)
type focus_target =
  | FretboardFocus
  | ContextPanelFocus
  | HelpPanelFocus
  | SelectorFocus

(** Complete application state *)
type t =
  { mode : app_mode ref
  ; focus : focus_target ref
  ; context : Types.ocamuse_structure
  ; help_visible : bool ref
  ; keybindings_modal_visible : bool ref
  ; debug_mode : bool ref
  }

(** Create initial application state *)
let make context =
  { mode = ref Normal
  ; focus = ref FretboardFocus
  ; context
  ; help_visible = ref false
  ; keybindings_modal_visible = ref false
  ; debug_mode = ref false
  }

(** State transitions *)
let enter_help state =
  state.mode := Help;
  state.help_visible := true

let exit_help state =
  state.mode := Normal;
  state.help_visible := false

let enter_tonality_selection state =
  state.mode :=
    TonalitySelection
      { temp_root = None; temp_mode = None; selection_step = SelectingRoot }

let enter_tuning_selection state =
  let tunings =
    [ ("Standard 6-string", Config.Tunings.to_tuning Config.Tunings.standard_6)
    ; ("Drop D 6-string", Config.Tunings.to_tuning Config.Tunings.drop_d_6)
    ; ("Standard 7-string", Config.Tunings.to_tuning Config.Tunings.standard_7)
    ; ("Drop A 7-string", Config.Tunings.to_tuning Config.Tunings.drop_a_7)
    ; ("Open G", Config.Tunings.to_tuning Config.Tunings.open_g)
    ; ("Open D", Config.Tunings.open_d)
    ]
  in
  state.mode :=
    TuningSelection
      { available_tunings = tunings; selected_index = 0; custom_editing = false }

let enter_chord_mode state =
  state.mode :=
    ChordMode
      { selected_chord = None; inversion = 0; voicings = []; voicing_index = 0 }

let enter_arpeggio_mode state =
  state.mode :=
    ArpeggioMode { arp_chord = None; arp_pattern = Ascending; arp_speed = 500 }

let enter_progression_mode state =
  state.mode :=
    ProgressionMode
      { progression = []; current_position = 0; preview_mode = false }

let return_to_normal state = state.mode := Normal

(** Debug mode **)
let toggle_debug state = state.debug_mode := not !(state.debug_mode)

(** Keybindings modal **)
let toggle_keybindings_modal state =
  state.keybindings_modal_visible := not !(state.keybindings_modal_visible)

(** Focus management *)
let cycle_focus state =
  state.focus :=
    match !(state.focus) with
    | FretboardFocus -> ContextPanelFocus
    | ContextPanelFocus -> HelpPanelFocus
    | HelpPanelFocus -> SelectorFocus
    | SelectorFocus -> FretboardFocus

(** Check if in normal mode *)
let is_normal state = match !(state.mode) with Normal -> true | _ -> false

(** Get current mode name for display *)
let mode_name state =
  match !(state.mode) with
  | Normal -> "Normal"
  | Help -> "Help"
  | TonalitySelection _ -> "Tonality Selection"
  | TuningSelection _ -> "Tuning Selection"
  | PaletteSelection _ -> "Palette Selection"
  | ChordMode _ -> "Chord Mode"
  | ArpeggioMode _ -> "Arpeggio Mode"
  | ProgressionMode _ -> "Progression Mode"
