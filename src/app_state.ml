(** Application state machine for managing interactive modes *)

(** Application modes - each mode has different keyboard shortcuts and behavior *)
type app_mode =
  | Normal  (** Default mode - fretboard display and basic controls *)
  | TonalitySelection of tonality_state  (** Selecting root note and mode *)
  | TuningSelection of tuning_state  (** Selecting guitar tuning *)
  | ThemeSelection of theme_state  (** Selecting color theme *)
  | ChordLookup of chord_state  (** Chord lookup - find and highlight any chord *)
  | ChordProgression of progression_state  (** Building and analyzing chord progressions *)
  | MultiView of multi_view_state  (** Multi-view focus mode with multiple concurrent panels *)

(** State for tonality selection *)
and tonality_state =
  { temp_root : Types.note option  (** Temporary root note being built *)
  ; temp_scale : Types.scale_type option  (** Temporary scale selection *)
  ; temp_category : Types.scale_category  (** Current category being browsed *)
  ; category_index : int  (** Index within category list *)
  ; scale_index : int  (** Index within current category's scales *)
  ; selection_step : selection_step  (** Current step in selection process *)
  }

(** State for tuning selection *)
and tuning_state =
  { available_tunings : (string * Types.tuning) list  (** List of preset tunings *)
  ; selected_index : int  (** Currently highlighted tuning *)
  ; custom_editing : bool  (** Whether editing custom tuning *)
  }

(** State for theme selection *)
and theme_state =
  { available_themes : Types.color_theme list  (** All available themes *)
  ; theme_index : int  (** Currently highlighted theme *)
  ; preview_active : bool  (** Whether preview is being shown *)
  }

(** State for chord selection *)
and chord_state =
  { chord_root : Types.note option  (** Temporary root note being built *)
  ; chord_type : Types.chord option  (** Selected chord type *)
  ; chord_category : Types.chord_category  (** Current category being browsed *)
  ; chord_category_index : int  (** Index within category list *)
  ; chord_index : int  (** Index within current category's chords *)
  ; chord_step : chord_selection_step  (** Current step in selection process *)
  }

(** Selection steps for chord *)
and chord_selection_step =
  | ChordSelectingRoot  (** Choosing root note (A-G) *)
  | ChordSelectingAlteration  (** Choosing sharp/flat *)
  | ChordSelectingCategory  (** Choosing chord category *)
  | ChordSelectingType  (** Choosing specific chord within category *)

(** Chord in a progression with analysis *)
and progression_chord =
  { prog_root : Types.note  (** Root note of chord *)
  ; prog_chord : Types.chord  (** Chord type *)
  ; prog_roman : string option  (** Roman numeral if diatonic, None if borrowed *)
  }

(** Input modes for progression editing *)
and progression_input_mode =
  | ProgNavigating  (** Arrow keys move cursor *)
  | ProgAddingChord  (** Using chord lookup picker to add *)
  | ProgRomanInput  (** Typing Roman numerals *)
  | ProgBrowsingLibrary  (** Browsing preset progressions *)

(** State for chord progression mode *)
and progression_state =
  { prog_chords : progression_chord list  (** List of chords in progression *)
  ; prog_cursor : int  (** Index of selected chord *)
  ; prog_input_mode : progression_input_mode  (** Current input mode *)
  ; prog_roman_buffer : string  (** Text buffer for Roman input *)
  ; prog_library_index : int  (** Index in common progressions list *)
  ; prog_add_state : chord_state option  (** State for AddingChord sub-mode *)
  }

(** State for multi-view mode *)
and multi_view_state =
  { sync_context : Multi_view.Sync.sync_context  (** Synchronization state between panels *)
  ; layout_config : Multi_view.Layout.layout_config  (** Layout configuration *)
  ; focused_panel_id : string option  (** ID of currently focused panel, None = fretboard *)
  ; show_panel_controls : bool  (** Whether to show panel control bar *)
  }

(** Selection steps for tonality *)
and selection_step =
  | SelectingRoot  (** Choosing root note (A-G) *)
  | SelectingAlteration  (** Choosing sharp/flat *)
  | SelectingCategory  (** Choosing scale category *)
  | SelectingScale  (** Choosing scale within category *)

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
let enter_tonality_selection state =
  state.mode :=
    TonalitySelection
      { temp_root = None
      ; temp_scale = None
      ; temp_category = Types.Diatonic
      ; category_index = 0
      ; scale_index = 0
      ; selection_step = SelectingRoot
      }

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

let enter_multi_view state =
  (* Create initial multi-view state with no panels *)
  let sync_context =
    { Multi_view.Sync.fretboard = Multi_view.Sync.initial_fretboard_state
    ; panels = []
    }
  in
  state.mode :=
    MultiView
      { sync_context
      ; layout_config = Multi_view.Layout.default_config
      ; focused_panel_id = None
      ; show_panel_controls = true
      }

let enter_chord_lookup state =
  state.mode :=
    ChordLookup
      { chord_root = None
      ; chord_type = None
      ; chord_category = Types.Triads
      ; chord_category_index = 0
      ; chord_index = 0
      ; chord_step = ChordSelectingRoot
      }

let enter_chord_progression state =
  state.mode :=
    ChordProgression
      { prog_chords = []
      ; prog_cursor = 0
      ; prog_input_mode = ProgNavigating
      ; prog_roman_buffer = ""
      ; prog_library_index = 0
      ; prog_add_state = None
      }

let enter_theme_selection state =
  (* Build list of all themes: built-in + custom palettes *)
  let all_themes =
    Types.ChromaticGradient ::
    Types.DiatonicDegrees ::
    (List.map (fun p -> Types.CustomPalette p.Config.Palettes.name)
       Config.Palettes.all_palettes)
  in
  (* Find current theme index *)
  let current = state.context.color_theme in
  let rec find_idx i = function
    | [] -> 0
    | t :: _ when t = current -> i
    | _ :: rest -> find_idx (i + 1) rest
  in
  let current_idx = find_idx 0 all_themes in
  state.mode :=
    ThemeSelection
      { available_themes = all_themes
      ; theme_index = current_idx
      ; preview_active = true  (* Always preview *)
      }

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
  | TonalitySelection _ -> "Tonality Selection"
  | TuningSelection _ -> "Tuning Selection"
  | ThemeSelection _ -> "Theme Selection"
  | ChordLookup _ -> "Chord Lookup"
  | ChordProgression _ -> "Progression"
  | MultiView _ -> "Multi-View"
