type parse_error =
  | InvalidCharacter of char
  | InvalidNoteRange of int
  | InvalidMode of string

(** Scale categories for grouping scales in the UI *)
type scale_category =
  | Diatonic
  | HarmonicMinor
  | MelodicMinor
  | PentatonicBlues
  | Symmetric
  | Ethnic
  | Bebop

(** Comprehensive scale type supporting 45+ scales *)
type scale_type =
  (* Diatonic modes - 7 scales *)
  | Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
  (* Harmonic minor modes - 7 scales *)
  | HarmonicMinor | LocrianNat6 | IonianSharp5 | DorianSharp4
  | PhrygianDominant | LydianSharp2 | SuperLocrianbb7
  (* Melodic minor modes - 7 scales *)
  | MelodicMinor | DorianFlat2 | LydianAugmented | LydianDominant
  | MixolydianFlat6 | LocrianNat2 | Altered
  (* Pentatonic & Blues - 6 scales *)
  | MajorPentatonic | MinorPentatonic | BluesMajor | BluesMinor
  | Hirajoshi | InSen
  (* Symmetric - 6 scales *)
  | WholeTone | DiminishedHW | DiminishedWH | Chromatic
  | Augmented | Tritone
  (* Ethnic/Exotic - 8 scales *)
  | HungarianMinor | DoubleHarmonic | Persian | Arabian
  | Japanese | Egyptian | NeapolitanMinor | NeapolitanMajor
  (* Bebop - 4 scales *)
  | BebopDominant | BebopMajor | BebopMinor | BebopDorian

(** Legacy mode type - alias for diatonic scales for backward compatibility *)
type mode =
  | C_mode  (* Ionian *)
  | D_mode  (* Dorian *)
  | E_mode  (* Phrygian *)
  | F_mode  (* Lydian *)
  | G_mode  (* Mixolydian *)
  | A_mode  (* Aeolian *)
  | B_mode  (* Locrian *)

(** [base_note] is one of the 7 alternatives in the Western notation *)
type base_note =
  | A
  | B
  | C
  | D
  | E
  | F
  | G

(** [note] is a record of a base_note and an alteration represented by an int
    which distance to 0 equals *)
type note =
  { base : base_note
  ; alteration : int
  }

type chord =
  | Major
  | Minor
  | Dimin
  | Augment
  | Suspend2
  | Suspend4
  | Major7
  | Domin7
  | Minor7
  | HalfDim7
  | Sixth
  | MinorSixth

type diatonic_triad =
  | Major
  | Minor
  | Diminished

type tuning = note list

(* temporary display mode for lambda term itf *)
type fullview_color_options =
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | Lred
  | Lgreen
  | Lyellow
  | Lblue
  | Lmagenta
  | Lcyan

type base_colour =
  | Black
  | Lblack
  | White
  | Lwhite

type view =
  | Plain of base_colour
  | Fretted of base_colour
  | Interline of base_colour

type fretboard = (int * note array) array

type fretboard_data =
  { notes : note array array
  ; coord_lookup : (int * int, note) Hashtbl.t
  }

type display =
  | Flat of view
  | Pattern of view * scale_type

type highlight_source =
  | Tonality of scale_type * note
  | Chord of note * chord
  | Arpeggio of note * chord

type color_theme =
  | ChromaticGradient  (* Maps 12 pitch classes to color wheel *)
  | DiatonicDegrees    (* Traditional degree-based coloring *)
  | CustomPalette of string  (* Custom palette by name *)

type pattern_view_draw_struc =
  { view : view
  ; scale : scale_type
  ; string : int ref
  ; offset : int ref
  ; cursor_i : int ref
  ; cursor_j : int ref
  ; number_of_frets : int
  ; number_of_strings : int
  ; ctx : LTerm_draw.context
  ; color : LTerm_style.color
  ; fretboard : note array array
  ; notes_to_degree_tbl : (int, int) Hashtbl.t  (* pitch class -> degree *)
  ; degree_to_color_tbl : (int, LTerm_style.color) Hashtbl.t
  }

type ocamuse_structure =
  { display_mode : display ref
  ; mutable fretboard : fretboard_data
  ; base_colour : base_colour ref
  ; mutable tuning : tuning
  ; mutable root_note : note
  ; mutable scale : scale_type
  ; mutable highlight_source : highlight_source
  ; mutable color_theme : color_theme
  }
