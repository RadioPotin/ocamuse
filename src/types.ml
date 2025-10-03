type parse_error =
  | InvalidCharacter of char
  | InvalidNoteRange of int
  | InvalidMode of string

type mode =
  | C_mode
  | D_mode
  | E_mode
  | F_mode
  | G_mode
  | A_mode
  | B_mode

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
  | Pattern of view * mode

type highlight_source =
  | Tonality of mode * note
  | Chord of note * chord
  | Arpeggio of note * chord

type color_theme =
  | ChromaticGradient  (* Maps 12 pitch classes to color wheel *)
  | DiatonicDegrees    (* Traditional degree-based coloring *)
  | CustomPalette of string  (* Custom palette by name *)

type pattern_view_draw_struc =
  { view : view
  ; mode : mode
  ; string : int ref
  ; offset : int ref
  ; cursor_i : int ref
  ; cursor_j : int ref
  ; number_of_frets : int
  ; number_of_strings : int
  ; ctx : LTerm_draw.context
  ; color : LTerm_style.color
  ; fretboard : note array array
  ; notes_to_degree_tbl : (note, int) Hashtbl.t
  ; degree_to_color_tbl : (int, LTerm_style.color) Hashtbl.t
  }

type ocamuse_structure =
  { display_mode : display ref
  ; mutable fretboard : fretboard_data
  ; base_colour : base_colour ref
  ; mutable tuning : tuning
  ; mutable root_note : note
  ; mutable mode : mode
  ; mutable highlight_source : highlight_source
  ; mutable color_theme : color_theme
  }
