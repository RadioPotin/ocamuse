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

type fretboard = ( int * note array ) array

type display =
  | Flat of view
  | Pattern of view * mode

type flat_view_draw_struc =
  {
    string: int ref;
    offset : int ref;
    cursor_j : int ref;
    number_of_strings: int;
    ctx : LTerm_draw.context;
    color : LTerm_style.color;
    guitar_string : (note array) ref;
    fretboard : (note array) array;
  }

type pattern_view_draw_struc =
  {
    view : view;
    mode : mode;
    string: int ref;
    offset : int ref;
    cursor_i : int ref;
    cursor_j : int ref;
    number_of_frets: int;
    number_of_strings: int;
    ctx : LTerm_draw.context;
    color : LTerm_style.color;
    fretboard : (note array) array;
    notes_to_degree_tbl: (note, int) Hashtbl.t;
    degree_to_color_tbl: (int, LTerm_style.color) Hashtbl.t;
  }

type ocamuse_structure =
  {
    display_mode : display ref;
    fretboard : (note array) array;
    base_colour : base_colour ref;
    tuning : tuning;
    root_note : note;
    mode : mode;
  }
