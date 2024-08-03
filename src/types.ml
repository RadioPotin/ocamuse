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
type color_plain_view_event =
  | Up
  | Down
  | Left
  | Right
  | Interline

type view =
  | Plain of color_plain_view_event
  | Pattern of mode

type fretboard = ( int * note array ) array
