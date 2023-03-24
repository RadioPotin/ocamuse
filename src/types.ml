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

module Fretboard = struct
  type note =
    | A
    | Ash_Bfl
    | B
    | C
    | Csh_Dfl
    | D
    | Dsh_Efl
    | E
    | F
    | Fsh_Gfl
    | G
    | Gsh_Afl

  type tuning = note list
end
