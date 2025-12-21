(** Diatonic modes representing the seven rotations of the major scale.

    Each mode has a unique intervallic pattern and characteristic sound:
    - Ionian (I): Bright, major tonality
    - Dorian (II): Minor with raised 6th
    - Phrygian (III): Dark, Spanish/flamenco character
    - Lydian (IV): Bright, dreamy, raised 4th
    - Mixolydian (V): Dominant, bluesy
    - Aeolian (VI): Natural minor
    - Locrian (VII): Diminished, unstable
*)

type t =
  | Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian

(** {1 Construction} *)

val all : t list
(** All seven modes in order from brightest (Lydian) to darkest (Locrian) *)

val of_degree : int -> t option
(** [of_degree n] returns the mode for scale degree n (0=Ionian, 1=Dorian, ..., 6=Locrian) *)

val of_degree_exn : int -> t
(** [of_degree_exn n] returns the mode, raises Invalid_argument if n not in [0,6] *)

(** {1 Intervallic structure} *)

val intervals : t -> Interval.t list
(** [intervals mode] returns the interval pattern from root to each scale degree *)

val semitones : t -> int list
(** [semitones mode] returns the semitone pattern (e.g., [2; 2; 1; 2; 2; 2; 1] for Ionian) *)

(** {1 Mode relationships} *)

val to_degree : t -> int
(** [to_degree mode] returns the scale degree (0-6) *)

val rotate : t -> int -> t
(** [rotate mode n] rotates mode by n steps. E.g., rotate Ionian 1 = Dorian *)

val relative_to_major : t -> int
(** [relative_to_major mode] returns rotation count from Ionian.
    E.g., Aeolian is 5 rotations from Ionian (relative minor) *)

(** {1 Brightness ordering (Circle of Fifths)} *)

val brightness : t -> int
(** [brightness mode] returns brightness score from -3 (darkest) to +3 (brightest).
    Lydian=3, Ionian=2, Mixolydian=1, Dorian=0, Aeolian=-1, Phrygian=-2, Locrian=-3

    This ordering follows the circle of fifths: each step adds a sharp (or removes a flat).
*)

val of_brightness : int -> t option
(** [of_brightness n] returns mode with brightness n, if n in [-3, 3] *)

val brighter : t -> t option
(** [brighter mode] returns the next brighter mode (add a sharp), if exists *)

val darker : t -> t option
(** [darker mode] returns the next darker mode (add a flat), if exists *)

(** {1 Modal characteristics} *)

val characteristic_interval : t -> Interval.t
(** [characteristic_interval mode] returns the interval that most defines the mode.
    E.g., Lydian = Augmented 4th, Phrygian = Minor 2nd *)

val is_major : t -> bool
(** [is_major mode] true if mode has major third (Ionian, Lydian, Mixolydian) *)

val is_minor : t -> bool
(** [is_minor mode] true if mode has minor third (Dorian, Phrygian, Aeolian, Locrian) *)

(** {1 String conversion} *)

val to_string : t -> string
(** [to_string mode] returns mode name: "Ionian", "Dorian", etc. *)

val of_string : string -> (t, string) result
(** [of_string s] parses mode name (case-insensitive) *)

val to_roman : t -> string
(** [to_roman mode] returns Roman numeral: "I", "II", "III", etc. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int
