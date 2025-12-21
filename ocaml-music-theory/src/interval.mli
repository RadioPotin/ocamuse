(** Musical intervals *)
type t =
  | Unison
  | MinorSecond
  | MajorSecond
  | MinorThird
  | MajorThird
  | PerfectFourth
  | Tritone
  | PerfectFifth
  | MinorSixth
  | MajorSixth
  | MinorSeventh
  | MajorSeventh
  | Octave

(** {1 Conversion} *)

val to_semitones : t -> int
(** [to_semitones interval] returns chromatic distance *)

val of_semitones : int -> t option
(** [of_semitones n] returns interval if n in [0, 12], None otherwise *)

val of_semitones_exn : int -> t
(** [of_semitones_exn n] returns interval, raises if invalid *)

(** {1 Operations} *)

val add : t -> t -> t option
(** [add i1 i2] adds intervals, None if result > Octave *)

val invert : t -> t
(** [invert interval] returns complement to octave.
    e.g., MajorThird -> MinorSixth *)

(** {1 Interval qualities} *)

val is_perfect : t -> bool
(** True for Unison, PerfectFourth, PerfectFifth, Octave *)

val is_major : t -> bool
(** True for Major intervals *)

val is_minor : t -> bool
(** True for Minor intervals *)

(** {1 Diatonic properties} *)

val letter_distance : t -> int
(** [letter_distance interval] returns letter-name steps (0-7).
    Used for proper note naming. E.g., MajorThird -> 2 (C to E) *)

(** {1 String conversion} *)

val to_string : t -> string
(** Human-readable: "m3", "M3", "P5", etc. *)

val all : t list
(** All 13 intervals *)
