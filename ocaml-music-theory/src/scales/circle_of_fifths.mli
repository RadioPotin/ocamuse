(** Circle of Fifths - the fundamental organizing principle of Western tonal music.

    The circle of fifths arranges the 12 pitch classes in perfect fifth intervals,
    revealing key signatures, mode brightness, and modulation relationships.

    Reading clockwise: each step adds a sharp (or removes a flat)
    Reading counter-clockwise: each step adds a flat (or removes a sharp)
*)

(** {1 Circle position} *)

type position = int
(** Position on the circle (0-11). C=0, G=7, F=5, etc.
    Moving clockwise (+1) = up a fifth
    Moving counter-clockwise (-1) = up a fourth *)

val of_pitch : Pitch.t -> position
(** [of_pitch p] returns circle position for pitch class *)

val to_pitch : position -> Pitch.t
(** [to_pitch pos] returns pitch class at position *)

val of_note : Note.t -> position
(** [of_note n] returns circle position for note *)

val to_note : position -> prefer:[`Sharp | `Flat] -> Note.t
(** [to_note pos ~prefer] returns note at position with preferred accidental *)

(** {1 Navigation} *)

val move_clockwise : position -> int -> position
(** [move_clockwise pos steps] moves clockwise (add sharps) by steps *)

val move_counter_clockwise : position -> int -> position
(** [move_counter_clockwise pos steps] moves counter-clockwise (add flats) by steps *)

val fifth_above : position -> position
(** [fifth_above pos] moves up a perfect fifth (clockwise 1 step) *)

val fifth_below : position -> position
(** [fifth_below pos] moves down a perfect fifth (counter-clockwise 1 step) *)

val fourth_above : position -> position
(** [fourth_above pos] moves up a perfect fourth (counter-clockwise 1 step) *)

val fourth_below : position -> position
(** [fourth_below pos] moves down a perfect fourth (clockwise 1 step) *)

(** {1 Key signatures} *)

type accidental_type = Sharp | Flat | Natural

val sharps_count : position -> int
(** [sharps_count pos] returns number of sharps in key signature (0-7).
    Returns 0 if position uses flats. *)

val flats_count : position -> int
(** [flats_count pos] returns number of flats in key signature (0-7).
    Returns 0 if position uses sharps. *)

val accidental_type : position -> accidental_type
(** [accidental_type pos] returns whether key uses sharps, flats, or neither *)

val key_signature : position -> Note.base list
(** [key_signature pos] returns ordered list of sharped or flatted notes.
    E.g., G major (pos=7) returns [F; C] (F# and C# in key signature).
    F major (pos=5) returns [B] (Bb in key signature). *)

(** {1 Scales at positions} *)

val major_scale : position -> Scale.t
(** [major_scale pos] returns major scale with tonic at this position *)

val minor_scale : position -> Scale.t
(** [minor_scale pos] returns natural minor scale with tonic at this position *)

val relative_minor_position : position -> position
(** [relative_minor_position pos] returns position of relative minor.
    E.g., C major (0) -> A minor (9) *)

val relative_major_position : position -> position
(** [relative_major_position pos] returns position of relative major.
    E.g., A minor (9) -> C major (0) *)

(** {1 Mode brightness and modal interchange} *)

val mode_at_brightness : position -> Mode.t -> position
(** [mode_at_brightness tonic_pos mode] returns the circle position for
    the given mode starting from tonic.

    E.g., C Lydian: C is tonic, but Lydian is +1 brighter than Ionian,
    so it aligns with G on the circle (1 sharp). *)

val brightness_difference : Mode.t -> Mode.t -> int
(** [brightness_difference m1 m2] returns the circle steps between modes.
    Positive = m2 is brighter, negative = m2 is darker. *)

val parallel_mode : position -> Mode.t -> position
(** [parallel_mode pos new_mode] returns circle position for parallel mode.
    Same tonic, different mode (adjusts position based on mode brightness). *)

(** {1 Modulation and key relationships} *)

type relationship =
  | Same             (** Same key *)
  | Relative         (** Relative major/minor *)
  | Parallel         (** Parallel major/minor *)
  | Dominant         (** Dominant (V) *)
  | Subdominant      (** Subdominant (IV) *)
  | Closely_related  (** Shares many common tones (within 2 steps) *)
  | Distantly_related (** Remote key (3+ steps away) *)

val relationship : position -> position -> relationship
(** [relationship pos1 pos2] categorizes the relationship between two keys *)

val closely_related_keys : position -> position list
(** [closely_related_keys pos] returns positions of all closely related keys.
    These are the most common modulation targets (within 1-2 steps). *)

val common_tone_count : position -> position -> int
(** [common_tone_count pos1 pos2] returns number of shared pitch classes
    between major scales at these positions. *)

(** {1 Harmonic functions based on circle} *)

val dominant_position : position -> position
(** [dominant_position pos] returns the dominant (V) position.
    One fifth up (1 step clockwise). *)

val subdominant_position : position -> position
(** [subdominant_position pos] returns the subdominant (IV) position.
    One fifth down (1 step counter-clockwise). *)

val secondary_dominant : position -> Note.t -> position option
(** [secondary_dominant tonic target_degree] returns circle position of
    secondary dominant chord (V/x).

    E.g., in C major, V/V (D major) dominates G (the dominant).
    secondary_dominant C_pos (Note.g) returns position for D. *)

(** {1 All positions} *)

val all_positions : position list
(** All 12 positions in clockwise order starting from C (0) *)

val all_positions_clockwise : position -> position list
(** [all_positions_clockwise start] returns all 12 positions starting from start,
    going clockwise around the circle *)

(** {1 String conversion} *)

val to_string : position -> string
(** [to_string pos] returns key name: "C", "G", "F", etc. *)

val to_string_with_signature : position -> string
(** [to_string_with_signature pos] returns key with signature info.
    E.g., "G major (1#)", "F major (1♭)" *)

(** {1 Visualization helpers} *)

val position_name : position -> string
(** [position_name pos] returns the name used in circle diagrams.
    E.g., position 0 = "C", position 7 = "G", position 5 = "F" *)

val enharmonic_name : position -> string option
(** [enharmonic_name pos] returns enharmonic equivalent if commonly used.
    E.g., position 6 (F#/Gb) returns Some "Gb" when primary is "F#" *)
