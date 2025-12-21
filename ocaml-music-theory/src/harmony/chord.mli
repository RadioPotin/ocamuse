(** Chords - simultaneous combinations of notes forming harmonic structures.

    This module provides construction, analysis, and manipulation of chords
    from simple triads to complex extended harmonies.
*)

(** {1 Chord types} *)

(** Triad qualities (3-note chords) *)
type triad_quality =
  | Major          (** Major triad: M3 + P5 (1-3-5) *)
  | Minor          (** Minor triad: m3 + P5 (1-♭3-5) *)
  | Diminished     (** Diminished triad: m3 + d5 (1-♭3-♭5) *)
  | Augmented      (** Augmented triad: M3 + A5 (1-3-♯5) *)
  | Sus2           (** Suspended 2nd: M2 + P5 (1-2-5) *)
  | Sus4           (** Suspended 4th: P4 + P5 (1-4-5) *)

(** Seventh chord qualities (4-note chords) *)
type seventh_quality =
  | Major7         (** Major 7th: M3 + P5 + M7 (Imaj7, IVmaj7) *)
  | Minor7         (** Minor 7th: m3 + P5 + m7 (ii7, iii7, vi7) *)
  | Dominant7      (** Dominant 7th: M3 + P5 + m7 (V7) *)
  | HalfDiminished7 (** Half-diminished: m3 + d5 + m7 (viiø7) *)
  | Diminished7    (** Fully diminished: m3 + d5 + d7 (vii°7) *)
  | MinorMajor7    (** Minor-major 7th: m3 + P5 + M7 (rare, i△7) *)
  | Augmented7     (** Augmented 7th: M3 + A5 + m7 (III+7) *)

(** Extended chord types (5+ notes) *)
type extension =
  | Ninth of seventh_quality          (** Add 9th to seventh chord *)
  | Eleventh of seventh_quality       (** Add 11th to seventh chord *)
  | Thirteenth of seventh_quality     (** Add 13th to seventh chord *)
  | Add9                              (** Add 9th to triad (no 7th) *)
  | Add11                             (** Add 11th to triad (no 7th) *)
  | Sixth                             (** Add major 6th to major triad *)
  | MinorSixth                        (** Add major 6th to minor triad *)

(** Chord structure *)
type chord_type =
  | Triad of triad_quality
  | Seventh of seventh_quality
  | Extended of extension

(** A chord is a root note plus a chord type *)
type t =
  { root : Note.t
  ; chord_type : chord_type
  ; notes : Note.t list  (** Computed chord tones *)
  }

(** {1 Construction} *)

val make : Note.t -> chord_type -> t
(** [make root chord_type] constructs chord from root and type *)

val triad : Note.t -> triad_quality -> t
(** [triad root quality] constructs a triad *)

val seventh : Note.t -> seventh_quality -> t
(** [seventh root quality] constructs a seventh chord *)

val major : Note.t -> t
(** [major root] constructs major triad *)

val minor : Note.t -> t
(** [minor root] constructs minor triad *)

val diminished : Note.t -> t
(** [diminished root] constructs diminished triad *)

val augmented : Note.t -> t
(** [augmented root] constructs augmented triad *)

val major7 : Note.t -> t
(** [major7 root] constructs major seventh *)

val minor7 : Note.t -> t
(** [minor7 root] constructs minor seventh *)

val dominant7 : Note.t -> t
(** [dominant7 root] constructs dominant seventh *)

val half_diminished7 : Note.t -> t
(** [half_diminished7 root] constructs half-diminished seventh (ø7) *)

(** {1 Construction from intervals} *)

val from_intervals : Note.t -> Interval.t list -> t option
(** [from_intervals root intervals] analyzes intervals and constructs chord.
    Returns None if interval pattern doesn't match a recognized chord type. *)

val from_notes : Note.t list -> t option
(** [from_notes notes] analyzes a list of notes and identifies the chord.
    First note is assumed to be the root. Returns None if unrecognized. *)

(** {1 Chord tones} *)

val notes : t -> Note.t list
(** [notes chord] returns all notes in the chord in ascending order from root *)

val root : t -> Note.t
(** [root chord] returns the root note *)

val third : t -> Note.t option
(** [third chord] returns the third (or None for sus chords) *)

val fifth : t -> Note.t
(** [fifth chord] returns the fifth *)

val seventh_note : t -> Note.t option
(** [seventh_note chord] returns the seventh if present *)

val extensions : t -> Note.t list
(** [extensions chord] returns extension tones (9th, 11th, 13th) if any *)

(** {1 Inversions} *)

type inversion =
  | Root         (** Root position (root in bass) *)
  | First        (** First inversion (third in bass) *)
  | Second       (** Second inversion (fifth in bass) *)
  | Third        (** Third inversion (seventh in bass, only for 7th chords) *)

val invert : t -> inversion -> Note.t list
(** [invert chord inv] returns notes in specified inversion.
    E.g., C major triad in first inversion = [E, G, C] *)

val bass_note : t -> inversion -> Note.t
(** [bass_note chord inv] returns the bass note for given inversion *)

val detect_inversion : Note.t list -> inversion option
(** [detect_inversion notes] detects the inversion from a list of notes.
    Returns None if notes don't form a recognizable chord. *)

(** {1 Chord analysis} *)

type chord_function =
  | Tonic          (** I - stable, home *)
  | Supertonic     (** ii - pre-dominant *)
  | Mediant        (** iii - tonic extension *)
  | Subdominant    (** IV - pre-dominant *)
  | Dominant       (** V - tension, wants to resolve to I *)
  | Submediant     (** vi - tonic extension, deceptive resolution *)
  | LeadingTone    (** vii° - dominant function *)

val contains : t -> Note.t -> bool
(** [contains chord note] checks if note is a chord tone (enharmonic aware) *)

val is_diatonic : Scale.t -> t -> bool
(** [is_diatonic scale chord] checks if all chord tones are in the scale *)

val degree_in_scale : Scale.t -> t -> int option
(** [degree_in_scale scale chord] returns which scale degree the chord is built on.
    E.g., G major chord in C major scale returns Some 4 (V chord, 0-indexed). *)

val function_in_key : Scale.t -> t -> chord_function option
(** [function_in_key scale chord] returns the harmonic function (tonic, dominant, etc.) *)

(** {1 Chord quality analysis} *)

val quality_name : t -> string
(** [quality_name chord] returns quality as string: "major", "minor", "dom7", etc. *)

val interval_structure : t -> Interval.t list
(** [interval_structure chord] returns intervals from root to each chord tone *)

val tension_level : t -> int
(** [tension_level chord] returns subjective tension score (0-10).
    Major triads = low, diminished/augmented = medium, altered dominants = high. *)

(** {1 Voice leading} *)

val voice_leading_distance : Note.t list -> Note.t list -> int
(** [voice_leading_distance voicing1 voicing2] returns total semitone movement
    across all voices when moving from one chord voicing to another.
    Lower = smoother voice leading. *)

val closest_voicing : t -> Note.t list -> inversion -> Note.t list
(** [closest_voicing chord from_notes target_inv] returns the voicing of chord
    in target_inv that minimizes voice leading distance from from_notes. *)

(** {1 Chord progressions} *)

val resolves_to : t -> t -> bool
(** [resolves_to chord1 chord2] checks if chord1 naturally resolves to chord2.
    E.g., V7 resolves to I, vii° resolves to I. *)

val common_tones : t -> t -> Note.t list
(** [common_tones c1 c2] returns notes shared between two chords *)

(** {1 Alterations} *)

val add_extension : t -> Interval.t -> t option
(** [add_extension chord interval] adds an extension tone (9th, 11th, 13th).
    Returns None if extension doesn't make sense for this chord type. *)

val alter_fifth : t -> [`Diminished | `Augmented] -> t option
(** [alter_fifth chord alteration] raises or lowers the fifth.
    Returns None if incompatible with chord type. *)

val omit_fifth : t -> t option
(** [omit_fifth chord] removes the fifth from the chord (common in jazz voicings) *)

(** {1 String conversion} *)

val to_string : ?unicode:bool -> t -> string
(** [to_string chord] returns chord symbol: "Cmaj7", "Dm", "G7", "F#°".
    If [unicode=true], uses "△" for maj7, "ø" for half-dim, etc. *)

val to_string_with_notes : t -> string
(** [to_string_with_notes chord] returns "C major: C E G" *)

val of_string : string -> (t, string) result
(** [of_string s] parses chord symbol like "Cmaj7", "Dm", "G7".
    Returns Error if unparseable. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal c1 c2] checks exact equality (same root and type) *)

val enharmonic_equal : t -> t -> bool
(** [enharmonic_equal c1 c2] checks if chords contain same pitch classes *)
