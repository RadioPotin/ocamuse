(** Musical scales combining a root note with an interval pattern.

    A scale is the foundation for harmony and melody. This module provides
    construction, analysis, and navigation of musical scales.
*)

type t =
  { root : Note.t
  ; mode : Mode.t
  ; degrees : Note.t list  (** Computed scale degrees *)
  }

(** {1 Construction} *)

val make : Note.t -> Mode.t -> t
(** [make root mode] constructs a scale from root note and mode *)

val major : Note.t -> t
(** [major root] constructs a major scale (Ionian mode) *)

val minor : Note.t -> t
(** [minor root] constructs a natural minor scale (Aeolian mode) *)

val harmonic_minor : Note.t -> t
(** [harmonic_minor root] constructs harmonic minor (raised 7th) *)

val melodic_minor : Note.t -> t
(** [melodic_minor root] constructs melodic minor ascending (raised 6th and 7th) *)

(** {1 Scale degrees} *)

val degrees : t -> Note.t list
(** [degrees scale] returns all 7 notes in the scale *)

val degree : t -> int -> Note.t option
(** [degree scale n] returns the nth degree (0-indexed). 0=root, 1=2nd, ..., 6=7th *)

val degree_exn : t -> int -> Note.t
(** [degree_exn scale n] returns nth degree, raises if n not in [0, 6] *)

val tonic : t -> Note.t
(** [tonic scale] returns the root note (1st degree) *)

val supertonic : t -> Note.t
(** [supertonic scale] returns 2nd degree *)

val mediant : t -> Note.t
(** [mediant scale] returns 3rd degree *)

val subdominant : t -> Note.t
(** [subdominant scale] returns 4th degree *)

val dominant : t -> Note.t
(** [dominant scale] returns 5th degree *)

val submediant : t -> Note.t
(** [submediant scale] returns 6th degree *)

val leading_tone : t -> Note.t
(** [leading_tone scale] returns 7th degree *)

(** {1 Scale analysis} *)

val contains : t -> Note.t -> bool
(** [contains scale note] checks if note is in the scale (enharmonic aware) *)

val degree_of_note : t -> Note.t -> int option
(** [degree_of_note scale note] returns the degree (0-6) of note in scale, if present *)

val interval_from_root : t -> Note.t -> Interval.t option
(** [interval_from_root scale note] returns interval from root to note, if in scale *)

(** {1 Diatonic harmony} *)

type triad_quality = Major | Minor | Diminished | Augmented

val diatonic_triads : t -> (Note.t * triad_quality) list
(** [diatonic_triads scale] returns the 7 diatonic triads built on each degree.
    E.g., C major: [(C, Major); (D, Minor); (E, Minor); (F, Major); (G, Major); (A, Minor); (B, Diminished)]
*)

type seventh_quality =
  | Maj7 | Min7 | Dom7 | HalfDim7 | Dim7

val diatonic_sevenths : t -> (Note.t * seventh_quality) list
(** [diatonic_sevenths scale] returns the 7 diatonic seventh chords *)

(** {1 Modal interchange} *)

val parallel_mode : t -> Mode.t -> t
(** [parallel_mode scale new_mode] creates parallel mode (same root, different mode).
    E.g., C major -> C dorian *)

(** {1 Related keys} *)

val relative_minor : t -> t
(** [relative_minor scale] returns the relative minor (6th degree becomes root).
    Only valid for major scales. E.g., C major -> A minor *)

val relative_major : t -> t
(** [relative_major scale] returns the relative major (3rd degree becomes root).
    Only valid for minor scales. E.g., A minor -> C major *)

val parallel_minor : t -> t
(** [parallel_minor scale] returns parallel minor (same root, Aeolian mode) *)

val parallel_major : t -> t
(** [parallel_major scale] returns parallel major (same root, Ionian mode) *)

(** {1 String conversion} *)

val to_string : t -> string
(** [to_string scale] returns "C Major", "D Dorian", etc. *)

val to_string_degrees : t -> string
(** [to_string_degrees scale] returns all degrees as string: "C D E F G A B" *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** Scales are equal if they have the same root and mode *)

val enharmonic_equal : t -> t -> bool
(** Scales are enharmonically equal if they contain the same pitch classes *)
