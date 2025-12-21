(** Pitch classes from 0-11 representing chromatic scale.
    C=0, C#/Db=1, D=2, ..., B=11 *)
type t = private int

(** {1 Construction} *)

val of_int : int -> t
(** [of_int n] creates pitch class from integer (mod 12) *)

val of_int_exn : int -> t
(** [of_int_exn n] creates pitch class, raises if n < 0 or n > 11 *)

(** {1 Named constructors} *)

val c : t
val c_sharp : t
val d_flat : t
val d : t
val d_sharp : t
val e_flat : t
val e : t
val f : t
val f_sharp : t
val g_flat : t
val g : t
val g_sharp : t
val a_flat : t
val a : t
val a_sharp : t
val b_flat : t
val b : t

(** {1 Operations} *)

val to_int : t -> int
(** [to_int pc] returns integer 0-11 *)

val add : t -> int -> t
(** [add pc semitones] transposes by semitones (mod 12) *)

val diff : t -> t -> int
(** [diff pc1 pc2] returns semitone distance (always 0-11) *)

val enharmonic_eq : t -> t -> bool
(** [enharmonic_eq pc1 pc2] is always true since pitch classes ARE enharmonic equivalents *)

(** {1 String conversion} *)

val to_string : prefer:[`Sharp | `Flat] -> t -> string
(** [to_string ~prefer pc] returns "C", "C#"/"Db", etc. *)

val all : t list
(** All 12 pitch classes in chromatic order *)

(** {1 Comparison} *)

val equal : t -> t -> bool
val compare : t -> t -> int
