(** Base note letters A-G *)
type base =
  | A
  | B
  | C
  | D
  | E
  | F
  | G

(** A note is a base letter plus alteration *)
type t =
  { base : base
  ; alteration : int  (** -2=♭♭, -1=♭, 0=♮, 1=♯, 2=♯♯ *)
  }

(** {1 Construction} *)

val make : base -> int -> t
(** [make base alt] creates note *)

val natural : base -> t
(** [natural base] creates natural note (alteration=0) *)

val sharp : base -> t
(** [sharp base] creates sharped note *)

val flat : base -> t
(** [flat base] creates flatted note *)

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

(** {1 Pitch class conversion} *)

val to_pitch : t -> Pitch.t
(** [to_pitch note] converts to pitch class (loses enharmonic info) *)

val of_pitch : Pitch.t -> prefer:[`Sharp | `Flat] -> t
(** [of_pitch pc ~prefer] chooses enharmonic spelling *)

(** {1 Enharmonic equivalence} *)

val enharmonic_eq : t -> t -> bool
(** [enharmonic_eq n1 n2] true if same pitch class.
    E.g., C# = Db *)

(** {1 Base note operations} *)

val next_base : base -> base
(** [next_base b] returns next letter (C -> D, B -> C) *)

val prev_base : base -> base
(** [prev_base b] returns previous letter *)

val base_distance : base -> int
(** [base_distance b] returns semitones to next base.
    E.g., C -> 2 (to D), E -> 1 (to F) *)

val base_to_index : base -> int
(** [base_to_index b] returns 0-6 (C=0, D=1, ..., B=6) *)

val base_of_index : int -> base option
(** [base_of_index n] returns base if n in [0, 6] *)

val all_bases : base list
(** [C; D; E; F; G; A; B] in order *)

(** {1 String conversion} *)

val to_string : ?unicode:bool -> t -> string
(** [to_string note] returns "C#", "Db", etc.
    If [unicode=true], uses "♯" and "♭" *)

val of_string : string -> (t, string) result
(** [of_string s] parses "C", "C#", "Db", "C-1", etc.
    Returns Error with message if invalid *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** Exact equality (base AND alteration) *)

val pitch_equal : t -> t -> bool
(** Enharmonic equality (same pitch class) *)

val compare : t -> t -> int
