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

(* {1 Conversion} *)

let to_semitones = function
  | Unison -> 0
  | MinorSecond -> 1
  | MajorSecond -> 2
  | MinorThird -> 3
  | MajorThird -> 4
  | PerfectFourth -> 5
  | Tritone -> 6
  | PerfectFifth -> 7
  | MinorSixth -> 8
  | MajorSixth -> 9
  | MinorSeventh -> 10
  | MajorSeventh -> 11
  | Octave -> 12

let of_semitones n =
  match n with
  | 0 -> Some Unison
  | 1 -> Some MinorSecond
  | 2 -> Some MajorSecond
  | 3 -> Some MinorThird
  | 4 -> Some MajorThird
  | 5 -> Some PerfectFourth
  | 6 -> Some Tritone
  | 7 -> Some PerfectFifth
  | 8 -> Some MinorSixth
  | 9 -> Some MajorSixth
  | 10 -> Some MinorSeventh
  | 11 -> Some MajorSeventh
  | 12 -> Some Octave
  | _ -> None

let of_semitones_exn n =
  match of_semitones n with
  | Some interval -> interval
  | None -> invalid_arg (Printf.sprintf "Interval.of_semitones_exn: %d not in range [0, 12]" n)

(* {1 Operations} *)

let add i1 i2 =
  let semitones = to_semitones i1 + to_semitones i2 in
  if semitones > 12 then None else of_semitones semitones

let invert = function
  | Unison -> Octave
  | MinorSecond -> MajorSeventh
  | MajorSecond -> MinorSeventh
  | MinorThird -> MajorSixth
  | MajorThird -> MinorSixth
  | PerfectFourth -> PerfectFifth
  | Tritone -> Tritone
  | PerfectFifth -> PerfectFourth
  | MinorSixth -> MajorThird
  | MajorSixth -> MinorThird
  | MinorSeventh -> MajorSecond
  | MajorSeventh -> MinorSecond
  | Octave -> Unison

(* {1 Interval qualities} *)

let is_perfect = function
  | Unison | PerfectFourth | PerfectFifth | Octave -> true
  | _ -> false

let is_major = function
  | MajorSecond | MajorThird | MajorSixth | MajorSeventh -> true
  | _ -> false

let is_minor = function
  | MinorSecond | MinorThird | MinorSixth | MinorSeventh -> true
  | _ -> false

(* {1 Diatonic properties} *)

let letter_distance = function
  | Unison -> 0
  | MinorSecond | MajorSecond -> 1
  | MinorThird | MajorThird -> 2
  | PerfectFourth | Tritone -> 3
  | PerfectFifth -> 4
  | MinorSixth | MajorSixth -> 5
  | MinorSeventh | MajorSeventh -> 6
  | Octave -> 7

(* {1 String conversion} *)

let to_string = function
  | Unison -> "P1"
  | MinorSecond -> "m2"
  | MajorSecond -> "M2"
  | MinorThird -> "m3"
  | MajorThird -> "M3"
  | PerfectFourth -> "P4"
  | Tritone -> "TT"
  | PerfectFifth -> "P5"
  | MinorSixth -> "m6"
  | MajorSixth -> "M6"
  | MinorSeventh -> "m7"
  | MajorSeventh -> "M7"
  | Octave -> "P8"

let all = [
  Unison;
  MinorSecond;
  MajorSecond;
  MinorThird;
  MajorThird;
  PerfectFourth;
  Tritone;
  PerfectFifth;
  MinorSixth;
  MajorSixth;
  MinorSeventh;
  MajorSeventh;
  Octave;
]
