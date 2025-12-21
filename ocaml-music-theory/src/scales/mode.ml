type t =
  | Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian

(* {1 Construction} *)

(* Brightness order: Lydian (brightest) to Locrian (darkest) *)
let all = [Lydian; Ionian; Mixolydian; Dorian; Aeolian; Phrygian; Locrian]

let of_degree n =
  match n with
  | 0 -> Some Ionian
  | 1 -> Some Dorian
  | 2 -> Some Phrygian
  | 3 -> Some Lydian
  | 4 -> Some Mixolydian
  | 5 -> Some Aeolian
  | 6 -> Some Locrian
  | _ -> None

let of_degree_exn n =
  match of_degree n with
  | Some m -> m
  | None -> invalid_arg (Printf.sprintf "Mode.of_degree_exn: %d not in [0, 6]" n)

(* {1 Intervallic structure} *)

let semitones = function
  | Ionian     -> [2; 2; 1; 2; 2; 2; 1]  (* W W H W W W H *)
  | Dorian     -> [2; 1; 2; 2; 2; 1; 2]  (* W H W W W H W *)
  | Phrygian   -> [1; 2; 2; 2; 1; 2; 2]  (* H W W W H W W *)
  | Lydian     -> [2; 2; 2; 1; 2; 2; 1]  (* W W W H W W H *)
  | Mixolydian -> [2; 2; 1; 2; 2; 1; 2]  (* W W H W W H W *)
  | Aeolian    -> [2; 1; 2; 2; 1; 2; 2]  (* W H W W H W W *)
  | Locrian    -> [1; 2; 2; 1; 2; 2; 2]  (* H W W H W W W *)

let intervals mode =
  let sems = semitones mode in
  List.filter_map Interval.of_semitones sems

(* {1 Mode relationships} *)

let to_degree = function
  | Ionian -> 0
  | Dorian -> 1
  | Phrygian -> 2
  | Lydian -> 3
  | Mixolydian -> 4
  | Aeolian -> 5
  | Locrian -> 6

let rotate mode n =
  let deg = to_degree mode in
  let new_deg = (deg + n) mod 7 in
  let new_deg = if new_deg < 0 then new_deg + 7 else new_deg in
  of_degree_exn new_deg

let relative_to_major mode =
  to_degree mode

(* {1 Brightness ordering} *)

let brightness = function
  | Lydian -> 3      (* 1 more sharp than major *)
  | Ionian -> 2      (* Major scale baseline *)
  | Mixolydian -> 1  (* 1 flat *)
  | Dorian -> 0      (* 2 flats - neutral *)
  | Aeolian -> -1    (* 3 flats - natural minor *)
  | Phrygian -> -2   (* 4 flats *)
  | Locrian -> -3    (* 5 flats - darkest *)

let of_brightness n =
  match n with
  | 3 -> Some Lydian
  | 2 -> Some Ionian
  | 1 -> Some Mixolydian
  | 0 -> Some Dorian
  | -1 -> Some Aeolian
  | -2 -> Some Phrygian
  | -3 -> Some Locrian
  | _ -> None

let brighter mode =
  let b = brightness mode in
  of_brightness (b + 1)

let darker mode =
  let b = brightness mode in
  of_brightness (b - 1)

(* {1 Modal characteristics} *)

let characteristic_interval = function
  | Ionian -> Interval.MajorSeventh     (* Natural 7th *)
  | Dorian -> Interval.MajorSixth       (* Raised 6th *)
  | Phrygian -> Interval.MinorSecond    (* Flat 2nd *)
  | Lydian -> Interval.Tritone          (* Sharp 4th *)
  | Mixolydian -> Interval.MinorSeventh (* Flat 7th *)
  | Aeolian -> Interval.MinorSixth      (* Flat 6th *)
  | Locrian -> Interval.Tritone         (* Flat 5th *)

let is_major = function
  | Ionian | Lydian | Mixolydian -> true
  | _ -> false

let is_minor = function
  | Dorian | Phrygian | Aeolian | Locrian -> true
  | _ -> false

(* {1 String conversion} *)

let to_string = function
  | Ionian -> "Ionian"
  | Dorian -> "Dorian"
  | Phrygian -> "Phrygian"
  | Lydian -> "Lydian"
  | Mixolydian -> "Mixolydian"
  | Aeolian -> "Aeolian"
  | Locrian -> "Locrian"

let of_string s =
  match String.lowercase_ascii s with
  | "ionian" | "major" | "i" -> Ok Ionian
  | "dorian" | "ii" -> Ok Dorian
  | "phrygian" | "iii" -> Ok Phrygian
  | "lydian" | "iv" -> Ok Lydian
  | "mixolydian" | "v" -> Ok Mixolydian
  | "aeolian" | "minor" | "vi" -> Ok Aeolian
  | "locrian" | "vii" -> Ok Locrian
  | _ -> Error (Printf.sprintf "Unknown mode: %s" s)

let to_roman = function
  | Ionian -> "I"
  | Dorian -> "II"
  | Phrygian -> "III"
  | Lydian -> "IV"
  | Mixolydian -> "V"
  | Aeolian -> "VI"
  | Locrian -> "VII"

(* {1 Comparison} *)

let equal m1 m2 = m1 = m2

let compare m1 m2 = compare (to_degree m1) (to_degree m2)
