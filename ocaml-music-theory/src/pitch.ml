type t = int

(* {1 Construction} *)

let of_int n =
  let n' = n mod 12 in
  if n' < 0 then n' + 12 else n'

let of_int_exn n =
  if n < 0 || n > 11 then
    invalid_arg (Printf.sprintf "Pitch.of_int_exn: %d not in range [0, 11]" n)
  else
    n

(* {1 Named constructors} *)

let c = 0
let c_sharp = 1
let d_flat = 1
let d = 2
let d_sharp = 3
let e_flat = 3
let e = 4
let f = 5
let f_sharp = 6
let g_flat = 6
let g = 7
let g_sharp = 8
let a_flat = 8
let a = 9
let a_sharp = 10
let b_flat = 10
let b = 11

(* {1 Operations} *)

let to_int pc = pc

let add pc semitones = of_int (pc + semitones)

let diff pc1 pc2 =
  let d = pc2 - pc1 in
  if d < 0 then d + 12 else d

let enharmonic_eq pc1 pc2 = pc1 = pc2

(* {1 String conversion} *)

let sharp_names = [|
  "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B"
|]

let flat_names = [|
  "C"; "Db"; "D"; "Eb"; "E"; "F"; "Gb"; "G"; "Ab"; "A"; "Bb"; "B"
|]

let to_string ~prefer pc =
  match prefer with
  | `Sharp -> sharp_names.(pc)
  | `Flat -> flat_names.(pc)

let all = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11]

(* {1 Comparison} *)

let equal pc1 pc2 = pc1 = pc2

let compare pc1 pc2 = compare pc1 pc2
