type base =
  | A
  | B
  | C
  | D
  | E
  | F
  | G

type t =
  { base : base
  ; alteration : int
  }

(* {1 Construction} *)

let make base alteration = { base; alteration }

let natural base = { base; alteration = 0 }

let sharp base = { base; alteration = 1 }

let flat base = { base; alteration = -1 }

(* {1 Named constructors} *)

let c = { base = C; alteration = 0 }
let c_sharp = { base = C; alteration = 1 }
let d_flat = { base = D; alteration = -1 }
let d = { base = D; alteration = 0 }
let d_sharp = { base = D; alteration = 1 }
let e_flat = { base = E; alteration = -1 }
let e = { base = E; alteration = 0 }
let f = { base = F; alteration = 0 }
let f_sharp = { base = F; alteration = 1 }
let g_flat = { base = G; alteration = -1 }
let g = { base = G; alteration = 0 }
let g_sharp = { base = G; alteration = 1 }
let a_flat = { base = A; alteration = -1 }
let a = { base = A; alteration = 0 }
let a_sharp = { base = A; alteration = 1 }
let b_flat = { base = B; alteration = -1 }
let b = { base = B; alteration = 0 }

(* {1 Pitch class conversion} *)

let base_to_pitch_class = function
  | C -> 0
  | D -> 2
  | E -> 4
  | F -> 5
  | G -> 7
  | A -> 9
  | B -> 11

let to_pitch note =
  let base_pc = base_to_pitch_class note.base in
  Pitch.of_int (base_pc + note.alteration)

let of_pitch pc ~prefer =
  let pitch_int = Pitch.to_int pc in
  match prefer with
  | `Sharp -> begin
      match pitch_int with
      | 0 -> c
      | 1 -> c_sharp
      | 2 -> d
      | 3 -> d_sharp
      | 4 -> e
      | 5 -> f
      | 6 -> f_sharp
      | 7 -> g
      | 8 -> g_sharp
      | 9 -> a
      | 10 -> a_sharp
      | 11 -> b
      | _ -> failwith "Invalid pitch class"
    end
  | `Flat -> begin
      match pitch_int with
      | 0 -> c
      | 1 -> d_flat
      | 2 -> d
      | 3 -> e_flat
      | 4 -> e
      | 5 -> f
      | 6 -> g_flat
      | 7 -> g
      | 8 -> a_flat
      | 9 -> a
      | 10 -> b_flat
      | 11 -> b
      | _ -> failwith "Invalid pitch class"
    end

(* {1 Enharmonic equivalence} *)

let enharmonic_eq n1 n2 =
  Pitch.equal (to_pitch n1) (to_pitch n2)

(* {1 Base note operations} *)

let next_base = function
  | C -> D
  | D -> E
  | E -> F
  | F -> G
  | G -> A
  | A -> B
  | B -> C

let prev_base = function
  | C -> B
  | D -> C
  | E -> D
  | F -> E
  | G -> F
  | A -> G
  | B -> A

let base_distance = function
  | C -> 2
  | D -> 2
  | E -> 1
  | F -> 2
  | G -> 2
  | A -> 2
  | B -> 1

let base_to_index = function
  | C -> 0
  | D -> 1
  | E -> 2
  | F -> 3
  | G -> 4
  | A -> 5
  | B -> 6

let base_of_index = function
  | 0 -> Some C
  | 1 -> Some D
  | 2 -> Some E
  | 3 -> Some F
  | 4 -> Some G
  | 5 -> Some A
  | 6 -> Some B
  | _ -> None

let all_bases = [C; D; E; F; G; A; B]

(* {1 String conversion} *)

let base_to_string = function
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
  | G -> "G"
  | A -> "A"
  | B -> "B"

let to_string ?(unicode = false) note =
  let base_str = base_to_string note.base in
  let sharp_symbol = if unicode then "♯" else "#" in
  let flat_symbol = if unicode then "♭" else "b" in
  match note.alteration with
  | 0 -> base_str
  | 1 -> base_str ^ sharp_symbol
  | -1 -> base_str ^ flat_symbol
  | 2 -> base_str ^ sharp_symbol ^ sharp_symbol
  | -2 -> base_str ^ flat_symbol ^ flat_symbol
  | n when n > 0 -> base_str ^ string_of_int n
  | n -> base_str ^ string_of_int n

let base_of_char = function
  | 'A' | 'a' -> Ok A
  | 'B' | 'b' -> Ok B
  | 'C' | 'c' -> Ok C
  | 'D' | 'd' -> Ok D
  | 'E' | 'e' -> Ok E
  | 'F' | 'f' -> Ok F
  | 'G' | 'g' -> Ok G
  | c -> Error (Printf.sprintf "Invalid base note character: %c" c)

let of_string s =
  if String.length s < 1 then
    Error "Empty string"
  else
    match base_of_char s.[0] with
    | Error e -> Error e
    | Ok base ->
      if String.length s = 1 then
        Ok { base; alteration = 0 }
      else
        let rest = String.sub s 1 (String.length s - 1) in
        (* Count sharps and flats *)
        let rec count_accidentals pos sharps flats =
          if pos >= String.length rest then
            (sharps, flats)
          else
            match rest.[pos] with
            | '#' -> count_accidentals (pos + 1) (sharps + 1) flats
            | 'b' -> count_accidentals (pos + 1) sharps (flats + 1)
            | _ ->
              (* Check for unicode sharp/flat symbols *)
              if pos + 2 < String.length rest then
                let char_bytes = String.sub rest pos 3 in
                if char_bytes = "♯" then
                  count_accidentals (pos + 3) (sharps + 1) flats
                else if char_bytes = "♭" then
                  count_accidentals (pos + 3) sharps (flats + 1)
                else
                  (sharps, flats)
              else
                (sharps, flats)
        in
        let (sharps, flats) = count_accidentals 0 0 0 in
        let alteration = sharps - flats in
        Ok { base; alteration }

(* {1 Comparison} *)

let equal n1 n2 =
  n1.base = n2.base && n1.alteration = n2.alteration

let pitch_equal n1 n2 = enharmonic_eq n1 n2

let compare n1 n2 =
  let pitch_cmp = Pitch.compare (to_pitch n1) (to_pitch n2) in
  if pitch_cmp <> 0 then pitch_cmp
  else compare n1.base n2.base
