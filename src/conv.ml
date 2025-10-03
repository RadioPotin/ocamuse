let chord_to_string =
  let open Types in
  fun (chord : chord) : string ->
    match chord with
    | Major -> "maj"
    | Minor -> "min"
    | Dimin -> "dim"
    | Augment -> "aug"
    | Suspend2 -> "sus2"
    | Suspend4 -> "sus4"
    | Major7 -> "maj7"
    | Domin7 -> "7"
    | Minor7 -> "min7"
    | HalfDim7 -> "m7b5"
    | Sixth -> "6"
    | MinorSixth -> "m6"

let base_of_char =
  let open Types in
  function
  | 'a' | 'A' -> Ok A
  | 'b' | 'B' -> Ok B
  | 'c' | 'C' -> Ok C
  | 'd' | 'D' -> Ok D
  | 'e' | 'E' -> Ok E
  | 'f' | 'F' -> Ok F
  | 'g' | 'G' -> Ok G
  | c -> Error (InvalidCharacter c)

let note_of_string s : (Types.note, Types.parse_error) result =
  let open Types in
  if String.length s < 1 then Error (InvalidCharacter ' ')
  else if String.length s < 2 then
    match base_of_char s.[0] with
    | Ok base -> Ok { base; alteration = 0 }
    | Error e -> Error e
  else
    match base_of_char s.[0] with
    | Error e -> Error e
    | Ok base ->
      let sub = String.sub s 1 (String.length s - 1) in
      (try
        let alteration = int_of_string sub in
        Ok { base; alteration }
      with Failure _ -> Error (InvalidCharacter s.[1]))

let mode_of_string s : (Types.mode, Types.parse_error) result =
  let open Types in
  match s with
  | "A" | "a" -> Ok A_mode
  | "B" | "b" -> Ok B_mode
  | "C" | "c" -> Ok C_mode
  | "D" | "d" -> Ok D_mode
  | "E" | "e" -> Ok E_mode
  | "F" | "f" -> Ok F_mode
  | "G" | "g" -> Ok G_mode
  | _ -> Error (InvalidMode s)

let note_to_int =
  let open Types in
  function
  | { base = A; alteration = 0 } -> 0
  | { base = A; alteration = 1 } | { base = B; alteration = -1 } -> 1
  | { base = B; alteration = 0 } -> 2
  | { base = C; alteration = 0 } -> 3
  | { base = C; alteration = 1 } | { base = D; alteration = -1 } -> 4
  | { base = D; alteration = 0 } -> 5
  | { base = D; alteration = 1 } | { base = E; alteration = -1 } -> 6
  | { base = E; alteration = 0 } -> 7
  | { base = F; alteration = 0 } -> 8
  | { base = F; alteration = 1 } | { base = G; alteration = -1 } -> 9
  | { base = G; alteration = 0 } -> 10
  | { base = G; alteration = 1 } | { base = A; alteration = -1 } -> 11
  | note ->
    (* For other alterations, calculate modulo 12 *)
    let base_int = match note.base with
      | A -> 0 | B -> 2 | C -> 3 | D -> 5 | E -> 7 | F -> 8 | G -> 10
    in
    (base_int + note.alteration) mod 12

type alt =
  | Sharp
  | Flat

let rec int_to_note p_alt =
  let open Types in
  let alter (sharp, flat) =
    match p_alt with
    | Sharp -> { base = sharp; alteration = 1 }
    | Flat -> { base = flat; alteration = -1 }
  in
  function
  | 0 -> { base = A; alteration = 0 }
  | 1 -> alter (A, B)
  | 2 -> { base = B; alteration = 0 }
  | 3 -> { base = C; alteration = 0 }
  | 4 -> alter (C, D)
  | 5 -> { base = D; alteration = 0 }
  | 6 -> alter (D, E)
  | 7 -> { base = E; alteration = 0 }
  | 8 -> { base = F; alteration = 0 }
  | 9 -> alter (F, G)
  | 10 -> { base = G; alteration = 0 }
  | 11 -> alter (G, A)
  | n -> int_to_note p_alt (n - 12)

let degree_to_colour =
  let open Types in
  function
  | 0 -> Lred
  | 1 -> Blue
  | 2 -> Lgreen
  | 3 -> Lblue
  | 4 -> Lyellow
  | 5 -> Lmagenta
  | 6 -> Lcyan
  | 7 -> Green
  | _n -> assert false
