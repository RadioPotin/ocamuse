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

let base_of_string =
  let open Types in
  function
  | 'a' | 'A' -> A
  | 'b' | 'B' -> B
  | 'c' | 'C' -> C
  | 'd' | 'D' -> D
  | 'e' | 'E' -> E
  | 'f' | 'F' -> F
  | 'g' | 'G' -> G
  | _ -> invalid_arg "note_of_string"

let note_of_string s =
  let open Types in
  if String.length s < 2 then
    let base = base_of_string s.[0] in
    { base; alteration = 0 }
  else
    let base = base_of_string s.[0] in
    let sub = String.sub s 1 (String.length s - 1) in
    let alteration =
      match int_of_string_opt sub with
      | None -> failwith "invalid_arg"
      | Some s -> s
    in
    { base; alteration }

let mode_of_string =
  let open Types in
  function
  | "A" | "a" -> A_mode
  | "B" | "b" -> B_mode
  | "C" | "c" -> C_mode
  | "D" | "d" -> D_mode
  | "E" | "e" -> E_mode
  | "F" | "f" -> F_mode
  | "G" | "g" -> G_mode
  | _ -> invalid_arg "mode_of_string"

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
  | _ -> assert false

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
  | 1 -> Lred
  | 2 -> Lgreen
  | 3 -> Lblue
  | 4 -> Lyellow
  | 5 -> Lmagenta
  | 6 -> Lcyan
  | 7 -> Green
  | _n -> assert false
