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

let note_of_string s =
  let open Types in
  if String.length s < 2 then invalid_arg "note_of_string"
  else
    let base =
      match s.[0] with
      | 'a' | 'A' -> A
      | 'b' | 'B' -> B
      | 'c' | 'C' -> C
      | 'd' | 'D' -> D
      | 'e' | 'E' -> E
      | 'f' | 'F' -> F
      | 'g' | 'G' -> G
      | _ -> invalid_arg "note_of_string"
    in
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
  | "A" -> A_mode
  | "B" -> B_mode
  | "C" -> C_mode
  | "D" -> D_mode
  | "E" -> E_mode
  | "F" -> F_mode
  | "G" -> G_mode
  | _ -> invalid_arg "mode_of_string"

let fretnote_to_note =
  let open Types.Fretboard in
  function
  | A -> Types.{ base = A; alteration = 0 }
  | Ash_Bfl -> Types.{ base = A; alteration = 1 }
  | B -> Types.{ base = B; alteration = 0 }
  | C -> Types.{ base = C; alteration = 0 }
  | Csh_Dfl -> { base = C; alteration = 1 }
  | D -> Types.{ base = D; alteration = 0 }
  | Dsh_Efl -> Types.{ base = D; alteration = 1 }
  | E -> Types.{ base = E; alteration = 0 }
  | F -> Types.{ base = F; alteration = 0 }
  | Fsh_Gfl -> Types.{ base = F; alteration = 1 }
  | G -> Types.{ base = G; alteration = 0 }
  | Gsh_Afl -> Types.{ base = G; alteration = 1 }

let note_to_fretnote =
  let open Types in
  fun note ->
    let base = note.base in
    let alteration = note.alteration in
    match (base, alteration) with
    | A, 0 | B, -2 -> Types.Fretboard.A
    | A, 1 | B, -1 -> Types.Fretboard.Ash_Bfl
    | B, 0 -> Types.Fretboard.B
    | C, 0 -> Types.Fretboard.C
    | C, 1 | D, -1 -> Types.Fretboard.Csh_Dfl
    | D, 0 -> Types.Fretboard.D
    | D, 1 | E, -1 -> Types.Fretboard.Dsh_Efl
    | E, 0 -> Types.Fretboard.E
    | F, 0 -> Types.Fretboard.F
    | F, 1 | G, -1 -> Types.Fretboard.Fsh_Gfl
    | G, 0 -> Types.Fretboard.G
    | G, 1 | A, -1 -> Types.Fretboard.Gsh_Afl
    | _ -> invalid_arg "note_to_fretnote, alteration not handled"

let int_of_fret_note =
  let open Types.Fretboard in
  function
  | A -> 0
  | Ash_Bfl -> 1
  | B -> 2
  | C -> 3
  | Csh_Dfl -> 4
  | D -> 5
  | Dsh_Efl -> 6
  | E -> 7
  | F -> 8
  | Fsh_Gfl -> 9
  | G -> 10
  | Gsh_Afl -> 11

let fret_note_of_int =
  let open Types.Fretboard in
  function
  | 0 -> A
  | 1 -> Ash_Bfl
  | 2 -> B
  | 3 -> C
  | 4 -> Csh_Dfl
  | 5 -> D
  | 6 -> Dsh_Efl
  | 7 -> E
  | 8 -> F
  | 9 -> Fsh_Gfl
  | 10 -> G
  | 11 -> Gsh_Afl
  | n -> invalid_arg ("fret_note_of_int: " ^ string_of_int n)
