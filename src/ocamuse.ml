(** [mode_rotation_int mode] takes a mode and returns the number of rotation
    required to find its intervalic formula once applied to the formula of the
    C_mode *)
let mode_rotation_int =
  let open Types in
  function
  | C_mode -> 0
  | D_mode -> 1
  | E_mode -> 2
  | F_mode -> 3
  | G_mode -> 4
  | A_mode -> 5
  | B_mode -> 6

let note_of_string s =
  let open Types in
  if String.length s < 2 then invalid_arg "note_of_string"
  else
    let base =
      match s.[0] with
      | 'a' -> A
      | 'b' -> B
      | 'c' -> C
      | 'd' -> D
      | 'e' -> E
      | 'f' -> F
      | 'g' -> G
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

(** [rotate_list_n l n] rotates the elements of l n times to the left *)
let rec rotate_list_n l n =
  if n = 0 then l
  else match l with [] -> [] | x :: r -> rotate_list_n (r @ [ x ]) (n - 1)

let compute_rotations base target =
  let rotation_needed = mode_rotation_int target in
  rotate_list_n base rotation_needed

let chords_of_mode =
  let open Types in
  let base_diatonic_chord_formula =
    [ Major; Minor; Minor; Major; Major; Minor; Diminished ]
  in
  fun mode -> compute_rotations base_diatonic_chord_formula mode

(** [intervals_of_mode mode] takes a mode and returns its intervalic formula by
    rotating the base formula of the C_mode *)
let intervals_of_mode =
  let base = [ 2; 2; 1; 2; 2; 2; 1 ] in
  fun mode -> compute_rotations base mode

(** [get_next_base_note_distance base_note] take a base_note a returns the
    intervalic distance to the next one *)
let get_next_base_note_distance =
  let open Types in
  function C -> 2 | D -> 2 | E -> 1 | F -> 2 | G -> 2 | A -> 2 | B -> 1

(** [get_next_note note] takes a base_note and returns the next one *)
let get_next_note =
  let open Types in
  function C -> D | D -> E | E -> F | F -> G | G -> A | A -> B | B -> C

let get_next_note =
  let open Types in
  fun { base; alteration } interval ->
    let distance = get_next_base_note_distance base in
    { base = get_next_note base; alteration = interval - distance + alteration }

let rec compute_tonality mode note =
  match mode with
  | [] -> [] (* | [] -> [ note ] if we want C D E F G A B C *)
  | interval :: mode ->
    let next = get_next_note note interval in
    note :: compute_tonality mode next

let build_tonality mode note =
  let intervalic_formula = intervals_of_mode mode in
  compute_tonality intervalic_formula note

let build_diatonic_triads_sequence mode note =
  let diatonic_chord_sequence = chords_of_mode mode in
  let tonality = build_tonality mode note in
  List.map2 (fun chord note -> (note, chord)) diatonic_chord_sequence tonality

(*
  TODO:
    * suite d'accords d'une tonalité donnée [x]
    * Tabs                                  []
      * map to fretboard                    []
      * function to print sequence          []
      * function to print chords            []
      * function to print notes             []
    * Markov Chord progressions             []
    * generer du midi                       []
    * generer partoche pdf                  []
 *)

let () =
  if Array.length Sys.argv <> 3 then
    failwith @@ Format.sprintf "Usage: %s <mode> <note>" Sys.argv.(0)
  else
    let fmt = Format.std_formatter in
    let mode = mode_of_string Sys.argv.(1) in
    let note = note_of_string Sys.argv.(2) in
    Pp.print_notes fmt @@ build_tonality mode note;
    Pp.print_diatonic_chords fmt @@ build_diatonic_triads_sequence mode note
