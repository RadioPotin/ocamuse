let name_chord =
  let open Types in
  fun (root : note) (chord_type : chord) : string ->
    let base_str = Pp.Notes.sprint_note root in
    let suffix_str = Conv.chord_to_string chord_type in
    base_str ^ suffix_str

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
let get_next_base_note =
  let open Types in
  function C -> D | D -> E | E -> F | F -> G | G -> A | A -> B | B -> C

let get_next_degree =
  let open Types in
  fun { base; alteration } interval ->
    let distance = get_next_base_note_distance base in
    { base = get_next_base_note base
    ; alteration = interval - distance + alteration
    }

let rec compute_tonality mode note =
  match mode with
  | [] -> [] (* | [] -> [ note ] if we want C D E F G A B C *)
  | interval :: mode ->
    let next = get_next_degree note interval in
    note :: compute_tonality mode next

let build_tonality mode note =
  let intervalic_formula = intervals_of_mode mode in
  compute_tonality intervalic_formula note

let build_diatonic_triads_sequence mode note =
  let diatonic_chord_sequence = chords_of_mode mode in
  let tonality = build_tonality mode note in
  List.map2 (fun chord note -> (note, chord)) diatonic_chord_sequence tonality

(** [generate_chord note chord_type] generates a list of [note]s for the chord
    defined by [chord_type] with [note] as the root note
let generate_chord (root_note : Types.note) (chord_type : Types.chord) :
    Types.note list =
  let intervals =
    match chord_type with
    | Major -> [ 0; 4; 7 ]
    | Minor -> [ 0; 3; 7 ]
    | Dimin -> [ 0; 3; 6 ]
    | Augment -> [ 0; 4; 8 ]
    | Suspend2 -> [ 0; 2; 7 ]
    | Suspend4 -> [ 0; 5; 7 ]
    | Major7 -> [ 0; 4; 7; 11 ]
    | Domin7 -> [ 0; 4; 7; 10 ]
    | Minor7 -> [ 0; 3; 7; 10 ]
    | HalfDim7 -> [ 0; 3; 6; 10 ]
    | Sixth -> [ 0; 4; 7; 9 ]
    | MinorSixth -> [ 0; 3; 7; 9 ]
  in
  let note_to_add_semitones_to = { root_note with alteration = 0 } in
  let rec generate_chord_helper notes intervals_remaining =
    match intervals_remaining with
    | [] -> List.rev notes
    | hd :: tl ->
      let next_note = add_semitones note_to_add_semitones_to hd in
      generate_chord_helper (next_note :: notes) tl
  in
  generate_chord_helper [ root_note ] intervals
 *)

(*
  TODO:
    * suite d'accords d'une tonalité donnée [x]
    * Fretboard print                       [x]
    * Tabs                                  []
      * map to fretboard                    []
      * function to print sequence          []
      * function to print chords            []
      * function to print notes             []
    * Markov Chord progressions             []
    * generer du midi                       []
    * generer partoche pdf                  []
 *)
