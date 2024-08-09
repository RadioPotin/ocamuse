let name_chord =
  let open Types in
  fun (root : note) (chord_type : chord) : string ->
    let base_str = Pp.NOTES.FMT.sprint_note root in
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

(** [get_next_base_note note] takes a base_note and returns the next one *)
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
  List.map2
    (fun chord note -> (note, chord)) diatonic_chord_sequence tonality

let build_degree_tbl mode =
  let degree_tbl = Hashtbl.create 512 in
  List.iteri (fun i note -> Hashtbl.add degree_tbl note (i)) mode;
  degree_tbl

let build_degree_colour_tbl mode =
  let degree_colour_tbl = Hashtbl.create 512 in
  List.iteri
    (fun i _note ->
        let color = Conv.degree_to_colour (i) in
        let color = Pp.COLOR.event_to_color color in
        Hashtbl.add degree_colour_tbl (i) color)
    mode;
  degree_colour_tbl


let is_diatonic struc note =
  let open Types in
  Hashtbl.mem struc.notes_to_degree_tbl note

(*
  TODO:
    * suite d'accords d'une tonalité donnée  [x]
    * Fretboard print                        [x]
      * build patterns to apply to a board   []
      * create pattern highlighting function []
    * Chord diagrams                         []
    * Scale degree sequence (progressions)   []
    * Tabs                                   []
      * map to fretboard                     []
      * function to print sequence           []
      * function to print chords             []
      * function to print notes              []
    * Add lambda_term support for
    interactive experience                   []
    * Markov Chord progressions              []
    * generer du midi                        []
    * generer partoche pdf                   []
 *)
