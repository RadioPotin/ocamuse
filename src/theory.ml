let name_chord =
  let open Types in
  fun (root : note) (chord_type : chord) : string ->
    let base_str = Pp.NOTES.FMT.sprint_note root in
    let suffix_str = Conv.chord_to_string chord_type in
    Fmt.str "%s%s" base_str suffix_str

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
  List.map2 (fun chord note -> (note, chord)) diatonic_chord_sequence tonality

(** Build a note from a root by adding semitones chromatically *)
let build_note_by_semitones root semitones =
  let open Types in
  (* Convert root to pitch class *)
  let root_pc = match root.base with
    | C -> 0 | D -> 2 | E -> 4 | F -> 5 | G -> 7 | A -> 9 | B -> 11
  in
  let root_pc_with_alt = root_pc + root.alteration in

  (* Calculate target pitch class *)
  let target_pc_raw = root_pc_with_alt + semitones in
  let target_pc = ((target_pc_raw mod 12) + 12) mod 12 in

  (* Find the appropriate base note based on interval from root *)
  (* For triads, we want: root, skip one letter, skip one letter *)
  let base_offset = match semitones with
    | 3 | 4 -> 2      (* Third: skip 2 letter names (C->E, D->F, etc.) *)
    | 6 | 7 | 8 -> 4  (* Fifth: skip 4 letter names (C->G, D->A, etc.) *)
    | _ -> 0          (* Root or other *)
  in

  let root_base_idx = match root.base with
    | C -> 0 | D -> 1 | E -> 2 | F -> 3 | G -> 4 | A -> 5 | B -> 6
  in
  let target_base_idx = (root_base_idx + base_offset) mod 7 in
  let target_base = match target_base_idx with
    | 0 -> C | 1 -> D | 2 -> E | 3 -> F | 4 -> G | 5 -> A | 6 -> B | _ -> C
  in

  (* Calculate what alteration we need *)
  let target_base_pc = match target_base with
    | C -> 0 | D -> 2 | E -> 4 | F -> 5 | G -> 7 | A -> 9 | B -> 11
  in
  let alteration = target_pc - target_base_pc in

  {base = target_base; alteration}

(** Build notes of a triad chord (root, third, fifth) *)
let build_triad_notes root (chord_type : Types.chord) =
  let open Types in
  let (third_interval, fifth_interval) = match chord_type with
    | Major -> (4, 7)      (* Major third + Perfect fifth *)
    | Minor -> (3, 7)      (* Minor third + Perfect fifth *)
    | Dimin -> (3, 6)      (* Minor third + Diminished fifth *)
    | Augment -> (4, 8)    (* Major third + Augmented fifth *)
    | _ -> (4, 7)          (* Default to major for other types *)
  in
  let third = build_note_by_semitones root third_interval in
  let fifth = build_note_by_semitones root fifth_interval in
  [root; third; fifth]

(** Apply inversion to chord notes
    - 0 = root position: [1, 3, 5]
    - 1 = 1st inversion: [3, 5, 1] (third in bass)
    - 2 = 2nd inversion: [5, 1, 3] (fifth in bass)
*)
let invert_chord_notes notes inversion =
  let inv = inversion mod 3 in  (* Triads have 3 inversions *)
  match inv with
  | 0 -> notes  (* Root position *)
  | 1 -> (match notes with [a; b; c] -> [b; c; a] | _ -> notes)  (* 1st inversion *)
  | 2 -> (match notes with [a; b; c] -> [c; a; b] | _ -> notes)  (* 2nd inversion *)
  | _ -> notes

(** Convert note to chromatic pitch class (0-11) *)
let note_to_pitch_class note =
  let open Types in
  let base_semitones = match note.base with
    | C -> 0 | D -> 2 | E -> 4 | F -> 5 | G -> 7 | A -> 9 | B -> 11
  in
  (base_semitones + note.alteration) mod 12

let build_degree_tbl mode =
  let degree_tbl = Hashtbl.create 512 in

  (* Build pitch class to degree mapping *)
  let pitch_class_to_degree = Hashtbl.create 12 in
  List.iteri (fun i note ->
    let pc = note_to_pitch_class note in
    Hashtbl.add pitch_class_to_degree pc i
  ) mode;

  (* Generate all enharmonic equivalents *)
  let all_bases = [Types.C; Types.D; Types.E; Types.F; Types.G; Types.A; Types.B] in
  List.iter (fun base ->
    for alt = -2 to 2 do
      let note = Types.{base; alteration = alt} in
      let pc = note_to_pitch_class note in
      match Hashtbl.find_opt pitch_class_to_degree pc with
      | Some degree ->
          Hashtbl.add degree_tbl note degree;
      | None -> ()
    done
  ) all_bases;

  degree_tbl

let build_degree_colour_tbl mode theme =
  let degree_colour_tbl = Hashtbl.create 512 in
  List.iteri
    (fun i note ->
      let color = Color_theme.get_note_color theme note (Some i) in
      Hashtbl.add degree_colour_tbl i color )
    mode;
  degree_colour_tbl

(** Build hashtable for chord/arpeggio highlighting *)
let build_chord_highlight_tables chord_notes theme =
  let notes_tbl = Hashtbl.create 512 in
  let degree_tbl = Hashtbl.create 512 in

  (* Build a set of pitch classes for this chord *)
  let pitch_classes = List.map note_to_pitch_class chord_notes in
  let pitch_class_to_degree = Hashtbl.create 12 in
  List.iteri (fun i pitch_class ->
    Hashtbl.add pitch_class_to_degree pitch_class i
  ) pitch_classes;

  (* Generate all possible note representations for each pitch class *)
  let all_bases = [Types.C; Types.D; Types.E; Types.F; Types.G; Types.A; Types.B] in
  List.iter (fun base ->
    for alt = -2 to 2 do
      let note = Types.{base; alteration = alt} in
      let pc = note_to_pitch_class note in
      match Hashtbl.find_opt pitch_class_to_degree pc with
      | Some degree ->
          Hashtbl.add notes_tbl note degree;
      | None -> ()
    done
  ) all_bases;

  (* Build color table using theme *)
  List.iteri (fun i note ->
    let color = Color_theme.get_note_color theme note (Some i) in
    Hashtbl.add degree_tbl i color
  ) chord_notes;

  (notes_tbl, degree_tbl)

let is_diatonic struc note =
  let open Types in
  Hashtbl.mem struc.notes_to_degree_tbl note


(** Chord voicing - represents a playable chord position on the fretboard.
    Each element is (string_index, fret_number) or None for muted strings. *)
type chord_voicing =
  { positions : (int * int) option array  (** One per string: Some (string, fret) or None *)
  ; start_fret : int  (** Starting fret for the diagram (usually lowest fret used) *)
  ; bass_note : Types.note  (** The lowest note played (bass note) *)
  }


(** [find_chord_voicings fretboard chord_notes max_frets] finds playable chord
    voicings on the fretboard. Returns a list of voicings sorted by
    playability (lower fret positions first). *)
let find_chord_voicings (fretboard : Types.fretboard_data) chord_notes =
  let open Types in
  let num_strings = Array.length fretboard.notes in
  let num_frets = if num_strings > 0 then Array.length fretboard.notes.(0) else 0 in

  (* Convert chord notes to pitch classes for matching *)
  let chord_pcs = List.map note_to_pitch_class chord_notes in
  let chord_pc_set = Hashtbl.create 12 in
  List.iter (fun pc -> Hashtbl.add chord_pc_set pc ()) chord_pcs;

  let voicings = ref [] in

  (* Try to find voicings starting from each fret position (open to fret 10) *)
  for start_fret = 0 to min 10 (num_frets - 1) do
    (* For each starting fret, try to build a chord shape within 4-fret span *)
    let max_stretch = 4 in
    let end_fret = min (start_fret + max_stretch) (num_frets - 1) in

    (* Try all combinations of positions within this range *)
    let positions = Array.make num_strings None in
    let found_notes = ref [] in

    (* For each string, find the best note to play *)
    for string_idx = 0 to num_strings - 1 do
      let best_fret = ref None in

      for fret = start_fret to end_fret do
        let note = fretboard.notes.(string_idx).(fret) in
        let pc = note_to_pitch_class note in

        if Hashtbl.mem chord_pc_set pc then begin
          match !best_fret with
          | None ->
            best_fret := Some fret;
            found_notes := note :: !found_notes
          | Some current_fret ->
            (* Prefer lower frets within range *)
            if fret < current_fret then begin
              best_fret := Some fret;
              found_notes := note :: !found_notes
            end
        end
      done;

      positions.(string_idx) <- Option.map (fun f -> (string_idx, f)) !best_fret
    done;

    (* Check if we have at least 3 notes and all chord tones present *)
    let played_pcs = List.map note_to_pitch_class !found_notes in
    let unique_pcs = Hashtbl.create 12 in
    List.iter (fun pc -> Hashtbl.add unique_pcs pc ()) played_pcs;

    let has_all_chord_tones =
      List.for_all (fun pc -> Hashtbl.mem unique_pcs pc) chord_pcs
    in

    if has_all_chord_tones && List.length !found_notes >= 3 then begin
      (* Find the bass note (lowest string with a note) *)
      let bass_note = ref None in
      for string_idx = num_strings - 1 downto 0 do
        match positions.(string_idx) with
        | Some (_, fret) ->
          if !bass_note = None then
            bass_note := Some fretboard.notes.(string_idx).(fret)
        | None -> ()
      done;

      match !bass_note with
      | Some bn ->
        voicings := {
          positions;
          start_fret = if start_fret = 0 then 0 else start_fret;
          bass_note = bn;
        } :: !voicings
      | None -> ()
    end
  done;

  (* Sort by start_fret (prefer lower positions) *)
  List.sort (fun v1 v2 -> compare v1.start_fret v2.start_fret) !voicings


(*
  TODO:
    * Add lambda_term support for interactive experience        [x]
    * display diatonic chord names                              [x]
    * Fretboard features:
      * create simple pattern highlighting function for Cmajor  [x]
      * Navigate all modes of given tonality with highlighting  []
      * Highlight an arpeggio                                   []
      * Highlight a path of notes on the fb                     []
      * input patterns                                          []

    * Chord diagrams                                            []
      * Display the diatonic chord diagrams                     []
      * Select a chord, a voicing, and generate the diagram     []

    * Scale degree sequence (progressions)                      []
      * Select from an array of predefined chord progressions   []
    * Tabs                                                      []
      * map to fretboard                                        []
      * function to print sequence                              []
      * function to print chords                                []
      * function to print notes                                 []
    * Markov Chord progressions                                 []
    * generate midi                                             []
    * generate pdf music sheet                                  []
 *)
