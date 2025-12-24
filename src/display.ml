(** Display module - renders fretboard with highlighting *)

(** Get all scale categories *)
let all_categories =
  let open Types in
  [Diatonic; HarmonicMinor; MelodicMinor; PentatonicBlues; Symmetric; Ethnic; Bebop]

(** Get scales in a category *)
let scales_in_category category =
  let open Types in
  match category with
  | Diatonic -> [Ionian; Dorian; Phrygian; Lydian; Mixolydian; Aeolian; Locrian]
  | HarmonicMinor -> [HarmonicMinor; LocrianNat6; IonianSharp5; DorianSharp4;
                      PhrygianDominant; LydianSharp2; SuperLocrianbb7]
  | MelodicMinor -> [MelodicMinor; DorianFlat2; LydianAugmented; LydianDominant;
                     MixolydianFlat6; LocrianNat2; Altered]
  | PentatonicBlues -> [MajorPentatonic; MinorPentatonic; BluesMajor; BluesMinor;
                        Hirajoshi; InSen]
  | Symmetric -> [WholeTone; DiminishedHW; DiminishedWH; Chromatic; Augmented; Tritone]
  | Ethnic -> [HungarianMinor; DoubleHarmonic; Persian; Arabian; Japanese;
               Egyptian; NeapolitanMinor; NeapolitanMajor]
  | Bebop -> [BebopDominant; BebopMajor; BebopMinor; BebopDorian]

(** Get category for a scale *)
let category_of_scale scale =
  let open Types in
  match scale with
  | Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian -> Diatonic
  | HarmonicMinor | LocrianNat6 | IonianSharp5 | DorianSharp4
  | PhrygianDominant | LydianSharp2 | SuperLocrianbb7 -> HarmonicMinor
  | MelodicMinor | DorianFlat2 | LydianAugmented | LydianDominant
  | MixolydianFlat6 | LocrianNat2 | Altered -> MelodicMinor
  | MajorPentatonic | MinorPentatonic | BluesMajor | BluesMinor
  | Hirajoshi | InSen -> PentatonicBlues
  | WholeTone | DiminishedHW | DiminishedWH | Chromatic | Augmented | Tritone -> Symmetric
  | HungarianMinor | DoubleHarmonic | Persian | Arabian | Japanese
  | Egyptian | NeapolitanMinor | NeapolitanMajor -> Ethnic
  | BebopDominant | BebopMajor | BebopMinor | BebopDorian -> Bebop

(** Get display name for a category *)
let category_name category =
  let open Types in
  match category with
  | Diatonic -> "Diatonic"
  | HarmonicMinor -> "Harm. Minor"
  | MelodicMinor -> "Mel. Minor"
  | PentatonicBlues -> "Penta/Blues"
  | Symmetric -> "Symmetric"
  | Ethnic -> "Ethnic"
  | Bebop -> "Bebop"

(** Get display name for a scale *)
let scale_name scale =
  let open Types in
  match scale with
  (* Diatonic *)
  | Ionian -> "Ionian (Major)"
  | Dorian -> "Dorian"
  | Phrygian -> "Phrygian"
  | Lydian -> "Lydian"
  | Mixolydian -> "Mixolydian"
  | Aeolian -> "Aeolian (Minor)"
  | Locrian -> "Locrian"
  (* Harmonic Minor *)
  | HarmonicMinor -> "Harmonic Minor"
  | LocrianNat6 -> "Locrian nat6"
  | IonianSharp5 -> "Ionian #5"
  | DorianSharp4 -> "Dorian #4"
  | PhrygianDominant -> "Phrygian Dom"
  | LydianSharp2 -> "Lydian #2"
  | SuperLocrianbb7 -> "Super Locr bb7"
  (* Melodic Minor *)
  | MelodicMinor -> "Melodic Minor"
  | DorianFlat2 -> "Dorian b2"
  | LydianAugmented -> "Lydian Aug"
  | LydianDominant -> "Lydian Dom"
  | MixolydianFlat6 -> "Mixolydian b6"
  | LocrianNat2 -> "Locrian nat2"
  | Altered -> "Altered"
  (* Pentatonic & Blues *)
  | MajorPentatonic -> "Major Penta"
  | MinorPentatonic -> "Minor Penta"
  | BluesMajor -> "Blues Major"
  | BluesMinor -> "Blues Minor"
  | Hirajoshi -> "Hirajoshi"
  | InSen -> "In Sen"
  (* Symmetric *)
  | WholeTone -> "Whole Tone"
  | DiminishedHW -> "Dim HW"
  | DiminishedWH -> "Dim WH"
  | Chromatic -> "Chromatic"
  | Augmented -> "Augmented"
  | Tritone -> "Tritone"
  (* Ethnic *)
  | HungarianMinor -> "Hungarian Min"
  | DoubleHarmonic -> "Double Harm"
  | Persian -> "Persian"
  | Arabian -> "Arabian"
  | Japanese -> "Japanese"
  | Egyptian -> "Egyptian"
  | NeapolitanMinor -> "Neapolitan Min"
  | NeapolitanMajor -> "Neapolitan Maj"
  (* Bebop *)
  | BebopDominant -> "Bebop Dom"
  | BebopMajor -> "Bebop Major"
  | BebopMinor -> "Bebop Minor"
  | BebopDorian -> "Bebop Dorian"

(** Get intervals for a scale type. Returns list of semitone steps. *)
let scale_intervals scale_type =
  let open Types in
  match scale_type with
  (* Diatonic modes - 7 notes *)
  | Ionian -> [2; 2; 1; 2; 2; 2; 1]
  | Dorian -> [2; 1; 2; 2; 2; 1; 2]
  | Phrygian -> [1; 2; 2; 2; 1; 2; 2]
  | Lydian -> [2; 2; 2; 1; 2; 2; 1]
  | Mixolydian -> [2; 2; 1; 2; 2; 1; 2]
  | Aeolian -> [2; 1; 2; 2; 1; 2; 2]
  | Locrian -> [1; 2; 2; 1; 2; 2; 2]
  (* Harmonic minor modes - 7 notes *)
  | HarmonicMinor -> [2; 1; 2; 2; 1; 3; 1]
  | LocrianNat6 -> [1; 2; 2; 1; 3; 1; 2]
  | IonianSharp5 -> [2; 2; 1; 3; 1; 2; 1]
  | DorianSharp4 -> [2; 1; 3; 1; 2; 1; 2]
  | PhrygianDominant -> [1; 3; 1; 2; 1; 2; 2]
  | LydianSharp2 -> [3; 1; 2; 1; 2; 2; 1]
  | SuperLocrianbb7 -> [1; 2; 1; 2; 2; 1; 3]
  (* Melodic minor modes - 7 notes *)
  | MelodicMinor -> [2; 1; 2; 2; 2; 2; 1]
  | DorianFlat2 -> [1; 2; 2; 2; 2; 1; 2]
  | LydianAugmented -> [2; 2; 2; 2; 1; 2; 1]
  | LydianDominant -> [2; 2; 2; 1; 2; 1; 2]
  | MixolydianFlat6 -> [2; 2; 1; 2; 1; 2; 2]
  | LocrianNat2 -> [2; 1; 2; 1; 2; 2; 2]
  | Altered -> [1; 2; 1; 2; 2; 2; 2]
  (* Pentatonic & Blues - 5-6 notes *)
  | MajorPentatonic -> [2; 2; 3; 2; 3]
  | MinorPentatonic -> [3; 2; 2; 3; 2]
  | BluesMajor -> [2; 1; 1; 3; 2; 3]
  | BluesMinor -> [3; 2; 1; 1; 3; 2]
  | Hirajoshi -> [2; 1; 4; 1; 4]
  | InSen -> [1; 4; 2; 3; 2]
  (* Symmetric - 6-12 notes *)
  | WholeTone -> [2; 2; 2; 2; 2; 2]
  | DiminishedHW -> [1; 2; 1; 2; 1; 2; 1; 2]
  | DiminishedWH -> [2; 1; 2; 1; 2; 1; 2; 1]
  | Chromatic -> [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1]
  | Augmented -> [3; 1; 3; 1; 3; 1]
  | Tritone -> [1; 3; 2; 1; 3; 2]
  (* Ethnic/Exotic - 5-7 notes *)
  | HungarianMinor -> [2; 1; 3; 1; 1; 3; 1]
  | DoubleHarmonic -> [1; 3; 1; 2; 1; 3; 1]
  | Persian -> [1; 3; 1; 1; 2; 3; 1]
  | Arabian -> [2; 2; 1; 1; 2; 2; 2]
  | Japanese -> [1; 4; 2; 1; 4]
  | Egyptian -> [2; 3; 2; 3; 2]
  | NeapolitanMinor -> [1; 2; 2; 2; 1; 3; 1]
  | NeapolitanMajor -> [1; 2; 2; 2; 2; 2; 1]
  (* Bebop - 8 notes *)
  | BebopDominant -> [2; 2; 1; 2; 2; 1; 1; 1]
  | BebopMajor -> [2; 2; 1; 2; 1; 1; 2; 1]
  | BebopMinor -> [2; 1; 1; 1; 2; 2; 1; 2]
  | BebopDorian -> [2; 1; 1; 1; 2; 2; 1; 2]

(** Build a tonality (scale notes) from scale type and root note *)
let build_tonality scale_type root =
  let open Types in
  let intervals = scale_intervals scale_type in
  let num_notes = List.length intervals in

  (* Build scale starting from root *)
  let base_notes = [| C; D; E; F; G; A; B |] in
  let base_to_int = function C -> 0 | D -> 1 | E -> 2 | F -> 3 | G -> 4 | A -> 5 | B -> 6 in
  let int_to_base i = base_notes.(i mod 7) in

  (* Calculate semitones from C for a note *)
  let note_to_semitone note =
    let base_semitones = match note.base with
      | C -> 0 | D -> 2 | E -> 4 | F -> 5 | G -> 7 | A -> 9 | B -> 11
    in
    (base_semitones + note.alteration + 12) mod 12
  in

  (* Build scale notes - handle variable length scales *)
  let scale = Array.make num_notes root in
  let current_semitone = ref (note_to_semitone root) in
  let current_base_idx = ref (base_to_int root.base) in
  let intervals_arr = Array.of_list intervals in

  for i = 1 to num_notes - 1 do
    current_semitone := (!current_semitone + intervals_arr.(i - 1)) mod 12;
    current_base_idx := (!current_base_idx + 1) mod 7;
    let next_base = int_to_base !current_base_idx in
    let natural_semitone = match next_base with
      | C -> 0 | D -> 2 | E -> 4 | F -> 5 | G -> 7 | A -> 9 | B -> 11
    in
    let alteration = (!current_semitone - natural_semitone + 12) mod 12 in
    let alteration = if alteration > 6 then alteration - 12 else alteration in
    scale.(i) <- { base = next_base; alteration }
  done;
  Array.to_list scale

(** Build degree table: maps pitch class (0-11) to scale degree (0-6) *)
let build_degree_tbl tonality =
  let tbl = Hashtbl.create 12 in
  List.iteri (fun degree note ->
    (* Use pitch class as key to handle all enharmonic equivalents *)
    let pitch_class = Conv.note_to_int note in
    Hashtbl.replace tbl pitch_class degree
  ) tonality;
  tbl

(** Get semitone intervals for a chord type (from root) *)
let chord_intervals (chord_type : Types.chord) =
  match chord_type with
  | Types.Major -> [0; 4; 7]              (* R, M3, P5 *)
  | Types.Minor -> [0; 3; 7]              (* R, m3, P5 *)
  | Types.Dimin -> [0; 3; 6]              (* R, m3, d5 *)
  | Types.Augment -> [0; 4; 8]            (* R, M3, A5 *)
  | Types.Suspend2 -> [0; 2; 7]           (* R, M2, P5 *)
  | Types.Suspend4 -> [0; 5; 7]           (* R, P4, P5 *)
  | Types.Major7 -> [0; 4; 7; 11]         (* R, M3, P5, M7 *)
  | Types.Domin7 -> [0; 4; 7; 10]         (* R, M3, P5, m7 *)
  | Types.Minor7 -> [0; 3; 7; 10]         (* R, m3, P5, m7 *)
  | Types.HalfDim7 -> [0; 3; 6; 10]       (* R, m3, d5, m7 *)
  | Types.Sixth -> [0; 4; 7; 9]           (* R, M3, P5, M6 *)
  | Types.MinorSixth -> [0; 3; 7; 9]      (* R, m3, P5, M6 *)

(** Build chord notes from root and chord type - returns pitch classes *)
let build_chord_pitch_classes root chord_type =
  let root_pc = Conv.note_to_int root in
  let intervals = chord_intervals chord_type in
  List.map (fun interval -> (root_pc + interval) mod 12) intervals

(** Build degree table for chord: maps pitch class (0-11) to chord degree (0-n) *)
let build_chord_degree_tbl root chord_type =
  let tbl = Hashtbl.create 12 in
  let pitch_classes = build_chord_pitch_classes root chord_type in
  List.iteri (fun degree pc ->
    Hashtbl.replace tbl pc degree
  ) pitch_classes;
  tbl

let select_view ctx ocamuse_context =
  let open Types in
  let bubble_view = function
    | Flat view -> view
    | Pattern (view, _scale) -> view
  in
  let make_struc ctx scale view notes_to_degree_tbl color_theme
    ocamuse_context : Types.pattern_view_draw_struc =
    let cursor_i = ref 0 in
    let cursor_j = ref 0 in
    let offset_for_frets_numbers =
      match view with Plain _ -> 0 | Fretted _ -> 2 | Interline _ -> 1
    in
    let fretboard = ocamuse_context.fretboard.notes in
    let offset = ref offset_for_frets_numbers in
    let number_of_strings = Array.length ocamuse_context.fretboard.notes in
    let color = Color.event_to_base_color !(ocamuse_context.base_colour) in
    let number_of_frets = Array.length ocamuse_context.fretboard.notes.(0) in
    { ctx
    ; scale
    ; view
    ; color
    ; offset
    ; cursor_i
    ; cursor_j
    ; fretboard
    ; string = ref 0
    ; number_of_frets
    ; number_of_strings
    ; notes_to_degree_tbl
    ; color_theme
    }
  in
  LTerm_draw.clear ctx;
  (* Build highlighting tables based on highlight_source *)
  let notes_to_degree_tbl =
    match ocamuse_context.highlight_source with
    | Tonality (scale, root) ->
      let tonality = build_tonality scale root in
      build_degree_tbl tonality
    | Chord (root, chord_type) ->
      (* Build chord degree table for highlighting *)
      build_chord_degree_tbl root chord_type
    | Arpeggio (root, chord_type) ->
      (* Arpeggio uses same logic as chord *)
      build_chord_degree_tbl root chord_type
  in
  let display_mode = !(ocamuse_context.display_mode) in
  let view = bubble_view display_mode in
  begin
    match display_mode with
    | Flat _base_colour ->
      let struc =
        make_struc ctx Ionian view notes_to_degree_tbl
          ocamuse_context.color_theme ocamuse_context
      in
      Draw.PATTERN.pattern struc
    | Pattern (view, scale) ->
      let struc =
        make_struc ctx scale view notes_to_degree_tbl
          ocamuse_context.color_theme ocamuse_context
      in
      Draw.PATTERN.pattern struc
  end
