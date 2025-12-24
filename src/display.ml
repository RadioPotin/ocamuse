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
  (* Triads *)
  | Types.ChordMajor -> [0; 4; 7]           (* R, M3, P5 *)
  | Types.ChordMinor -> [0; 3; 7]           (* R, m3, P5 *)
  | Types.ChordDimin -> [0; 3; 6]           (* R, m3, d5 *)
  | Types.ChordAugment -> [0; 4; 8]         (* R, M3, A5 *)
  (* Suspended *)
  | Types.ChordSus2 -> [0; 2; 7]            (* R, M2, P5 *)
  | Types.ChordSus4 -> [0; 5; 7]            (* R, P4, P5 *)
  (* Sixths *)
  | Types.ChordSixth -> [0; 4; 7; 9]        (* R, M3, P5, M6 *)
  | Types.ChordMinorSixth -> [0; 3; 7; 9]   (* R, m3, P5, M6 *)
  (* Sevenths *)
  | Types.ChordMaj7 -> [0; 4; 7; 11]        (* R, M3, P5, M7 *)
  | Types.ChordDom7 -> [0; 4; 7; 10]        (* R, M3, P5, m7 *)
  | Types.ChordMin7 -> [0; 3; 7; 10]        (* R, m3, P5, m7 *)
  | Types.ChordHalfDim7 -> [0; 3; 6; 10]    (* R, m3, d5, m7 *)
  | Types.ChordDim7 -> [0; 3; 6; 9]         (* R, m3, d5, d7 *)
  | Types.ChordMinMaj7 -> [0; 3; 7; 11]     (* R, m3, P5, M7 *)
  | Types.ChordAug7 -> [0; 4; 8; 10]        (* R, M3, A5, m7 *)
  | Types.ChordDom7Sus4 -> [0; 5; 7; 10]    (* R, P4, P5, m7 *)
  (* Extended - 9ths *)
  | Types.ChordMaj9 -> [0; 4; 7; 11; 14]    (* R, M3, P5, M7, M9 *)
  | Types.ChordDom9 -> [0; 4; 7; 10; 14]    (* R, M3, P5, m7, M9 *)
  | Types.ChordMin9 -> [0; 3; 7; 10; 14]    (* R, m3, P5, m7, M9 *)
  | Types.ChordAdd9 -> [0; 4; 7; 14]        (* R, M3, P5, M9 *)
  | Types.ChordMinAdd9 -> [0; 3; 7; 14]     (* R, m3, P5, M9 *)
  | Types.ChordSixNine -> [0; 4; 7; 9; 14]  (* R, M3, P5, M6, M9 *)
  (* Extended - 11ths *)
  | Types.ChordDom11 -> [0; 4; 7; 10; 14; 17]   (* R, M3, P5, m7, M9, P11 *)
  | Types.ChordMin11 -> [0; 3; 7; 10; 14; 17]   (* R, m3, P5, m7, M9, P11 *)
  | Types.ChordMaj11 -> [0; 4; 7; 11; 14; 17]   (* R, M3, P5, M7, M9, P11 *)
  | Types.ChordAdd11 -> [0; 4; 7; 17]           (* R, M3, P5, P11 *)
  (* Extended - 13ths *)
  | Types.ChordDom13 -> [0; 4; 7; 10; 14; 21]   (* R, M3, P5, m7, M9, M13 *)
  | Types.ChordMin13 -> [0; 3; 7; 10; 14; 21]   (* R, m3, P5, m7, M9, M13 *)
  | Types.ChordMaj13 -> [0; 4; 7; 11; 14; 21]   (* R, M3, P5, M7, M9, M13 *)
  (* Altered *)
  | Types.ChordDom7Sharp9 -> [0; 4; 7; 10; 15]  (* R, M3, P5, m7, #9 *)
  | Types.ChordDom7Flat9 -> [0; 4; 7; 10; 13]   (* R, M3, P5, m7, b9 *)
  | Types.ChordDom7Sharp5 -> [0; 4; 8; 10]      (* R, M3, A5, m7 *)
  | Types.ChordDom7Flat5 -> [0; 4; 6; 10]       (* R, M3, d5, m7 *)
  | Types.ChordDom7Sharp5Sharp9 -> [0; 4; 8; 10; 15]  (* R, M3, A5, m7, #9 *)
  | Types.ChordDom7Flat5Flat9 -> [0; 4; 6; 10; 13]    (* R, M3, d5, m7, b9 *)
  | Types.ChordDom7Alt -> [0; 4; 6; 10; 13; 15]       (* R, M3, d5, m7, b9, #9 *)
  | Types.ChordDom7Flat13 -> [0; 4; 7; 10; 20]        (* R, M3, P5, m7, b13 *)

(** Get all chord categories *)
let all_chord_categories =
  let open Types in
  [Triads; Suspended; Sixths; Sevenths; Extended; AlteredChords]

(** Get chords in a category *)
let chords_in_category category =
  let open Types in
  match category with
  | Triads -> [ChordMajor; ChordMinor; ChordDimin; ChordAugment]
  | Suspended -> [ChordSus2; ChordSus4]
  | Sixths -> [ChordSixth; ChordMinorSixth]
  | Sevenths -> [ChordMaj7; ChordDom7; ChordMin7; ChordHalfDim7;
                 ChordDim7; ChordMinMaj7; ChordAug7; ChordDom7Sus4]
  | Extended -> [ChordMaj9; ChordDom9; ChordMin9; ChordAdd9; ChordMinAdd9; ChordSixNine;
                 ChordDom11; ChordMin11; ChordMaj11; ChordAdd11;
                 ChordDom13; ChordMin13; ChordMaj13]
  | AlteredChords -> [ChordDom7Sharp9; ChordDom7Flat9; ChordDom7Sharp5; ChordDom7Flat5;
                      ChordDom7Sharp5Sharp9; ChordDom7Flat5Flat9; ChordDom7Alt; ChordDom7Flat13]

(** Get category for a chord *)
let category_of_chord chord =
  let open Types in
  match chord with
  | ChordMajor | ChordMinor | ChordDimin | ChordAugment -> Triads
  | ChordSus2 | ChordSus4 -> Suspended
  | ChordSixth | ChordMinorSixth -> Sixths
  | ChordMaj7 | ChordDom7 | ChordMin7 | ChordHalfDim7
  | ChordDim7 | ChordMinMaj7 | ChordAug7 | ChordDom7Sus4 -> Sevenths
  | ChordMaj9 | ChordDom9 | ChordMin9 | ChordAdd9 | ChordMinAdd9 | ChordSixNine
  | ChordDom11 | ChordMin11 | ChordMaj11 | ChordAdd11
  | ChordDom13 | ChordMin13 | ChordMaj13 -> Extended
  | ChordDom7Sharp9 | ChordDom7Flat9 | ChordDom7Sharp5 | ChordDom7Flat5
  | ChordDom7Sharp5Sharp9 | ChordDom7Flat5Flat9 | ChordDom7Alt | ChordDom7Flat13 -> AlteredChords

(** Get display name for a chord category *)
let chord_category_name category =
  let open Types in
  match category with
  | Triads -> "Triads"
  | Suspended -> "Suspended"
  | Sixths -> "Sixths"
  | Sevenths -> "Sevenths"
  | Extended -> "Extended"
  | AlteredChords -> "Altered"

(** Get display name for a chord *)
let chord_name chord =
  let open Types in
  match chord with
  (* Triads *)
  | ChordMajor -> "Major"
  | ChordMinor -> "Minor"
  | ChordDimin -> "Diminished"
  | ChordAugment -> "Augmented"
  (* Suspended *)
  | ChordSus2 -> "Sus2"
  | ChordSus4 -> "Sus4"
  (* Sixths *)
  | ChordSixth -> "6th"
  | ChordMinorSixth -> "Minor 6th"
  (* Sevenths *)
  | ChordMaj7 -> "Major 7th"
  | ChordDom7 -> "Dominant 7th"
  | ChordMin7 -> "Minor 7th"
  | ChordHalfDim7 -> "Half-Dim 7th"
  | ChordDim7 -> "Dim 7th"
  | ChordMinMaj7 -> "Minor-Maj 7th"
  | ChordAug7 -> "Aug 7th"
  | ChordDom7Sus4 -> "7sus4"
  (* Extended *)
  | ChordMaj9 -> "Major 9th"
  | ChordDom9 -> "Dominant 9th"
  | ChordMin9 -> "Minor 9th"
  | ChordAdd9 -> "Add 9"
  | ChordMinAdd9 -> "Minor Add 9"
  | ChordSixNine -> "6/9"
  | ChordDom11 -> "Dominant 11th"
  | ChordMin11 -> "Minor 11th"
  | ChordMaj11 -> "Major 11th"
  | ChordAdd11 -> "Add 11"
  | ChordDom13 -> "Dominant 13th"
  | ChordMin13 -> "Minor 13th"
  | ChordMaj13 -> "Major 13th"
  (* Altered *)
  | ChordDom7Sharp9 -> "7#9"
  | ChordDom7Flat9 -> "7b9"
  | ChordDom7Sharp5 -> "7#5"
  | ChordDom7Flat5 -> "7b5"
  | ChordDom7Sharp5Sharp9 -> "7#5#9"
  | ChordDom7Flat5Flat9 -> "7b5b9"
  | ChordDom7Alt -> "7alt"
  | ChordDom7Flat13 -> "7b13"

(** Compute diatonic triads for a 7-note scale.
    Returns list of (note, triad_type, chord_type) for each degree.
    Only works for 7-note scales; returns empty list for others. *)
let diatonic_triads scale_type root =
  let open Types in
  let intervals = scale_intervals scale_type in
  (* Only compute for 7-note scales *)
  if List.length intervals <> 7 then []
  else begin
    let tonality = build_tonality scale_type root in
    let tonality_arr = Array.of_list tonality in

    (* Convert intervals to cumulative semitones from root *)
    let cumulative = Array.make 7 0 in
    let sum = ref 0 in
    List.iteri (fun i interval ->
      cumulative.(i) <- !sum;
      sum := !sum + interval
    ) intervals;

    (* For each degree, compute the triad quality by stacking thirds *)
    List.mapi (fun degree _note ->
      (* Get semitones to 3rd (degree + 2) and 5th (degree + 4) *)
      let third_degree = (degree + 2) mod 7 in
      let fifth_degree = (degree + 4) mod 7 in

      (* Calculate intervals *)
      let root_semitone = cumulative.(degree) in
      let third_semitone = cumulative.(third_degree) in
      let fifth_semitone = cumulative.(fifth_degree) in

      (* Handle wraparound *)
      let third_interval =
        let diff = third_semitone - root_semitone in
        if diff < 0 then diff + 12 else diff
      in
      let fifth_interval =
        let diff = fifth_semitone - root_semitone in
        if diff < 0 then diff + 12 else diff
      in

      (* Determine triad quality based on intervals *)
      let (triad, chord) = match (third_interval, fifth_interval) with
        | (4, 7) -> (TriadMajor, ChordMajor)     (* Major: M3 + P5 *)
        | (3, 7) -> (TriadMinor, ChordMinor)     (* Minor: m3 + P5 *)
        | (3, 6) -> (TriadDiminished, ChordDimin) (* Dim: m3 + d5 *)
        | (4, 8) -> (TriadMajor, ChordAugment)   (* Aug: M3 + A5 - treat as major variant *)
        | _ -> (TriadMajor, ChordMajor)          (* Default fallback *)
      in
      (tonality_arr.(degree), triad, chord, degree + 1)  (* degree is 1-indexed for display *)
    ) tonality
  end

(** Get Roman numeral for a degree and triad type *)
let degree_to_roman degree triad =
  let open Types in
  let numeral = match degree with
    | 1 -> "I" | 2 -> "II" | 3 -> "III" | 4 -> "IV"
    | 5 -> "V" | 6 -> "VI" | 7 -> "VII" | _ -> "?"
  in
  match triad with
  | TriadMajor -> numeral
  | TriadMinor -> String.lowercase_ascii numeral
  | TriadDiminished -> String.lowercase_ascii numeral ^ "°"

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
