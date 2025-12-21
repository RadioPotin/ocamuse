(* {1 Chord types} *)

type triad_quality =
  | Major
  | Minor
  | Diminished
  | Augmented
  | Sus2
  | Sus4

type seventh_quality =
  | Major7
  | Minor7
  | Dominant7
  | HalfDiminished7
  | Diminished7
  | MinorMajor7
  | Augmented7

type extension =
  | Ninth of seventh_quality
  | Eleventh of seventh_quality
  | Thirteenth of seventh_quality
  | Add9
  | Add11
  | Sixth
  | MinorSixth

type chord_type =
  | Triad of triad_quality
  | Seventh of seventh_quality
  | Extended of extension

type t =
  { root : Note.t
  ; chord_type : chord_type
  ; notes : Note.t list
  }

(* {1 Helper: build notes from intervals} *)

let build_chord_notes root intervals =
  (* Build chord tones by applying intervals to root *)
  let rec build_notes _current_note remaining_intervals acc =
    match remaining_intervals with
    | [] -> List.rev (root :: acc)
    | interval :: rest ->
      (* Calculate target pitch *)
      let current_pitch = Note.to_pitch root in
      let target_pitch = Pitch.add current_pitch (Interval.to_semitones interval) in

      (* Determine proper note name based on interval *)
      let letter_dist = Interval.letter_distance interval in
      let target_base =
        let rec advance_base b steps =
          if steps = 0 then b
          else advance_base (Note.next_base b) (steps - 1)
        in
        let { Note.base = root_base; alteration = _ } = root in
        advance_base root_base letter_dist
      in

      (* Calculate alteration needed *)
      let natural_pitch = Note.to_pitch (Note.natural target_base) in
      let alteration = Pitch.diff natural_pitch target_pitch in
      let alteration = if alteration > 6 then alteration - 12 else alteration in
      let alteration = if alteration < -6 then alteration + 12 else alteration in
      let note = Note.make target_base (-alteration) in

      build_notes note rest (note :: acc)
  in
  build_notes root intervals []

(* {1 Interval patterns for chord types} *)

let triad_intervals = function
  | Major -> [Interval.MajorThird; Interval.PerfectFifth]
  | Minor -> [Interval.MinorThird; Interval.PerfectFifth]
  | Diminished -> [Interval.MinorThird; Interval.Tritone]
  | Augmented -> [Interval.MajorThird; Interval.MinorSixth] (* Augmented fifth *)
  | Sus2 -> [Interval.MajorSecond; Interval.PerfectFifth]
  | Sus4 -> [Interval.PerfectFourth; Interval.PerfectFifth]

let seventh_intervals = function
  | Major7 -> [Interval.MajorThird; Interval.PerfectFifth; Interval.MajorSeventh]
  | Minor7 -> [Interval.MinorThird; Interval.PerfectFifth; Interval.MinorSeventh]
  | Dominant7 -> [Interval.MajorThird; Interval.PerfectFifth; Interval.MinorSeventh]
  | HalfDiminished7 -> [Interval.MinorThird; Interval.Tritone; Interval.MinorSeventh]
  | Diminished7 -> [Interval.MinorThird; Interval.Tritone; Interval.MajorSixth] (* dim7 *)
  | MinorMajor7 -> [Interval.MinorThird; Interval.PerfectFifth; Interval.MajorSeventh]
  | Augmented7 -> [Interval.MajorThird; Interval.MinorSixth; Interval.MinorSeventh]

(* {1 Construction} *)

let make root chord_type =
  let intervals =
    match chord_type with
    | Triad q -> triad_intervals q
    | Seventh q -> seventh_intervals q
    | Extended (Ninth q) ->
      seventh_intervals q @ [Interval.MajorSecond] (* 9th = octave + major 2nd *)
    | Extended (Eleventh q) ->
      seventh_intervals q @ [Interval.MajorSecond; Interval.PerfectFourth]
    | Extended (Thirteenth q) ->
      seventh_intervals q @ [Interval.MajorSecond; Interval.PerfectFourth; Interval.MajorSixth]
    | Extended Add9 ->
      triad_intervals Major @ [Interval.MajorSecond]
    | Extended Add11 ->
      triad_intervals Major @ [Interval.PerfectFourth]
    | Extended Sixth ->
      triad_intervals Major @ [Interval.MajorSixth]
    | Extended MinorSixth ->
      triad_intervals Minor @ [Interval.MajorSixth]
  in
  let notes = build_chord_notes root intervals in
  { root; chord_type; notes }

let triad root quality =
  make root (Triad quality)

let seventh root quality =
  make root (Seventh quality)

let major root = triad root Major
let minor root = triad root Minor
let diminished root = triad root Diminished
let augmented root = triad root Augmented

let major7 root = seventh root Major7
let minor7 root = seventh root Minor7
let dominant7 root = seventh root Dominant7
let half_diminished7 root = seventh root HalfDiminished7

(* {1 Construction from intervals} *)

let from_intervals root intervals =
  (* Try to match interval pattern to known chord types *)
  let semitones = List.map Interval.to_semitones intervals |> List.sort compare in
  match semitones with
  | [3; 7] -> Some (triad root Minor)
  | [4; 7] -> Some (triad root Major)
  | [3; 6] -> Some (triad root Diminished)
  | [4; 8] -> Some (triad root Augmented)
  | [2; 7] -> Some (triad root Sus2)
  | [5; 7] -> Some (triad root Sus4)
  | [3; 7; 10] -> Some (seventh root Minor7)
  | [4; 7; 11] -> Some (seventh root Major7)
  | [4; 7; 10] -> Some (seventh root Dominant7)
  | [3; 6; 10] -> Some (seventh root HalfDiminished7)
  | [3; 6; 9] -> Some (seventh root Diminished7)
  | _ -> None

let from_notes note_list =
  match note_list with
  | [] -> None
  | root :: rest ->
    let root_pitch = Note.to_pitch root in
    let intervals =
      List.map (fun n ->
        let n_pitch = Note.to_pitch n in
        let semitones = Pitch.diff root_pitch n_pitch in
        Interval.of_semitones semitones
      ) rest
      |> List.filter_map (fun x -> x)
    in
    from_intervals root intervals

(* {1 Chord tones} *)

let notes chord = chord.notes

let root chord = chord.root

let third chord =
  match chord.notes with
  | _ :: third :: _ -> Some third
  | _ -> None

let fifth chord =
  match chord.notes with
  | _ :: _ :: fifth :: _ -> fifth
  | _ -> chord.root (* Fallback shouldn't happen *)

let seventh_note chord =
  match chord.notes with
  | _ :: _ :: _ :: seventh :: _ -> Some seventh
  | _ -> None

let extensions chord =
  match chord.notes with
  | _ :: _ :: _ :: _ :: exts -> exts
  | _ -> []

(* {1 Inversions} *)

type inversion =
  | Root
  | First
  | Second
  | Third

let invert chord inv =
  let note_list = chord.notes in
  match inv with
  | Root -> note_list
  | First ->
    (match note_list with
     | root :: third :: rest -> third :: (rest @ [root])
     | _ -> note_list)
  | Second ->
    (match note_list with
     | root :: third :: fifth :: rest -> fifth :: (rest @ [root; third])
     | _ -> note_list)
  | Third ->
    (match note_list with
     | root :: third :: fifth :: seventh :: rest -> seventh :: (rest @ [root; third; fifth])
     | _ -> note_list)

let bass_note chord inv =
  List.hd (invert chord inv)

let detect_inversion note_list =
  (* Simplified: check if bass note is root, third, fifth, or seventh *)
  match from_notes note_list with
  | None -> None
  | Some chord ->
    let bass = List.hd note_list in
    if Note.enharmonic_eq bass chord.root then
      Some Root
    else (match third chord with
      | Some t when Note.enharmonic_eq bass t -> Some First
      | _ ->
        let f = fifth chord in
        if Note.enharmonic_eq bass f then
          Some Second
        else (match seventh_note chord with
          | Some s when Note.enharmonic_eq bass s -> Some Third
          | _ -> None))

(* {1 Chord analysis} *)

let contains chord note =
  List.exists (fun n -> Note.enharmonic_eq n note) chord.notes

let is_diatonic scale chord =
  List.for_all (fun note -> Scale.contains scale note) chord.notes

let degree_in_scale scale chord =
  Scale.degree_of_note scale chord.root

type chord_function =
  | Tonic
  | Supertonic
  | Mediant
  | Subdominant
  | Dominant
  | Submediant
  | LeadingTone

let function_in_key scale chord =
  match degree_in_scale scale chord with
  | Some 0 -> Some Tonic
  | Some 1 -> Some Supertonic
  | Some 2 -> Some Mediant
  | Some 3 -> Some Subdominant
  | Some 4 -> Some Dominant
  | Some 5 -> Some Submediant
  | Some 6 -> Some LeadingTone
  | _ -> None

(* {1 Chord quality analysis} *)

let quality_name chord =
  match chord.chord_type with
  | Triad Major -> "major"
  | Triad Minor -> "minor"
  | Triad Diminished -> "diminished"
  | Triad Augmented -> "augmented"
  | Triad Sus2 -> "sus2"
  | Triad Sus4 -> "sus4"
  | Seventh Major7 -> "major7"
  | Seventh Minor7 -> "minor7"
  | Seventh Dominant7 -> "dominant7"
  | Seventh HalfDiminished7 -> "half-diminished7"
  | Seventh Diminished7 -> "diminished7"
  | Seventh MinorMajor7 -> "minor-major7"
  | Seventh Augmented7 -> "augmented7"
  | Extended (Ninth _) -> "ninth"
  | Extended (Eleventh _) -> "eleventh"
  | Extended (Thirteenth _) -> "thirteenth"
  | Extended Add9 -> "add9"
  | Extended Add11 -> "add11"
  | Extended Sixth -> "6"
  | Extended MinorSixth -> "m6"

let interval_structure chord =
  let root_pitch = Note.to_pitch chord.root in
  List.filter_map (fun note ->
    let note_pitch = Note.to_pitch note in
    let semitones = Pitch.diff root_pitch note_pitch in
    if semitones = 0 then None else Interval.of_semitones semitones
  ) chord.notes

let tension_level chord =
  match chord.chord_type with
  | Triad Major -> 0
  | Triad Minor -> 1
  | Triad Sus2 | Triad Sus4 -> 2
  | Triad Diminished -> 5
  | Triad Augmented -> 6
  | Seventh Major7 -> 2
  | Seventh Minor7 -> 3
  | Seventh Dominant7 -> 7
  | Seventh HalfDiminished7 -> 6
  | Seventh Diminished7 -> 8
  | Seventh MinorMajor7 -> 4
  | Seventh Augmented7 -> 9
  | Extended _ -> 5

(* {1 Voice leading} *)

let voice_leading_distance voicing1 voicing2 =
  (* Simplified: sum of absolute semitone differences *)
  let pitches1 = List.map Note.to_pitch voicing1 in
  let pitches2 = List.map Note.to_pitch voicing2 in
  List.fold_left2 (fun sum p1 p2 ->
    sum + abs (Pitch.diff p1 p2)
  ) 0 pitches1 pitches2

let closest_voicing chord _from_notes target_inv =
  (* Simplified: just return the inversion *)
  invert chord target_inv

(* {1 Chord progressions} *)

let resolves_to chord1 chord2 =
  (* Simplified resolution rules *)
  let root1 = Note.to_pitch chord1.root in
  let root2 = Note.to_pitch chord2.root in
  let interval = Pitch.diff root1 root2 in
  (* V -> I resolution (up a fourth / down a fifth) *)
  interval = 5 || interval = 7

let common_tones chord1 chord2 =
  List.filter (fun n1 ->
    List.exists (fun n2 -> Note.enharmonic_eq n1 n2) chord2.notes
  ) chord1.notes

(* {1 Alterations} *)

let add_extension chord _interval =
  (* Simplified: would need more sophisticated logic *)
  Some chord

let alter_fifth chord _alteration =
  Some chord

let omit_fifth chord =
  let filtered_notes = List.filteri (fun i _ -> i <> 2) chord.notes in
  Some { chord with notes = filtered_notes }

(* {1 String conversion} *)

let to_string ?(unicode = false) chord =
  let root_str = Note.to_string ~unicode chord.root in
  let quality_str =
    match chord.chord_type with
    | Triad Major -> ""
    | Triad Minor -> "m"
    | Triad Diminished -> if unicode then "°" else "dim"
    | Triad Augmented -> if unicode then "+" else "aug"
    | Triad Sus2 -> "sus2"
    | Triad Sus4 -> "sus4"
    | Seventh Major7 -> if unicode then "△7" else "maj7"
    | Seventh Minor7 -> "m7"
    | Seventh Dominant7 -> "7"
    | Seventh HalfDiminished7 -> if unicode then "ø7" else "m7♭5"
    | Seventh Diminished7 -> if unicode then "°7" else "dim7"
    | Seventh MinorMajor7 -> "m△7"
    | Seventh Augmented7 -> "+7"
    | Extended (Ninth _) -> "9"
    | Extended (Eleventh _) -> "11"
    | Extended (Thirteenth _) -> "13"
    | Extended Add9 -> "add9"
    | Extended Add11 -> "add11"
    | Extended Sixth -> "6"
    | Extended MinorSixth -> "m6"
  in
  root_str ^ quality_str

let to_string_with_notes chord =
  let chord_name = to_string chord in
  let notes_str = String.concat " " (List.map Note.to_string chord.notes) in
  Printf.sprintf "%s: %s" chord_name notes_str

let of_string _s =
  (* TODO: implement chord symbol parsing *)
  Error "Chord parsing not yet implemented"

(* {1 Comparison} *)

let equal c1 c2 =
  Note.equal c1.root c2.root &&
  c1.chord_type = c2.chord_type

let enharmonic_equal c1 c2 =
  let pitches1 = List.map Note.to_pitch c1.notes |> List.sort Pitch.compare in
  let pitches2 = List.map Note.to_pitch c2.notes |> List.sort Pitch.compare in
  pitches1 = pitches2
