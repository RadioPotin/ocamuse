type t =
  { root : Note.t
  ; mode : Mode.t
  ; degrees : Note.t list
  }

(* {1 Construction} *)

let make root mode =
  let all_intervals = Mode.intervals mode in
  (* Drop the last interval (back to octave) to get 7 notes *)
  let intervals = List.filteri (fun i _ -> i < 6) all_intervals in
  (* Build scale degrees by applying intervals step by step from root *)
  let rec build_degrees current_note remaining_intervals acc =
    match remaining_intervals with
    | [] -> List.rev acc
    | interval :: rest ->
      (* Get the next base note *)
      let { Note.base = current_base; alteration = _ } = current_note in
      let next_base = Note.next_base current_base in
      (* Calculate target pitch *)
      let current_pitch = Note.to_pitch current_note in
      let target_pitch = Pitch.add current_pitch (Interval.to_semitones interval) in
      (* Calculate what alteration is needed to reach target pitch *)
      let natural_pitch = Note.to_pitch (Note.natural next_base) in
      let alteration = Pitch.diff natural_pitch target_pitch in
      (* Handle wrap-around: if diff is large, it wrapped *)
      let alteration = if alteration > 6 then alteration - 12 else alteration in
      let alteration = if alteration < -6 then alteration + 12 else alteration in
      let next_note = Note.make next_base (-alteration) in
      build_degrees next_note rest (next_note :: acc)
  in
  let degrees = root :: build_degrees root intervals [] in
  { root; mode; degrees }

let major root = make root Mode.Ionian

let minor root = make root Mode.Aeolian

let harmonic_minor root =
  (* Harmonic minor = Aeolian with raised 7th *)
  let aeolian_scale = make root Mode.Aeolian in
  (* Modify the 7th degree *)
  let degrees =
    List.mapi (fun i note ->
      if i = 6 then
        (* Raise the 7th by a semitone *)
        let { Note.base = b; alteration = alt } = note in
        Note.make b (alt + 1)
      else
        note
    ) aeolian_scale.degrees
  in
  { root; mode = Mode.Aeolian; degrees }

let melodic_minor root =
  (* Melodic minor ascending = Aeolian with raised 6th and 7th *)
  let aeolian_scale = make root Mode.Aeolian in
  let degrees =
    List.mapi (fun i note ->
      if i = 5 || i = 6 then
        (* Raise 6th and 7th by a semitone *)
        let { Note.base = b; alteration = alt } = note in
        Note.make b (alt + 1)
      else
        note
    ) aeolian_scale.degrees
  in
  { root; mode = Mode.Aeolian; degrees }

(* {1 Scale degrees} *)

let degrees scale = scale.degrees

let degree scale n =
  if n >= 0 && n < 7 then
    Some (List.nth scale.degrees n)
  else
    None

let degree_exn scale n =
  match degree scale n with
  | Some d -> d
  | None -> invalid_arg (Printf.sprintf "Scale.degree_exn: %d not in [0, 6]" n)

let tonic scale = List.nth scale.degrees 0

let supertonic scale = List.nth scale.degrees 1

let mediant scale = List.nth scale.degrees 2

let subdominant scale = List.nth scale.degrees 3

let dominant scale = List.nth scale.degrees 4

let submediant scale = List.nth scale.degrees 5

let leading_tone scale = List.nth scale.degrees 6

(* {1 Scale analysis} *)

let contains scale note =
  List.exists (fun deg -> Note.enharmonic_eq deg note) scale.degrees

let degree_of_note scale note =
  let rec find_index i = function
    | [] -> None
    | deg :: rest ->
      if Note.enharmonic_eq deg note then
        Some i
      else
        find_index (i + 1) rest
  in
  find_index 0 scale.degrees

let interval_from_root scale note =
  if contains scale note then
    let root_pitch = Note.to_pitch scale.root in
    let note_pitch = Note.to_pitch note in
    let semitones = Pitch.diff root_pitch note_pitch in
    Interval.of_semitones semitones
  else
    None

(* {1 Diatonic harmony} *)

type triad_quality = Major | Minor | Diminished | Augmented

let diatonic_triads scale =
  (* Build triad on each degree: root, third (skip 1), fifth (skip 2) *)
  List.mapi (fun i root_note ->
    let third_idx = (i + 2) mod 7 in
    let fifth_idx = (i + 4) mod 7 in
    let third = List.nth scale.degrees third_idx in
    let fifth = List.nth scale.degrees fifth_idx in

    (* Determine quality by interval analysis *)
    let root_pitch = Note.to_pitch root_note in
    let third_pitch = Note.to_pitch third in
    let fifth_pitch = Note.to_pitch fifth in

    let third_interval = Pitch.diff root_pitch third_pitch in
    let fifth_interval = Pitch.diff root_pitch fifth_pitch in

    let quality =
      match (third_interval, fifth_interval) with
      | (3, 7) -> Minor          (* m3 + P5 *)
      | (4, 7) -> Major          (* M3 + P5 *)
      | (3, 6) -> Diminished     (* m3 + d5 *)
      | (4, 8) -> Augmented      (* M3 + A5 *)
      | _ -> Major               (* Fallback, shouldn't happen in diatonic *)
    in
    (root_note, quality)
  ) scale.degrees

type seventh_quality =
  | Maj7    (* Major triad + major 7th: M3, P5, M7 *)
  | Min7    (* Minor triad + minor 7th: m3, P5, m7 *)
  | Dom7    (* Major triad + minor 7th: M3, P5, m7 *)
  | HalfDim7 (* Diminished triad + minor 7th: m3, d5, m7 *)
  | Dim7    (* Diminished triad + diminished 7th: m3, d5, d7 *)

let diatonic_sevenths scale =
  List.mapi (fun i root_note ->
    let third_idx = (i + 2) mod 7 in
    let fifth_idx = (i + 4) mod 7 in
    let seventh_idx = (i + 6) mod 7 in
    let third = List.nth scale.degrees third_idx in
    let fifth = List.nth scale.degrees fifth_idx in
    let seventh = List.nth scale.degrees seventh_idx in

    let root_pitch = Note.to_pitch root_note in
    let third_pitch = Note.to_pitch third in
    let fifth_pitch = Note.to_pitch fifth in
    let seventh_pitch = Note.to_pitch seventh in

    let third_interval = Pitch.diff root_pitch third_pitch in
    let fifth_interval = Pitch.diff root_pitch fifth_pitch in
    let seventh_interval = Pitch.diff root_pitch seventh_pitch in

    let quality =
      match (third_interval, fifth_interval, seventh_interval) with
      | (4, 7, 11) -> Maj7       (* M3, P5, M7 - I, IV in major *)
      | (3, 7, 10) -> Min7       (* m3, P5, m7 - ii, iii, vi in major *)
      | (4, 7, 10) -> Dom7       (* M3, P5, m7 - V in major *)
      | (3, 6, 10) -> HalfDim7   (* m3, d5, m7 - vii in major *)
      | (3, 6, 9) -> Dim7        (* m3, d5, d7 - rare, diminished scale *)
      | _ -> Maj7                (* Fallback *)
    in
    (root_note, quality)
  ) scale.degrees

(* {1 Modal interchange} *)

let parallel_mode scale new_mode =
  make scale.root new_mode

(* {1 Related keys} *)

let relative_minor scale =
  (* 6th degree of major scale becomes root of relative minor *)
  match Mode.is_major scale.mode with
  | true ->
    let new_root = List.nth scale.degrees 5 in (* 6th degree, 0-indexed *)
    make new_root Mode.Aeolian
  | false ->
    (* If already minor, return as-is or could raise error *)
    scale

let relative_major scale =
  (* 3rd degree of minor scale becomes root of relative major *)
  match Mode.is_minor scale.mode with
  | true ->
    let new_root = List.nth scale.degrees 2 in (* 3rd degree *)
    make new_root Mode.Ionian
  | false ->
    scale

let parallel_minor scale =
  make scale.root Mode.Aeolian

let parallel_major scale =
  make scale.root Mode.Ionian

(* {1 String conversion} *)

let to_string scale =
  Printf.sprintf "%s %s"
    (Note.to_string scale.root)
    (Mode.to_string scale.mode)

let to_string_degrees scale =
  String.concat " " (List.map Note.to_string scale.degrees)

(* {1 Comparison} *)

let equal s1 s2 =
  Note.equal s1.root s2.root && Mode.equal s1.mode s2.mode

let enharmonic_equal s1 s2 =
  (* Two scales are enharmonically equal if they contain the same pitch classes *)
  let pitches1 = List.map Note.to_pitch s1.degrees |> List.sort Pitch.compare in
  let pitches2 = List.map Note.to_pitch s2.degrees |> List.sort Pitch.compare in
  pitches1 = pitches2
