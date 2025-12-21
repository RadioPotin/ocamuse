(* {1 Circle position} *)

type position = int

(* Circle of fifths maps pitch classes to positions
   Moving clockwise = up a fifth = +7 semitones
   Position ordering: C=0, G=1, D=2, A=3, E=4, B=5, F#=6, Db=7, Ab=8, Eb=9, Bb=10, F=11 *)

(* Pitch class to circle position lookup *)
let pitch_to_position_table =
  [| 0; 7; 2; 9; 4; 11; 6; 1; 8; 3; 10; 5 |]
  (* Index = pitch class (C=0, C#=1, ..., B=11) *)
  (* Value = circle position *)

(* Circle position to pitch class lookup *)
let position_to_pitch_table =
  [| 0; 7; 2; 9; 4; 11; 6; 1; 8; 3; 10; 5 |]
  (* Index = circle position *)
  (* Value = pitch class *)

let of_pitch p =
  let pc = Pitch.to_int p in
  pitch_to_position_table.(pc)

let to_pitch pos =
  let pos = pos mod 12 in
  let pos = if pos < 0 then pos + 12 else pos in
  Pitch.of_int position_to_pitch_table.(pos)

let of_note n =
  of_pitch (Note.to_pitch n)

let to_note pos ~prefer =
  Note.of_pitch (to_pitch pos) ~prefer

(* {1 Navigation} *)

let move_clockwise pos steps =
  (pos + steps) mod 12

let move_counter_clockwise pos steps =
  let new_pos = pos - steps in
  if new_pos < 0 then new_pos + 12 else new_pos

let fifth_above pos =
  move_clockwise pos 1

let fifth_below pos =
  move_counter_clockwise pos 1

let fourth_above pos =
  move_counter_clockwise pos 1

let fourth_below pos =
  move_clockwise pos 1

(* {1 Key signatures} *)

type accidental_type = Sharp | Flat | Natural

let accidental_type pos =
  (* C (0) has no accidentals
     Clockwise from C (1-6): sharps
     Counter-clockwise from C (11-7): flats
     Position 6 (F#/Gb) is ambiguous, we'll call it sharps *)
  match pos with
  | 0 -> Natural
  | 1 | 2 | 3 | 4 | 5 | 6 -> Sharp
  | 7 | 8 | 9 | 10 | 11 -> Flat
  | _ -> Natural (* Shouldn't happen *)

let sharps_count pos =
  match pos with
  | 0 -> 0           (* C major: no sharps *)
  | 1 -> 1           (* G major: 1 sharp *)
  | 2 -> 2           (* D major: 2 sharps *)
  | 3 -> 3           (* A major: 3 sharps *)
  | 4 -> 4           (* E major: 4 sharps *)
  | 5 -> 5           (* B major: 5 sharps *)
  | 6 -> 6           (* F# major: 6 sharps *)
  | _ -> 0           (* Flat keys *)

let flats_count pos =
  match pos with
  | 11 -> 1          (* F major: 1 flat *)
  | 10 -> 2          (* Bb major: 2 flats *)
  | 9 -> 3           (* Eb major: 3 flats *)
  | 8 -> 4           (* Ab major: 4 flats *)
  | 7 -> 5           (* Db major: 5 flats *)
  | 6 -> 6           (* Gb major: 6 flats - enharmonic *)
  | _ -> 0           (* Sharp or natural keys *)

(* Order of sharps: F C G D A E B *)
let sharp_order = [Note.F; Note.G; Note.D; Note.A; Note.E; Note.B; Note.F]

(* Order of flats: B E A D G C F *)
let flat_order = [Note.B; Note.E; Note.A; Note.D; Note.G; Note.C; Note.F]

let key_signature pos =
  let n_sharps = sharps_count pos in
  let n_flats = flats_count pos in
  if n_sharps > 0 then
    List.filteri (fun i _ -> i < n_sharps) sharp_order
  else if n_flats > 0 then
    List.filteri (fun i _ -> i < n_flats) flat_order
  else
    []

(* {1 Scales at positions} *)

let major_scale pos =
  let root = to_note pos ~prefer:`Sharp in
  Scale.make root Mode.Ionian

let minor_scale pos =
  let root = to_note pos ~prefer:`Flat in
  Scale.make root Mode.Aeolian

let relative_minor_position pos =
  (* Relative minor is 3 semitones down = 9 semitones up
     On circle, that's -3 fifths = move counter-clockwise 3 steps *)
  move_counter_clockwise pos 3

let relative_major_position pos =
  (* Relative major is 3 semitones up
     On circle, that's +3 fifths = move clockwise 3 steps *)
  move_clockwise pos 3

(* {1 Mode brightness and modal interchange} *)

let mode_at_brightness tonic_pos mode =
  (* Each mode has a brightness relative to Ionian
     Brightness = position on circle relative to tonic
     E.g., C Lydian (+1 bright) aligns with G on circle (1 sharp)
     E.g., C Dorian (-2 bright) aligns with Bb on circle (2 flats) *)
  let brightness = Mode.brightness mode in
  move_clockwise tonic_pos brightness

let brightness_difference m1 m2 =
  Mode.brightness m2 - Mode.brightness m1

let parallel_mode pos new_mode =
  (* Parallel mode: same tonic, different mode
     Position adjusts based on mode brightness *)
  let current_pitch = to_pitch pos in
  let tonic_note = Note.of_pitch current_pitch ~prefer:`Sharp in
  let tonic_pos = of_note tonic_note in
  mode_at_brightness tonic_pos new_mode

(* {1 Modulation and key relationships} *)

type relationship =
  | Same
  | Relative
  | Parallel
  | Dominant
  | Subdominant
  | Closely_related
  | Distantly_related

let relationship pos1 pos2 =
  if pos1 = pos2 then
    Same
  else
    let distance = abs (pos1 - pos2) in
    let distance = min distance (12 - distance) in (* Shortest distance *)
    match distance with
    | 1 ->
      if pos2 = move_clockwise pos1 1 then Dominant
      else Subdominant
    | 3 -> Relative
    | 0 -> Parallel (* Same pitch, could be parallel major/minor *)
    | _ when distance <= 2 -> Closely_related
    | _ -> Distantly_related

let closely_related_keys pos =
  (* Closely related keys: within 1-2 steps on circle
     Plus relative minor/major *)
  let rel_minor = relative_minor_position pos in
  let rel_major = relative_major_position pos in
  [
    move_clockwise pos 1;      (* Dominant *)
    move_counter_clockwise pos 1; (* Subdominant *)
    move_clockwise pos 2;
    move_counter_clockwise pos 2;
    rel_minor;
    rel_major;
  ] |> List.sort_uniq compare

let common_tone_count pos1 pos2 =
  let scale1 = major_scale pos1 in
  let scale2 = major_scale pos2 in
  let degrees1 = Scale.degrees scale1 |> List.map Note.to_pitch in
  let degrees2 = Scale.degrees scale2 |> List.map Note.to_pitch in
  List.fold_left (fun count pc ->
    if List.mem pc degrees2 then count + 1 else count
  ) 0 degrees1

(* {1 Harmonic functions based on circle} *)

let dominant_position pos =
  fifth_above pos

let subdominant_position pos =
  fifth_below pos

let secondary_dominant _tonic target_degree =
  (* Secondary dominant: the dominant of a scale degree
     E.g., in C major, V/V is the dominant of G, which is D
     Circle: find position of target, then move up a fifth *)
  let target_pos = of_note target_degree in
  Some (dominant_position target_pos)

(* {1 All positions} *)

let all_positions =
  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11]

let all_positions_clockwise start =
  List.init 12 (fun i -> (start + i) mod 12)

(* {1 String conversion} *)

let to_string pos =
  let prefer = if accidental_type pos = Flat then `Flat else `Sharp in
  Note.to_string (to_note pos ~prefer)

let to_string_with_signature pos =
  let key_name = to_string pos in
  let n_sharps = sharps_count pos in
  let n_flats = flats_count pos in
  if n_sharps > 0 then
    Printf.sprintf "%s major (%d♯)" key_name n_sharps
  else if n_flats > 0 then
    Printf.sprintf "%s major (%d♭)" key_name n_flats
  else
    Printf.sprintf "%s major" key_name

(* {1 Visualization helpers} *)

let position_name pos =
  to_string pos

let enharmonic_name pos =
  match pos with
  | 6 -> Some "Gb"  (* F# / Gb *)
  | 1 -> Some "Db"  (* C# / Db - in some contexts *)
  | 8 -> Some "G#"  (* Ab / G# *)
  | _ -> None
