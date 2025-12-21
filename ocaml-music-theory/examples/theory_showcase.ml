open Music_theory

let () =
  Printf.printf "=== OCaml Music Theory - Comprehensive Showcase ===\n\n"

(* ========== Scales and Modes ========== *)
let () =
  Printf.printf "=== Scales and Modes ===\n";

  (* Construct C major scale *)
  let c_major = Scales.Scale.major Core.Note.c in
  Printf.printf "C Major scale: %s\n" (Scales.Scale.to_string_degrees c_major);

  (* Access scale degrees *)
  Printf.printf "Tonic: %s\n" (Core.Note.to_string (Scales.Scale.tonic c_major));
  Printf.printf "Dominant: %s\n" (Core.Note.to_string (Scales.Scale.dominant c_major));
  Printf.printf "Leading tone: %s\n" (Core.Note.to_string (Scales.Scale.leading_tone c_major));

  (* Modal variations *)
  let c_dorian = Scales.Scale.make Core.Note.c Scales.Mode.Dorian in
  Printf.printf "\nC Dorian: %s\n" (Scales.Scale.to_string_degrees c_dorian);

  let c_lydian = Scales.Scale.make Core.Note.c Scales.Mode.Lydian in
  Printf.printf "C Lydian: %s\n" (Scales.Scale.to_string_degrees c_lydian);

  (* Relative keys *)
  let a_minor = Scales.Scale.relative_minor c_major in
  Printf.printf "\nRelative minor of C major: %s\n" (Scales.Scale.to_string a_minor);
  Printf.printf "A minor scale: %s\n" (Scales.Scale.to_string_degrees a_minor);

  Printf.printf "\n"

(* ========== Automatic Harmony Generation ========== *)
let () =
  Printf.printf "=== Automatic Harmony Generation ===\n";

  let c_major = Scales.Scale.major Core.Note.c in

  (* Generate diatonic triads *)
  Printf.printf "Diatonic triads in C major:\n";
  let triads = Scales.Scale.diatonic_triads c_major in
  List.iteri (fun i (root, quality) ->
    let quality_str = match quality with
      | Scales.Scale.Major -> "major"
      | Scales.Scale.Minor -> "minor"
      | Scales.Scale.Diminished -> "dim"
      | Scales.Scale.Augmented -> "aug"
    in
    let roman = ["I"; "ii"; "iii"; "IV"; "V"; "vi"; "vii°"] in
    Printf.printf "  %s: %s %s\n"
      (List.nth roman i)
      (Core.Note.to_string root)
      quality_str
  ) triads;

  (* Generate diatonic seventh chords *)
  Printf.printf "\nDiatonic seventh chords in C major:\n";
  let sevenths = Scales.Scale.diatonic_sevenths c_major in
  List.iteri (fun i (root, quality) ->
    let quality_str = match quality with
      | Scales.Scale.Maj7 -> "maj7"
      | Scales.Scale.Min7 -> "m7"
      | Scales.Scale.Dom7 -> "7"
      | Scales.Scale.HalfDim7 -> "m7♭5"
      | Scales.Scale.Dim7 -> "dim7"
    in
    let roman = ["Imaj7"; "ii7"; "iii7"; "IVmaj7"; "V7"; "vi7"; "viiø7"] in
    Printf.printf "  %s: %s%s\n"
      (List.nth roman i)
      (Core.Note.to_string root)
      quality_str
  ) sevenths;

  Printf.printf "\n"

(* ========== Chords ========== *)
let () =
  Printf.printf "=== Chord Construction ===\n";

  (* Build various chord types *)
  let c_maj = Harmony.Chord.major Core.Note.c in
  Printf.printf "C major: %s\n" (Harmony.Chord.to_string_with_notes c_maj);

  let d_min7 = Harmony.Chord.minor7 Core.Note.d in
  Printf.printf "D minor 7: %s\n" (Harmony.Chord.to_string_with_notes d_min7);

  let g_dom7 = Harmony.Chord.dominant7 Core.Note.g in
  Printf.printf "G dominant 7: %s\n" (Harmony.Chord.to_string_with_notes g_dom7);

  let b_dim = Harmony.Chord.diminished Core.Note.b in
  Printf.printf "B diminished: %s\n" (Harmony.Chord.to_string_with_notes b_dim);

  (* Chord inversions *)
  Printf.printf "\nC major inversions:\n";
  let root_pos = Harmony.Chord.invert c_maj Harmony.Chord.Root in
  let first_inv = Harmony.Chord.invert c_maj Harmony.Chord.First in
  let second_inv = Harmony.Chord.invert c_maj Harmony.Chord.Second in
  Printf.printf "  Root position: %s\n"
    (String.concat " " (List.map Core.Note.to_string root_pos));
  Printf.printf "  First inversion: %s\n"
    (String.concat " " (List.map Core.Note.to_string first_inv));
  Printf.printf "  Second inversion: %s\n"
    (String.concat " " (List.map Core.Note.to_string second_inv));

  Printf.printf "\n"

(* ========== Circle of Fifths Navigation ========== *)
let () =
  Printf.printf "=== Circle of Fifths Navigation ===\n";

  (* Starting position *)
  let c_pos = Scales.Circle_of_fifths.of_note Core.Note.c in
  Printf.printf "C is at position %d on the circle\n" c_pos;

  (* Move clockwise (add sharps) *)
  let g_pos = Scales.Circle_of_fifths.fifth_above c_pos in
  let g_note = Scales.Circle_of_fifths.to_note g_pos ~prefer:`Sharp in
  Printf.printf "One fifth above C: %s (position %d)\n"
    (Core.Note.to_string g_note) g_pos;

  let d_pos = Scales.Circle_of_fifths.fifth_above g_pos in
  let d_note = Scales.Circle_of_fifths.to_note d_pos ~prefer:`Sharp in
  Printf.printf "One fifth above G: %s (position %d)\n"
    (Core.Note.to_string d_note) d_pos;

  (* Move counter-clockwise (add flats) *)
  let f_pos = Scales.Circle_of_fifths.fourth_above c_pos in
  let f_note = Scales.Circle_of_fifths.to_note f_pos ~prefer:`Flat in
  Printf.printf "One fourth above C (fifth below): %s (position %d)\n"
    (Core.Note.to_string f_note) f_pos;

  (* Key signatures *)
  Printf.printf "\nKey signatures:\n";
  Printf.printf "C major: %d sharps, %d flats\n"
    (Scales.Circle_of_fifths.sharps_count c_pos)
    (Scales.Circle_of_fifths.flats_count c_pos);
  Printf.printf "G major: %d sharps, %d flats\n"
    (Scales.Circle_of_fifths.sharps_count g_pos)
    (Scales.Circle_of_fifths.flats_count g_pos);
  Printf.printf "F major: %d sharps, %d flats\n"
    (Scales.Circle_of_fifths.sharps_count f_pos)
    (Scales.Circle_of_fifths.flats_count f_pos);

  (* Closely related keys *)
  Printf.printf "\nCloselyrelated keys to C major:\n";
  let related = Scales.Circle_of_fifths.closely_related_keys c_pos in
  List.iter (fun pos ->
    let note = Scales.Circle_of_fifths.to_note pos ~prefer:`Sharp in
    Printf.printf "  %s\n" (Core.Note.to_string note)
  ) related;

  Printf.printf "\n"

(* ========== Modulation and Key Relationships ========== *)
let () =
  Printf.printf "=== Modulation and Key Relationships ===\n";

  let c_major = Scales.Scale.major Core.Note.c in

  (* Modal interchange *)
  Printf.printf "Modal interchange from C major:\n";
  let c_minor = Scales.Scale.parallel_minor c_major in
  Printf.printf "  Parallel minor: %s - %s\n"
    (Scales.Scale.to_string c_minor)
    (Scales.Scale.to_string_degrees c_minor);

  let c_mixolydian = Scales.Scale.parallel_mode c_major Scales.Mode.Mixolydian in
  Printf.printf "  C Mixolydian: %s\n"
    (Scales.Scale.to_string_degrees c_mixolydian);

  (* Secondary dominants *)
  Printf.printf "\nSecondary dominants in C major:\n";
  let c_pos = Scales.Circle_of_fifths.of_note Core.Note.c in
  let g_note = Core.Note.g in
  (match Scales.Circle_of_fifths.secondary_dominant c_pos g_note with
   | Some dom_pos ->
     let dom_note = Scales.Circle_of_fifths.to_note dom_pos ~prefer:`Sharp in
     Printf.printf "  V/V (dominant of G): %s\n" (Core.Note.to_string dom_note)
   | None -> ());

  (* Common chord progressions *)
  Printf.printf "\nCommon progressions in C major:\n";
  Printf.printf "  I-IV-V-I: C - F - G - C\n";
  Printf.printf "  ii-V-I: Dm - G - C\n";
  Printf.printf "  I-vi-IV-V: C - Am - F - G\n";

  Printf.printf "\n"

(* ========== Mode Brightness ========== *)
let () =
  Printf.printf "=== Mode Brightness Ordering ===\n";

  Printf.printf "Modes from brightest to darkest:\n";
  let modes = Scales.Mode.all in
  List.iter (fun mode ->
    let brightness = Scales.Mode.brightness mode in
    let char_interval = Scales.Mode.characteristic_interval mode in
    Printf.printf "  %s (brightness %+d): characteristic interval = %s\n"
      (Scales.Mode.to_string mode)
      brightness
      (Core.Interval.to_string char_interval)
  ) modes;

  Printf.printf "\n"

(* ========== Analysis Examples ========== *)
let () =
  Printf.printf "=== Scale and Chord Analysis ===\n";

  let c_major = Scales.Scale.major Core.Note.c in

  (* Test if notes are in scale *)
  Printf.printf "Notes in C major:\n";
  let test_notes = [Core.Note.c; Core.Note.e; Core.Note.g; Core.Note.c_sharp] in
  List.iter (fun note ->
    let in_scale = Scales.Scale.contains c_major note in
    Printf.printf "  %s: %s\n"
      (Core.Note.to_string note)
      (if in_scale then "✓" else "✗")
  ) test_notes;

  (* Analyze chord function in key *)
  Printf.printf "\nChord functions in C major:\n";
  let c_chord = Harmony.Chord.major Core.Note.c in
  let g_chord = Harmony.Chord.major Core.Note.g in

  (match Harmony.Chord.function_in_key c_major c_chord with
   | Some Harmony.Chord.Tonic -> Printf.printf "  C major: Tonic (I)\n"
   | _ -> ());

  (match Harmony.Chord.function_in_key c_major g_chord with
   | Some Harmony.Chord.Dominant -> Printf.printf "  G major: Dominant (V)\n"
   | _ -> ());

  Printf.printf "\n=== Showcase Complete ===\n"
