open Music_theory

let test_triad_construction () =
  let c_maj = Harmony.Chord.major Core.Note.c in
  let notes = Harmony.Chord.notes c_maj in
  Alcotest.(check int) "C major has 3 notes" 3 (List.length notes);

  let root = Harmony.Chord.root c_maj in
  Alcotest.(check bool) "C major root is C" true
    (Core.Note.equal root Core.Note.c);

  let d_min = Harmony.Chord.minor Core.Note.d in
  let d_notes = Harmony.Chord.notes d_min in
  Alcotest.(check int) "D minor has 3 notes" 3 (List.length d_notes)

let test_seventh_chords () =
  let g7 = Harmony.Chord.dominant7 Core.Note.g in
  let notes = Harmony.Chord.notes g7 in
  Alcotest.(check int) "G7 has 4 notes" 4 (List.length notes);

  let cmaj7 = Harmony.Chord.major7 Core.Note.c in
  let cmaj7_notes = Harmony.Chord.notes cmaj7 in
  Alcotest.(check int) "Cmaj7 has 4 notes" 4 (List.length cmaj7_notes)

let test_chord_inversions () =
  let c_maj = Harmony.Chord.major Core.Note.c in

  (* Root position *)
  let root_pos = Harmony.Chord.invert c_maj Harmony.Chord.Root in
  Alcotest.(check int) "Root position has 3 notes" 3 (List.length root_pos);

  (* First inversion - third in bass *)
  let first_inv = Harmony.Chord.invert c_maj Harmony.Chord.First in
  Alcotest.(check int) "First inversion has 3 notes" 3 (List.length first_inv);
  let bass = List.hd first_inv in
  (match Harmony.Chord.third c_maj with
   | Some third -> Alcotest.(check bool) "First inversion has third in bass" true
       (Core.Note.enharmonic_eq bass third)
   | None -> Alcotest.failf "C major should have a third");

  (* Second inversion - fifth in bass *)
  let second_inv = Harmony.Chord.invert c_maj Harmony.Chord.Second in
  Alcotest.(check int) "Second inversion has 3 notes" 3 (List.length second_inv);
  let bass2 = List.hd second_inv in
  let fifth = Harmony.Chord.fifth c_maj in
  Alcotest.(check bool) "Second inversion has fifth in bass" true
    (Core.Note.enharmonic_eq bass2 fifth)

let test_chord_contains () =
  let c_maj = Harmony.Chord.major Core.Note.c in

  Alcotest.(check bool) "C major contains C" true
    (Harmony.Chord.contains c_maj Core.Note.c);
  Alcotest.(check bool) "C major contains E" true
    (Harmony.Chord.contains c_maj Core.Note.e);
  Alcotest.(check bool) "C major contains G" true
    (Harmony.Chord.contains c_maj Core.Note.g);

  (* F is not in C major triad *)
  Alcotest.(check bool) "C major does not contain F" false
    (Harmony.Chord.contains c_maj Core.Note.f)

let test_chord_quality () =
  let c_maj = Harmony.Chord.major Core.Note.c in
  let quality = Harmony.Chord.quality_name c_maj in
  Alcotest.(check string) "C major quality" "major" quality;

  let d_min = Harmony.Chord.minor Core.Note.d in
  let min_quality = Harmony.Chord.quality_name d_min in
  Alcotest.(check string) "D minor quality" "minor" min_quality;

  let g7 = Harmony.Chord.dominant7 Core.Note.g in
  let dom7_quality = Harmony.Chord.quality_name g7 in
  Alcotest.(check string) "G7 quality" "dominant7" dom7_quality

let test_chord_to_string () =
  let c_maj = Harmony.Chord.major Core.Note.c in
  let chord_str = Harmony.Chord.to_string c_maj in
  Alcotest.(check string) "C major string" "C" chord_str;

  let d_min = Harmony.Chord.minor Core.Note.d in
  let min_str = Harmony.Chord.to_string d_min in
  Alcotest.(check string) "D minor string" "Dm" min_str;

  let g7 = Harmony.Chord.dominant7 Core.Note.g in
  let dom7_str = Harmony.Chord.to_string g7 in
  Alcotest.(check string) "G7 string" "G7" dom7_str

let test_diatonic_chords () =
  let c_major_scale = Scales.Scale.major Core.Note.c in

  (* Test that C major chord is diatonic to C major scale *)
  let c_maj = Harmony.Chord.major Core.Note.c in
  Alcotest.(check bool) "C major is diatonic to C major scale" true
    (Harmony.Chord.is_diatonic c_major_scale c_maj);

  (* Test that G major chord is diatonic to C major scale *)
  let g_maj = Harmony.Chord.major Core.Note.g in
  Alcotest.(check bool) "G major is diatonic to C major scale" true
    (Harmony.Chord.is_diatonic c_major_scale g_maj);

  (* Test that F major chord is diatonic to C major scale *)
  let f_maj = Harmony.Chord.major Core.Note.f in
  Alcotest.(check bool) "F major is diatonic to C major scale" true
    (Harmony.Chord.is_diatonic c_major_scale f_maj)

let test_chord_function () =
  let c_major_scale = Scales.Scale.major Core.Note.c in

  (* I chord - C major *)
  let c_maj = Harmony.Chord.major Core.Note.c in
  (match Harmony.Chord.function_in_key c_major_scale c_maj with
   | Some Harmony.Chord.Tonic -> () (* Success *)
   | _ -> Alcotest.failf "C major should be Tonic in C major");

  (* V chord - G major *)
  let g_maj = Harmony.Chord.major Core.Note.g in
  (match Harmony.Chord.function_in_key c_major_scale g_maj with
   | Some Harmony.Chord.Dominant -> () (* Success *)
   | _ -> Alcotest.failf "G major should be Dominant in C major");

  (* IV chord - F major *)
  let f_maj = Harmony.Chord.major Core.Note.f in
  (match Harmony.Chord.function_in_key c_major_scale f_maj with
   | Some Harmony.Chord.Subdominant -> () (* Success *)
   | _ -> Alcotest.failf "F major should be Subdominant in C major")

let test_common_tones () =
  let c_maj = Harmony.Chord.major Core.Note.c in
  let g_maj = Harmony.Chord.major Core.Note.g in

  (* C major (C E G) and G major (G B D) share G *)
  let common = Harmony.Chord.common_tones c_maj g_maj in
  Alcotest.(check int) "C and G major share 1 common tone" 1 (List.length common);

  (* Check if G is the common tone *)
  let common_note = List.hd common in
  Alcotest.(check bool) "Common tone is G" true
    (Core.Note.enharmonic_eq common_note Core.Note.g)

let test_chord_resolution () =
  let g7 = Harmony.Chord.dominant7 Core.Note.g in
  let c_maj = Harmony.Chord.major Core.Note.c in

  (* G7 should resolve to C (V7 -> I) *)
  Alcotest.(check bool) "G7 resolves to C" true
    (Harmony.Chord.resolves_to g7 c_maj)

let test_tension_level () =
  let c_maj = Harmony.Chord.major Core.Note.c in
  let tension_maj = Harmony.Chord.tension_level c_maj in
  Alcotest.(check bool) "C major has low tension" true (tension_maj <= 2);

  let g7 = Harmony.Chord.dominant7 Core.Note.g in
  let tension_dom = Harmony.Chord.tension_level g7 in
  Alcotest.(check bool) "G7 has higher tension" true (tension_dom >= 5);

  let b_dim = Harmony.Chord.diminished Core.Note.b in
  let tension_dim = Harmony.Chord.tension_level b_dim in
  Alcotest.(check bool) "B diminished has medium-high tension" true
    (tension_dim >= 4)

let () =
  Alcotest.run "Harmony"
    [ ("Chord construction", [
        Alcotest.test_case "triads" `Quick test_triad_construction;
        Alcotest.test_case "seventh chords" `Quick test_seventh_chords;
      ])
    ; ("Inversions", [
        Alcotest.test_case "chord inversions" `Quick test_chord_inversions;
      ])
    ; ("Chord analysis", [
        Alcotest.test_case "contains note" `Quick test_chord_contains;
        Alcotest.test_case "chord quality" `Quick test_chord_quality;
      ])
    ; ("String conversion", [
        Alcotest.test_case "to_string" `Quick test_chord_to_string;
      ])
    ; ("Diatonic harmony", [
        Alcotest.test_case "diatonic chords" `Quick test_diatonic_chords;
        Alcotest.test_case "chord function" `Quick test_chord_function;
      ])
    ; ("Chord relationships", [
        Alcotest.test_case "common tones" `Quick test_common_tones;
        Alcotest.test_case "resolution" `Quick test_chord_resolution;
        Alcotest.test_case "tension level" `Quick test_tension_level;
      ])
    ]
