open Music_theory

let test_mode_intervals () =
  let ionian_semitones = Scales.Mode.semitones Scales.Mode.Ionian in
  Alcotest.(check (list int)) "Ionian pattern"
    [2; 2; 1; 2; 2; 2; 1] ionian_semitones;

  let dorian_semitones = Scales.Mode.semitones Scales.Mode.Dorian in
  Alcotest.(check (list int)) "Dorian pattern"
    [2; 1; 2; 2; 2; 1; 2] dorian_semitones

let test_mode_brightness () =
  Alcotest.(check int) "Lydian brightness" 3
    (Scales.Mode.brightness Scales.Mode.Lydian);
  Alcotest.(check int) "Ionian brightness" 2
    (Scales.Mode.brightness Scales.Mode.Ionian);
  Alcotest.(check int) "Aeolian brightness" (-1)
    (Scales.Mode.brightness Scales.Mode.Aeolian);
  Alcotest.(check int) "Locrian brightness" (-3)
    (Scales.Mode.brightness Scales.Mode.Locrian)

let test_mode_rotation () =
  let dorian = Scales.Mode.rotate Scales.Mode.Ionian 1 in
  Alcotest.(check bool) "Ionian rotated by 1 = Dorian" true
    (Scales.Mode.equal dorian Scales.Mode.Dorian);

  let aeolian = Scales.Mode.rotate Scales.Mode.Ionian 5 in
  Alcotest.(check bool) "Ionian rotated by 5 = Aeolian" true
    (Scales.Mode.equal aeolian Scales.Mode.Aeolian)

let test_mode_classification () =
  Alcotest.(check bool) "Ionian is major" true
    (Scales.Mode.is_major Scales.Mode.Ionian);
  Alcotest.(check bool) "Lydian is major" true
    (Scales.Mode.is_major Scales.Mode.Lydian);
  Alcotest.(check bool) "Aeolian is minor" true
    (Scales.Mode.is_minor Scales.Mode.Aeolian);
  Alcotest.(check bool) "Dorian is minor" true
    (Scales.Mode.is_minor Scales.Mode.Dorian)

let test_scale_construction () =
  let c_major = Scales.Scale.major Core.Note.c in
  let degrees = Scales.Scale.degrees c_major in
  Alcotest.(check int) "C major has 7 degrees" 7 (List.length degrees);

  let tonic = Scales.Scale.tonic c_major in
  Alcotest.(check bool) "C major tonic is C" true
    (Core.Note.equal tonic Core.Note.c);

  let dominant = Scales.Scale.dominant c_major in
  Alcotest.(check bool) "C major dominant is G" true
    (Core.Note.equal dominant Core.Note.g)

let test_scale_degrees () =
  let c_major = Scales.Scale.major Core.Note.c in

  (match Scales.Scale.degree c_major 0 with
  | Some note -> Alcotest.(check bool) "Degree 0 is tonic" true
      (Core.Note.equal note Core.Note.c)
  | None -> Alcotest.failf "Degree 0 should exist");

  (match Scales.Scale.degree c_major 4 with
  | Some note -> Alcotest.(check bool) "Degree 4 is dominant" true
      (Core.Note.equal note Core.Note.g)
  | None -> Alcotest.failf "Degree 4 should exist");

  (match Scales.Scale.degree c_major 10 with
  | None -> ()
  | Some _ -> Alcotest.failf "Degree 10 should not exist")

let test_scale_contains () =
  let c_major = Scales.Scale.major Core.Note.c in

  Alcotest.(check bool) "C major contains C" true
    (Scales.Scale.contains c_major Core.Note.c);
  Alcotest.(check bool) "C major contains G" true
    (Scales.Scale.contains c_major Core.Note.g);
  Alcotest.(check bool) "C major contains E" true
    (Scales.Scale.contains c_major Core.Note.e);

  (* C# is not in C major *)
  Alcotest.(check bool) "C major does not contain C#" false
    (Scales.Scale.contains c_major Core.Note.c_sharp)

let test_diatonic_triads () =
  let c_major = Scales.Scale.major Core.Note.c in
  let triads = Scales.Scale.diatonic_triads c_major in

  Alcotest.(check int) "C major has 7 diatonic triads" 7 (List.length triads);

  (* I chord (C major) *)
  (match List.nth triads 0 with
   | (root, quality) ->
     Alcotest.(check bool) "I chord root is C" true (Core.Note.equal root Core.Note.c);
     Alcotest.(check bool) "I chord is Major" true (quality = Scales.Scale.Major));

  (* V chord (G major) *)
  (match List.nth triads 4 with
   | (root, quality) ->
     Alcotest.(check bool) "V chord root is G" true (Core.Note.equal root Core.Note.g);
     Alcotest.(check bool) "V chord is Major" true (quality = Scales.Scale.Major));

  (* vii chord (B diminished) *)
  (match List.nth triads 6 with
   | (_root, quality) ->
     Alcotest.(check bool) "vii chord is Diminished" true (quality = Scales.Scale.Diminished))

let test_relative_keys () =
  let c_major = Scales.Scale.major Core.Note.c in
  let a_minor = Scales.Scale.relative_minor c_major in

  let a_minor_root = Scales.Scale.tonic a_minor in
  Alcotest.(check bool) "Relative minor of C major is A minor" true
    (Core.Note.equal a_minor_root Core.Note.a);

  let c_major_again = Scales.Scale.relative_major a_minor in
  let c_major_root = Scales.Scale.tonic c_major_again in
  Alcotest.(check bool) "Relative major of A minor is C major" true
    (Core.Note.equal c_major_root Core.Note.c)

let test_parallel_modes () =
  let c_major = Scales.Scale.major Core.Note.c in
  let c_dorian = Scales.Scale.parallel_mode c_major Scales.Mode.Dorian in

  let dorian_root = Scales.Scale.tonic c_dorian in
  Alcotest.(check bool) "Parallel mode keeps same root" true
    (Core.Note.equal dorian_root Core.Note.c)

let () =
  Alcotest.run "Scales"
    [ ("Mode intervals", [
        Alcotest.test_case "intervallic patterns" `Quick test_mode_intervals;
      ])
    ; ("Mode brightness", [
        Alcotest.test_case "brightness ordering" `Quick test_mode_brightness;
        Alcotest.test_case "mode rotation" `Quick test_mode_rotation;
      ])
    ; ("Mode classification", [
        Alcotest.test_case "major/minor" `Quick test_mode_classification;
      ])
    ; ("Scale construction", [
        Alcotest.test_case "major scale" `Quick test_scale_construction;
        Alcotest.test_case "scale degrees" `Quick test_scale_degrees;
      ])
    ; ("Scale analysis", [
        Alcotest.test_case "contains note" `Quick test_scale_contains;
      ])
    ; ("Diatonic harmony", [
        Alcotest.test_case "diatonic triads" `Quick test_diatonic_triads;
      ])
    ; ("Key relationships", [
        Alcotest.test_case "relative keys" `Quick test_relative_keys;
        Alcotest.test_case "parallel modes" `Quick test_parallel_modes;
      ])
    ]
