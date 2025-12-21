open Music_theory

let test_semitones () =
  Alcotest.(check int) "Unison = 0" 0
    (Core.Interval.to_semitones Core.Interval.Unison);
  Alcotest.(check int) "MinorThird = 3" 3
    (Core.Interval.to_semitones Core.Interval.MinorThird);
  Alcotest.(check int) "MajorThird = 4" 4
    (Core.Interval.to_semitones Core.Interval.MajorThird);
  Alcotest.(check int) "PerfectFifth = 7" 7
    (Core.Interval.to_semitones Core.Interval.PerfectFifth);
  Alcotest.(check int) "Octave = 12" 12
    (Core.Interval.to_semitones Core.Interval.Octave)

let test_of_semitones () =
  Alcotest.(check bool) "0 = Unison" true
    (Core.Interval.of_semitones 0 = Some Core.Interval.Unison);
  Alcotest.(check bool) "7 = PerfectFifth" true
    (Core.Interval.of_semitones 7 = Some Core.Interval.PerfectFifth);
  Alcotest.(check bool) "13 = None" true
    (Core.Interval.of_semitones 13 = None)

let test_inversion () =
  Alcotest.(check bool) "Unison inverts to Octave" true
    (Core.Interval.invert Core.Interval.Unison = Core.Interval.Octave);
  Alcotest.(check bool) "MajorThird inverts to MinorSixth" true
    (Core.Interval.invert Core.Interval.MajorThird = Core.Interval.MinorSixth);
  Alcotest.(check bool) "PerfectFifth inverts to PerfectFourth" true
    (Core.Interval.invert Core.Interval.PerfectFifth = Core.Interval.PerfectFourth);
  Alcotest.(check bool) "Tritone inverts to Tritone" true
    (Core.Interval.invert Core.Interval.Tritone = Core.Interval.Tritone)

let test_qualities () =
  Alcotest.(check bool) "PerfectFifth is perfect" true
    (Core.Interval.is_perfect Core.Interval.PerfectFifth);
  Alcotest.(check bool) "MajorThird is not perfect" false
    (Core.Interval.is_perfect Core.Interval.MajorThird);

  Alcotest.(check bool) "MajorThird is major" true
    (Core.Interval.is_major Core.Interval.MajorThird);
  Alcotest.(check bool) "MinorThird is minor" true
    (Core.Interval.is_minor Core.Interval.MinorThird);
  Alcotest.(check bool) "PerfectFifth is not major" false
    (Core.Interval.is_major Core.Interval.PerfectFifth)

let test_letter_distance () =
  Alcotest.(check int) "Unison letter distance = 0" 0
    (Core.Interval.letter_distance Core.Interval.Unison);
  Alcotest.(check int) "MajorThird letter distance = 2" 2
    (Core.Interval.letter_distance Core.Interval.MajorThird);
  Alcotest.(check int) "PerfectFifth letter distance = 4" 4
    (Core.Interval.letter_distance Core.Interval.PerfectFifth);
  Alcotest.(check int) "Octave letter distance = 7" 7
    (Core.Interval.letter_distance Core.Interval.Octave)

let test_to_string () =
  Alcotest.(check string) "Unison string" "P1"
    (Core.Interval.to_string Core.Interval.Unison);
  Alcotest.(check string) "MinorThird string" "m3"
    (Core.Interval.to_string Core.Interval.MinorThird);
  Alcotest.(check string) "MajorThird string" "M3"
    (Core.Interval.to_string Core.Interval.MajorThird);
  Alcotest.(check string) "PerfectFifth string" "P5"
    (Core.Interval.to_string Core.Interval.PerfectFifth)

let () =
  Alcotest.run "Interval"
    [ ("conversion", [
        Alcotest.test_case "to_semitones" `Quick test_semitones;
        Alcotest.test_case "of_semitones" `Quick test_of_semitones;
      ])
    ; ("operations", [
        Alcotest.test_case "inversion" `Quick test_inversion;
      ])
    ; ("qualities", [
        Alcotest.test_case "is_perfect/major/minor" `Quick test_qualities;
        Alcotest.test_case "letter_distance" `Quick test_letter_distance;
      ])
    ; ("string conversion", [
        Alcotest.test_case "to_string" `Quick test_to_string;
      ])
    ]
