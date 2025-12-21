open Music_theory

let test_pitch_construction () =
  Alcotest.(check int) "C is 0" 0 (Core.Pitch.to_int Core.Pitch.c);
  Alcotest.(check int) "C# is 1" 1 (Core.Pitch.to_int Core.Pitch.c_sharp);
  Alcotest.(check int) "D is 2" 2 (Core.Pitch.to_int Core.Pitch.d);
  Alcotest.(check int) "G is 7" 7 (Core.Pitch.to_int Core.Pitch.g);
  Alcotest.(check int) "B is 11" 11 (Core.Pitch.to_int Core.Pitch.b);
  Alcotest.(check bool) "C# = Db" true
    (Core.Pitch.equal Core.Pitch.c_sharp Core.Pitch.d_flat)

let test_of_int () =
  Alcotest.(check int) "of_int 0 = C" 0
    (Core.Pitch.to_int (Core.Pitch.of_int 0));
  Alcotest.(check int) "of_int 7 = G" 7
    (Core.Pitch.to_int (Core.Pitch.of_int 7));
  Alcotest.(check int) "of_int 12 = C (mod 12)" 0
    (Core.Pitch.to_int (Core.Pitch.of_int 12));
  Alcotest.(check int) "of_int (-1) = B (mod 12)" 11
    (Core.Pitch.to_int (Core.Pitch.of_int (-1)));
  Alcotest.(check int) "of_int 13 = C# (mod 12)" 1
    (Core.Pitch.to_int (Core.Pitch.of_int 13))

let test_pitch_arithmetic () =
  let c = Core.Pitch.c in
  let g = Core.Pitch.add c 7 in
  Alcotest.(check int) "C + 7 semitones = G" 7 (Core.Pitch.to_int g);

  let c_again = Core.Pitch.add g 5 in
  Alcotest.(check int) "G + 5 = C (mod 12)" 0 (Core.Pitch.to_int c_again);

  let b = Core.Pitch.add c (-1) in
  Alcotest.(check int) "C + (-1) = B" 11 (Core.Pitch.to_int b)

let test_pitch_distance () =
  let c = Core.Pitch.c in
  let g = Core.Pitch.g in
  Alcotest.(check int) "C to G = 7 semitones" 7 (Core.Pitch.diff c g);

  let d = Core.Pitch.d in
  Alcotest.(check int) "G to D = 7 semitones" 7 (Core.Pitch.diff g d);

  Alcotest.(check int) "C to C = 0" 0 (Core.Pitch.diff c c)

let test_to_string () =
  Alcotest.(check string) "C with sharps" "C"
    (Core.Pitch.to_string ~prefer:`Sharp Core.Pitch.c);
  Alcotest.(check string) "C# with sharps" "C#"
    (Core.Pitch.to_string ~prefer:`Sharp Core.Pitch.c_sharp);
  Alcotest.(check string) "C# with flats" "Db"
    (Core.Pitch.to_string ~prefer:`Flat Core.Pitch.c_sharp);
  Alcotest.(check string) "B with sharps" "B"
    (Core.Pitch.to_string ~prefer:`Sharp Core.Pitch.b)

let test_all () =
  Alcotest.(check int) "all has 12 pitch classes" 12
    (List.length Core.Pitch.all);
  Alcotest.(check bool) "all contains C" true
    (List.mem Core.Pitch.c Core.Pitch.all);
  Alcotest.(check bool) "all contains B" true
    (List.mem Core.Pitch.b Core.Pitch.all)

let () =
  Alcotest.run "Pitch"
    [ ("construction", [
        Alcotest.test_case "pitch values" `Quick test_pitch_construction;
        Alcotest.test_case "of_int" `Quick test_of_int;
      ])
    ; ("arithmetic", [
        Alcotest.test_case "addition" `Quick test_pitch_arithmetic;
        Alcotest.test_case "distance" `Quick test_pitch_distance;
      ])
    ; ("string conversion", [
        Alcotest.test_case "to_string" `Quick test_to_string;
        Alcotest.test_case "all" `Quick test_all;
      ])
    ]
