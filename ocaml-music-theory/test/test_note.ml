open Music_theory

let test_construction () =
  let c = Core.Note.c in
  Alcotest.(check int) "C base to index" 0
    (Core.Note.base_to_index c.base);
  Alcotest.(check int) "C alteration" 0 c.alteration;

  let c_sharp = Core.Note.c_sharp in
  Alcotest.(check int) "C# alteration" 1 c_sharp.alteration;

  let d_flat = Core.Note.d_flat in
  Alcotest.(check int) "Db alteration" (-1) d_flat.alteration

let test_to_pitch () =
  let c = Core.Note.c in
  let c_pitch = Core.Note.to_pitch c in
  Alcotest.(check int) "C to pitch class 0" 0 (Core.Pitch.to_int c_pitch);

  let c_sharp = Core.Note.c_sharp in
  let cs_pitch = Core.Note.to_pitch c_sharp in
  Alcotest.(check int) "C# to pitch class 1" 1 (Core.Pitch.to_int cs_pitch);

  let d_flat = Core.Note.d_flat in
  let db_pitch = Core.Note.to_pitch d_flat in
  Alcotest.(check int) "Db to pitch class 1" 1 (Core.Pitch.to_int db_pitch)

let test_enharmonic_eq () =
  let c_sharp = Core.Note.c_sharp in
  let d_flat = Core.Note.d_flat in
  Alcotest.(check bool) "C# = Db enharmonically" true
    (Core.Note.enharmonic_eq c_sharp d_flat);

  let c = Core.Note.c in
  let d = Core.Note.d in
  Alcotest.(check bool) "C ≠ D" false
    (Core.Note.enharmonic_eq c d)

let test_base_operations () =
  Alcotest.(check bool) "next_base C = D" true
    (Core.Note.next_base Core.Note.C = Core.Note.D);
  Alcotest.(check bool) "next_base B = C" true
    (Core.Note.next_base Core.Note.B = Core.Note.C);

  Alcotest.(check bool) "prev_base D = C" true
    (Core.Note.prev_base Core.Note.D = Core.Note.C);
  Alcotest.(check bool) "prev_base C = B" true
    (Core.Note.prev_base Core.Note.C = Core.Note.B);

  Alcotest.(check int) "distance C to D" 2
    (Core.Note.base_distance Core.Note.C);
  Alcotest.(check int) "distance E to F" 1
    (Core.Note.base_distance Core.Note.E)

let test_string_conversion () =
  Alcotest.(check string) "C to string" "C"
    (Core.Note.to_string Core.Note.c);
  Alcotest.(check string) "C# to string" "C#"
    (Core.Note.to_string Core.Note.c_sharp);
  Alcotest.(check string) "Db to string" "Db"
    (Core.Note.to_string Core.Note.d_flat);

  (* Test unicode *)
  Alcotest.(check string) "C# unicode" "C♯"
    (Core.Note.to_string ~unicode:true Core.Note.c_sharp);
  Alcotest.(check string) "Db unicode" "D♭"
    (Core.Note.to_string ~unicode:true Core.Note.d_flat)

let test_of_string () =
  (match Core.Note.of_string "C" with
  | Ok note ->
    Alcotest.(check bool) "Parse C" true
      (Core.Note.equal note Core.Note.c)
  | Error e -> Alcotest.failf "Failed to parse C: %s" e);

  (match Core.Note.of_string "C#" with
  | Ok note ->
    Alcotest.(check bool) "Parse C#" true
      (Core.Note.equal note Core.Note.c_sharp)
  | Error e -> Alcotest.failf "Failed to parse C#: %s" e);

  (match Core.Note.of_string "Db" with
  | Ok note ->
    Alcotest.(check bool) "Parse Db" true
      (Core.Note.equal note Core.Note.d_flat)
  | Error e -> Alcotest.failf "Failed to parse Db: %s" e);

  match Core.Note.of_string "Invalid" with
  | Ok _ -> Alcotest.fail "Should not parse Invalid"
  | Error _ -> ()

let () =
  Alcotest.run "Note"
    [ ("construction", [
        Alcotest.test_case "make notes" `Quick test_construction;
      ])
    ; ("pitch conversion", [
        Alcotest.test_case "to_pitch" `Quick test_to_pitch;
        Alcotest.test_case "enharmonic_eq" `Quick test_enharmonic_eq;
      ])
    ; ("base operations", [
        Alcotest.test_case "next/prev/distance" `Quick test_base_operations;
      ])
    ; ("string conversion", [
        Alcotest.test_case "to_string" `Quick test_string_conversion;
        Alcotest.test_case "of_string" `Quick test_of_string;
      ])
    ]
