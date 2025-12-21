open Music_theory

let () =
  Printf.printf "=== OCaml Music Theory Library - Basic Usage Examples ===\n\n";

  (* Pitch classes *)
  Printf.printf "=== Pitch Classes ===\n";
  let c = Core.Pitch.c in
  let g = Core.Pitch.add c 7 in
  Printf.printf "C + 7 semitones = %s\n" (Core.Pitch.to_string ~prefer:`Sharp g);
  Printf.printf "C and G are %d semitones apart\n" (Core.Pitch.diff c g);

  let pc_list = [Core.Pitch.c; Core.Pitch.d; Core.Pitch.e; Core.Pitch.f;
                 Core.Pitch.g; Core.Pitch.a; Core.Pitch.b] in
  Printf.printf "C major scale pitch classes (sharps): ";
  List.iter (fun pc ->
    Printf.printf "%s " (Core.Pitch.to_string ~prefer:`Sharp pc)
  ) pc_list;
  Printf.printf "\n\n";

  (* Intervals *)
  Printf.printf "=== Intervals ===\n";
  let fifth = Core.Interval.PerfectFifth in
  Printf.printf "Perfect fifth = %d semitones (%s)\n"
    (Core.Interval.to_semitones fifth)
    (Core.Interval.to_string fifth);
  Printf.printf "Inverted: %s\n"
    (Core.Interval.to_string (Core.Interval.invert fifth));
  Printf.printf "Letter distance: %d steps\n"
    (Core.Interval.letter_distance fifth);

  let third = Core.Interval.MajorThird in
  Printf.printf "\nMajor third = %d semitones (%s)\n"
    (Core.Interval.to_semitones third)
    (Core.Interval.to_string third);
  Printf.printf "Is major? %b\n" (Core.Interval.is_major third);
  Printf.printf "Is perfect? %b\n" (Core.Interval.is_perfect third);
  Printf.printf "\n";

  (* Notes *)
  Printf.printf "=== Notes ===\n";
  let c_note = Core.Note.c in
  let c_sharp = Core.Note.sharp Core.Note.C in
  let d_flat = Core.Note.flat Core.Note.D in

  Printf.printf "C note: %s\n" (Core.Note.to_string c_note);
  Printf.printf "C# note: %s\n" (Core.Note.to_string c_sharp);
  Printf.printf "Db note: %s\n" (Core.Note.to_string d_flat);
  Printf.printf "C# and Db enharmonic? %b\n"
    (Core.Note.enharmonic_eq c_sharp d_flat);

  Printf.printf "\nWith unicode symbols:\n";
  Printf.printf "C# = %s\n" (Core.Note.to_string ~unicode:true c_sharp);
  Printf.printf "Db = %s\n" (Core.Note.to_string ~unicode:true d_flat);

  (* String parsing *)
  Printf.printf "\n=== String Parsing ===\n";
  (match Core.Note.of_string "F#" with
  | Ok note -> Printf.printf "Parsed 'F#': %s\n" (Core.Note.to_string note)
  | Error msg -> Printf.printf "Error: %s\n" msg);

  (match Core.Note.of_string "Bb" with
  | Ok note -> Printf.printf "Parsed 'Bb': %s\n" (Core.Note.to_string note)
  | Error msg -> Printf.printf "Error: %s\n" msg);

  (match Core.Note.of_string "Invalid" with
  | Ok note -> Printf.printf "Parsed: %s\n" (Core.Note.to_string note)
  | Error msg -> Printf.printf "Failed to parse 'Invalid': %s\n" msg);

  (* Base note operations *)
  Printf.printf "\n=== Base Note Operations ===\n";
  Printf.printf "Next after C: %s\n"
    (Core.Note.to_string (Core.Note.natural (Core.Note.next_base Core.Note.C)));
  Printf.printf "Next after B: %s\n"
    (Core.Note.to_string (Core.Note.natural (Core.Note.next_base Core.Note.B)));
  Printf.printf "Distance from C to next (D): %d semitones\n"
    (Core.Note.base_distance Core.Note.C);
  Printf.printf "Distance from E to next (F): %d semitones\n"
    (Core.Note.base_distance Core.Note.E);

  Printf.printf "\n=== Example Complete ===\n"
