let stop () = failwith @@ Format.sprintf "Usage: %s <mode> <note>" Sys.argv.(0)

let help () = stop ()

let usage () = help ()

let () =
  let default_tuning () : Types.tuning =
    List.map
      (fun note -> Types.{ base = note; alteration = 0 })
      [ E; A; D; G; B; E ]
  in

  let tuning = Some (default_tuning ()) in
  let mode, note =
    Conv.mode_of_string "C",
    Conv.note_of_string "C"
  in
  let fmt = Format.std_formatter in
  Pp.NOTES.print_notes fmt @@ Ocamuse.build_tonality mode note;
  Pp.NOTES.print_diatonic_chords fmt
  @@ Ocamuse.build_diatonic_triads_sequence mode note;
  let fb =
    Fretboard.init
      ~tuning:(Option.value tuning ~default:(default_tuning ()))
      ~range:13 ()
  in
  Pp.FRETBOARD.fb fb;
  Format.printf "@\n";
  Pp.FRETBOARD.fb_with_frets fb
