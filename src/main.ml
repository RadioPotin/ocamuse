let stop () = failwith @@ Format.sprintf "Usage: %s <mode> <note>" Sys.argv.(0)

let help () = stop ()

let usage () = help ()

let () =
  let finish_with f =
    f ();
    exit 1
  in
  let use_tuning t =
    let l = String.split_on_char ';' t in
    List.map Conv.note_of_string l
  in

  let tuning = ref None in
  let mode = ref None in
  let note = ref None in

  let cli =
    [ ("--help", Arg.Unit (fun () -> finish_with help), "")
    ; ("-help", Arg.Unit (fun () -> finish_with help), "")
    ; ("-h", Arg.Unit (fun () -> finish_with help), "")
    ; ("--usage", Arg.Unit (fun () -> finish_with usage), "")
    ; ("-u", Arg.Unit (fun () -> finish_with usage), "")
    ; ("--mode", Arg.String (fun m -> mode := Some (Conv.mode_of_string m)), "")
    ; ("-m", Arg.String (fun m -> mode := Some (Conv.mode_of_string m)), "")
    ; ("--note", Arg.String (fun n -> note := Some (Conv.note_of_string n)), "")
    ; ("-n", Arg.String (fun n -> note := Some (Conv.note_of_string n)), "")
    ; ("--tuning", Arg.String (fun t -> tuning := Some (use_tuning t)), "")
    ; ("-t", Arg.String (fun t -> tuning := Some (use_tuning t)), "")
    ; ("-pa", Arg.String (fun t -> tuning := Some (use_tuning t)), "")
    ]
  in

  Arg.parse cli (fun s -> failwith @@ Format.sprintf "unknown arg `%s`" s) "";

  let fmt = Format.std_formatter in
  let mode, note =
    match (!mode, !note) with
    | None, None | None, Some _ | Some _, None -> help ()
    | Some mode, Some note -> (mode, note)
  in
  let default_tuning () : Types.tuning =
    List.map
      (fun note -> Types.{ base = note; alteration = 0 })
      [ E; A; D; G; B; E ]
  in
  Pp.NOTES.print_notes fmt @@ Ocamuse.build_tonality mode note;
  Pp.NOTES.print_diatonic_chords fmt
  @@ Ocamuse.build_diatonic_triads_sequence mode note;
  let fb =
    Fretboard.init
      ~tuning:(Option.value !tuning ~default:(default_tuning ()))
      ~range:13 ()
  in
  Pp.FRETBOARD.fb fb;
  Format.printf "@\n";
  Pp.FRETBOARD.fb_with_frets fb
