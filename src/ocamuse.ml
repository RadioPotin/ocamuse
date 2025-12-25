(* Default context initialization *)
let default_context ?(tuning = Config.default_tuning ()) () =
  let open Types in
  let base_colour = Color.random_base_colour () in
  let display_mode = ref (Flat (Plain base_colour)) in
  let root_note = { base = E; alteration = 0 } in
  let default_scale = Ionian in
  let spelling_tbl = Display.build_spelling_table default_scale root_note in
  let fretboard_data =
    Fretboard.init ~tuning ~range:Config.default_fret_range ~spelling_tbl ()
  in
  { display_mode
  ; fretboard = fretboard_data
  ; base_colour = ref base_colour
  ; tuning
  ; root_note
  ; scale = default_scale
  ; highlight_source = Tonality (default_scale, root_note)
  ; color_theme = CustomPalette "Circle of Fifths"
  ; fret_range = Config.default_fret_range
  }

let run ?(tuning = Config.default_tuning ()) () =
  let open Lwt in
  let ocamuse_context = default_context ~tuning () in

  (* Use lambda-term's layer system for modals *)
  let do_run, push_layer, pop_layer, exit = LTerm_widget.prepare_simple_run () in
  let main_box = new Classes.main_box ocamuse_context exit push_layer pop_layer in

  Lazy.force LTerm.stdout >>= fun term ->
  Lwt.finalize
    (fun () ->
      LTerm.enable_mouse term >>= fun () ->
      do_run main_box)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (run ())
