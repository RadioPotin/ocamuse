(* Default context initialization *)
let default_context ?(tuning = Config.default_tuning ()) () =
  let open Types in
  let base_colour = Color.random_base_colour () in
  let display_mode = ref (Flat (Plain base_colour)) in
  let root_note = { base = E; alteration = 0 } in
  let fretboard_data =
    Fretboard.init ~tuning ~range:Config.default_fret_range ()
  in
  { display_mode
  ; fretboard = fretboard_data
  ; base_colour = ref base_colour
  ; tuning
  ; root_note
  ; mode = C_mode
  }

(*
  TODO:
    * Make context menu in half of bottom-right pane, show:
      * Current mode
      * Current tonality
      * Tuning
      *
 *)
let run ?(tuning = Config.default_tuning ()) () =
  let open Lwt in
  let waiter, wakener = wait () in
  let ocamuse_context = default_context ~tuning () in
  let main_box = new Classes.main_box ocamuse_context wakener in

  Lazy.force LTerm.stdout >>= fun term ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term main_box waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (run ())
