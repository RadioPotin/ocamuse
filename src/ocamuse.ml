(* Default context initialization *)
let default_context () =
  let open Types in
  let standard_tuning = [ A; E; A; D; G; B; E ] in
  let default_tuning () : Types.tuning =
    List.map (fun note -> { base = note; alteration = 0 }) standard_tuning
  in
  let base_colour = Color.random_base_colour () in
  let display_mode = ref (Flat (Plain base_colour)) in
  let root_note = { base = E; alteration = 0 } in
  let tuning = default_tuning () in
  let fretboard =
    Fretboard.init
      ~tuning:(Option.value (Some tuning) ~default:(default_tuning ()))
      ~range:13 ()
  in
  { display_mode
  ; fretboard
  ; base_colour = ref base_colour
  ; tuning
  ; root_note
  ; mode = C_mode
  }

let run () =
  let open Lwt in
  let waiter, wakener = wait () in
  let ocamuse_context = default_context () in
  let main_box = new Classes.main_box ocamuse_context wakener in

  Lazy.force LTerm.stdout >>= fun term ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term main_box waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (run ())
