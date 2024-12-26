(* Default context initialization *)
let default_context () =
  let open Types in
  let standard_tuning = [ E; A; D; G; B; E ] in
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
  {
    display_mode;
    fretboard;
    base_colour = ref base_colour;
    tuning;
    root_note;
    mode = C_mode;
  }


module MENU = struct
  let _menu_message =
    {| Use arrow keys to change view mode - Page Up / Page Down for color change - Enter for Pattern view - Escape to return |}
end

let update_color rotate =
  let open Types in
  function
  | Fretted c -> Fretted (rotate c)
  | Plain c -> Plain (rotate c)
  | Interline c -> Interline (rotate c)

let bubble_view =
  let open Types in
  function
  | Flat view -> view
  | Pattern (view, _mode) -> view

let select_pattern_view_display_mode ctx ocamuse_context (view, mode) =
  let open Types in
  match view with
  | Fretted color ->
    ocamuse_context.base_colour := color;
    Display.view_pattern ctx view ocamuse_context mode
  | Interline color ->
    ocamuse_context.base_colour := color;
    Display.view_pattern ctx view ocamuse_context mode
  | Plain color ->
    ocamuse_context.base_colour := color;
    Display.view_pattern ctx view ocamuse_context mode

let select_full_view_display_mode ctx ocamuse_context event =
  let open Types in
  let color = Color.event_to_color_flat_view event in
  match event with
  | Fretted _ ->
    Display.view_rows_with_no_interline ctx ocamuse_context.fretboard color
  | Interline _ ->
    Display.view_rows_with_interlines ctx ocamuse_context.fretboard color
  | Plain _ ->
    Display.view_plain_rows ctx ocamuse_context.fretboard color


let select_view ctx ocamuse_context =
  let open Types in
  LTerm_draw.clear ctx;
  match !(ocamuse_context.display_mode) with
  | Flat mode ->
    select_full_view_display_mode ctx ocamuse_context mode
  | Pattern (view, mode) ->
    select_pattern_view_display_mode ctx ocamuse_context (view, mode)

class fretboard_widget ocamuse_context =
  object (self)
    inherit LTerm_widget.t "fretboard"

    method! draw ctx _focused =
      LTerm_draw.clear ctx;
      select_view ctx ocamuse_context

    initializer
      self#on_event (fun event ->
        let open LTerm_event in
        let open Types in
        match event with
        | Key { code = Up; _ } ->
          (* Blue fretboard *)
          ocamuse_context.display_mode := Flat (Fretted !(ocamuse_context.base_colour));
          self#queue_draw;
          true
        | Key { code = Left; _ } ->
          (* Green fretboard *)
          ocamuse_context.display_mode := Flat (Plain !(ocamuse_context.base_colour));
          self#queue_draw;
          true
        | Key { code = Right; _ } ->
          (* Interline fretboard *)
          ocamuse_context.display_mode := Flat (Interline !(ocamuse_context.base_colour));
          self#queue_draw;
          true
        | Key { code = Prev_page; _ } ->
          (* Rotate color backward *)
          ocamuse_context.base_colour :=
            Color.rotate_to_prev !(ocamuse_context.base_colour);
          ocamuse_context.display_mode :=
            Flat (update_color Color.rotate_to_prev
              @@ bubble_view !(ocamuse_context.display_mode));
          self#queue_draw;
          true
        | Key { code = Next_page; _ } ->
          (* Rotate color forward *)
          ocamuse_context.base_colour :=
            Color.rotate_to_next !(ocamuse_context.base_colour);
          ocamuse_context.display_mode :=
            Flat (update_color Color.rotate_to_next
              @@ bubble_view !(ocamuse_context.display_mode));
          self#queue_draw;
          true
        | Key { code = Enter; _ } ->
          (* Switch to Pattern view *)
          begin
            match !(ocamuse_context.display_mode) with
            | Flat mode ->
              ocamuse_context.display_mode := Pattern (mode, C_mode);
              self#queue_draw;
              true
            | _ -> true
          end
        | Key { code = Escape; _ } ->
          (* Exit Pattern view *)
          begin
            match !(ocamuse_context.display_mode) with
            | Pattern (view, _) ->
              ocamuse_context.display_mode := Flat view;
              self#queue_draw;
              true
            | _ -> true
          end
        | _ -> false
      )

  end


class main_box ocamuse_context wakener =
  object (self)
    inherit LTerm_widget.vbox

    val fretboard_widget = new fretboard_widget ocamuse_context

    initializer
      self#add fretboard_widget;
      self#on_event (fun event ->
        let open LTerm_key in
        match event with
        | Key { code = Escape; _ } ->
          (* Signal termination *)
          Lwt.wakeup wakener ();
          false
        | Key { code = Up; _ }
        | Key { code = Left; _ }
        | Key { code = Right; _ }
        | Key { code = Prev_page; _ }
        | Key { code = Next_page; _ }
        | Key { code = Enter; _ } ->
          (* Propagate the event to the fretboard widget *)
          fretboard_widget#send_event event;
          true
        | _ -> false)
  end


let run () =
  let open Lwt in
  let waiter, wakener = wait () in
  let ocamuse_context = default_context () in
  let main_box = new main_box ocamuse_context wakener in

  Lazy.force LTerm.stdout >>= fun term ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term main_box waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (run ())
