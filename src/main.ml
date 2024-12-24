open Lwt
open LTerm_geom

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

let select_full_view_display_mode size ctx ocamuse_context event =
  let open LTerm_geom in
  let open Types in
  let color = Color.event_to_color_flat_view event in
  let sub_ctx =
    let offset_of_sub_context = 15 in
    let position =
      {
        row1 = offset_of_sub_context;
        col1 = offset_of_sub_context;
        row2 = size.rows - 1;
        col2 = size.cols - 1;
      }
    in
    LTerm_draw.sub ctx position
  in
  match event with
  | Fretted _ ->
    Display.view_rows_with_no_interline sub_ctx ocamuse_context.fretboard color
  | Interline _ ->
    let sub_ctx =
      let offset_of_sub_context = 15 in
      let position =
        {
          row1 = 18;
          col1 = offset_of_sub_context;
          row2 = size.rows - 1;
          col2 = size.cols - 1;
        }
      in
      LTerm_draw.sub ctx position
    in
    Display.view_rows_with_interlines sub_ctx ocamuse_context.fretboard color
  | Plain _ ->
    let open LTerm_geom in
    let position =
      {
        row1 = 19;
        col1 = 42;
        row2 = size.rows - 1;
        col2 = size.cols - 1;
      }
    in
    let ctx = LTerm_draw.sub ctx position in
    Display.view_plain_rows ctx ocamuse_context.fretboard color

let select_pattern_view_display_mode size ctx ocamuse_context (view, mode) =
  let open Types in
  let open LTerm_geom in
  let sub_ctx =
    let offset_of_sub_context = 15 in
    let position =
      {
        row1 = offset_of_sub_context;
        col1 = offset_of_sub_context;
        row2 = size.rows - 1;
        col2 = size.cols - 1;
      }
    in
    LTerm_draw.sub ctx position
  in
  match view with
  | Fretted color ->
    ocamuse_context.base_colour := color;
    Display.view_pattern sub_ctx view ocamuse_context mode
  | Interline color ->
    ocamuse_context.base_colour := color;
    Display.view_pattern sub_ctx view ocamuse_context mode
  | Plain color ->
    let open LTerm_geom in
    let position =
      {
        row1 = 16;
        col1 = 42;
        row2 = size.rows - 1;
        col2 = size.cols - 1;
      }
    in
    let ctx = LTerm_draw.sub ctx position in
    ocamuse_context.base_colour := color;
    Display.view_pattern ctx view ocamuse_context mode

let select_view ctx size ocamuse_context =
  let open Types in
  LTerm_draw.clear ctx;
  match !(ocamuse_context.display_mode) with
  | Flat mode ->
    select_full_view_display_mode size ctx ocamuse_context mode
  | Pattern (view, mode) ->
    select_pattern_view_display_mode size ctx ocamuse_context (view, mode)

class fretboard_widget ocamuse_context  =
  object(self)
    inherit LTerm_widget.t "fretboard"

    val mutable matrix =
      let open Types in
      LTerm_draw.make_matrix
        {rows = Array.length ocamuse_context.fretboard;
          cols = Array.length ocamuse_context.fretboard.(0)}

    method! draw ctx _focused =
      select_view ctx (self#size_request) ocamuse_context

    (* Override the draw method to print fretboard based on the state *)
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

let run () =
  let main_box = new LTerm_widget.vbox in
  let waiter, wakener = wait () in
  let ocamuse_context = default_context () in

  let fretboard = new fretboard_widget ocamuse_context in
  main_box#add ~expand:true fretboard;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.disable_mouse term >>= fun () ->
  main_box#on_event (function
      LTerm_event.Key{LTerm_key.code=LTerm_key.Escape ;_} ->
      wakeup wakener (); false | _ -> false);
  Lwt.finalize
    (fun () -> LTerm_widget.run term main_box waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (run ())
