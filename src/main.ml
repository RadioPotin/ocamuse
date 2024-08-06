open Lwt

module MENU = struct

  let menu view =
    match view with
    | _ -> {| Use arrow keys to change color - Up Down Left Righ for color change - Enter for Pattern view - Escape to return |}

end

let update_color rotate =
  let open Types in
  function
  | Fretted c -> Fretted (rotate c)
  | Plain c -> Plain (rotate c)
  | Interline c -> Interline (rotate c)

(* ********************************************** *)
(* ********************************************** *)

let rec loop ui ocamuse_context =
  let open Types in
  begin
    match !(ocamuse_context.display_mode) with
    | Flat mode ->
      let color = Display.COLOR.bubble_color mode in
      begin
        LTerm_ui.wait ui >>= function
        | LTerm_event.Key{ code = Up; _ } ->
          (* blue fretboard *)
          ocamuse_context.display_mode := Flat (Fretted color);
          LTerm_ui.draw ui;
          loop ui ocamuse_context

        | LTerm_event.Key{ code = Left; _ } ->
          (* green fretboard *)
          ocamuse_context.display_mode := Flat (Plain color);
          LTerm_ui.draw ui;
          loop ui ocamuse_context
        | LTerm_event.Key{ code = Right; _ } ->
          (* green fretboard *)
          ocamuse_context.display_mode := Flat (Interline color);
          LTerm_ui.draw ui;
          loop ui ocamuse_context
        | LTerm_event.Key{ code = Prev_page; _ } ->
          (* blue fretboard *)
          ocamuse_context.display_mode := Flat ( update_color Display.COLOR.rotate_to_prev mode);
          LTerm_ui.draw ui;
          loop ui ocamuse_context

        | LTerm_event.Key{ code = Next_page; _ } ->
          (* green fretboard *)
          ocamuse_context.display_mode := Flat ( update_color Display.COLOR.rotate_to_next mode);
          LTerm_ui.draw ui;
          loop ui ocamuse_context

        | LTerm_event.Key{ code = Enter; _ } ->
          ocamuse_context.display_mode := Pattern (mode, C_mode);
          LTerm_ui.draw ui;
          loop ui ocamuse_context
        | LTerm_event.Key{ code = Escape; _ } ->
          return ()
        | _ ->
          loop ui ocamuse_context
      end
    | Pattern (view, mode) ->
      begin
        let color = Display.COLOR.bubble_color view in
        LTerm_ui.wait ui >>= function
        | LTerm_event.Key{ code = Up; _ } ->
          (* blue fretboard *)
          ocamuse_context.display_mode := Pattern (Fretted color, mode);
          LTerm_ui.draw ui;
          loop ui ocamuse_context

        | LTerm_event.Key{ code = Left; _ } ->
          (* green fretboard *)
          ocamuse_context.display_mode := Pattern (Plain color, mode);
          LTerm_ui.draw ui;
          loop ui ocamuse_context
        | LTerm_event.Key{ code = Right; _ } ->
          (* green fretboard *)
          ocamuse_context.display_mode := Pattern (Interline color, mode);
          LTerm_ui.draw ui;
          loop ui ocamuse_context
        | LTerm_event.Key{ code = Prev_page; _ } ->
          (* blue fretboard *)
          ocamuse_context.display_mode := Pattern ( update_color Display.COLOR.rotate_to_prev view, mode);
          LTerm_ui.draw ui;
          loop ui ocamuse_context

        | LTerm_event.Key{ code = Next_page; _ } ->
          (* green fretboard *)
          ocamuse_context.display_mode := Pattern ( update_color Display.COLOR.rotate_to_next view, mode);

          LTerm_ui.draw ui;
          loop ui ocamuse_context

        | LTerm_event.Key{ code = Enter; _ } ->
          ocamuse_context.display_mode := Pattern (view, mode);
          LTerm_ui.draw ui;
          loop ui ocamuse_context
        | LTerm_event.Key{ code = Escape; _ } ->
          return ()
        | _ ->
          loop ui ocamuse_context
      end

  end

let select_full_view_display_mode size ctx ocamuse_context event =
  let open LTerm_geom in
  let open Types in
  let color = Display.COLOR.event_to_color_flat_view event in
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
  | Types.Fretted _ ->
    Display.MATRIX.DRAW.LINE.write_rows_with_no_interline sub_ctx ocamuse_context.fretboard color
  | Types.Interline _ ->
    Display.MATRIX.DRAW.LINE.write_rows_with_interlines sub_ctx ocamuse_context.fretboard color
  | Types.Plain _ ->
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
    Display.MATRIX.DRAW.LINE.write_plain_rows ctx ocamuse_context.fretboard color

let select_pattern_view_display_mode size ctx ocamuse_context e =
  let open Types in
  let open LTerm_geom in
  let _sub_ctx =
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
  let e, mode = e in
  match e with
  | Plain _color ->
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
    Display.MATRIX.DRAW.pattern ctx size ocamuse_context mode
  | _ -> assert false

let view_fretboard ctx size ocamuse_context =
  let open Types in
  begin
    match !(ocamuse_context.display_mode) with
    | Flat e ->
      select_full_view_display_mode size ctx ocamuse_context e
    | Pattern (view, mode) ->
      select_pattern_view_display_mode size ctx ocamuse_context (view, mode)
  end

(* ********************************************** *)
(* ********************************************** *)

let draw lt_matrix m ocamuse_context =
  let size = LTerm_ui.size lt_matrix in
  let ctx = LTerm_draw.context m size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_frame_labelled ctx
    {
      row1 = 0;
      col1 = 0;
      row2 = size.rows;
      col2 = size.cols;
    }
    ~alignment:H_align_center
    (Zed_string.of_utf8 (MENU.menu ocamuse_context))
    LTerm_draw.Light;
  view_fretboard ctx size ocamuse_context

let default_context () =
  let open Types in
  {
    display_mode = ref (Flat (Plain Lwhite));
    (* ********************************************** *)
    (* Initialize a default fretboard for development *)
    (* ********************************************** *)
    fretboard =
      let default_tuning () : Types.tuning =
        List.map
          (fun note -> Types.{ base = note; alteration = 0 })
          [ E; A; D; G; B; E ]
      in
      let tuning = Some (default_tuning ()) in
      Fretboard.init
        ~tuning:(Option.value tuning ~default:(default_tuning ()))
        ~range:13 ();
  }


(* TODO: Add cli to chose between interactive or cli display *)
let main () =
  begin
    Lazy.force LTerm.stdout
    >>= fun term ->
    (* Coordinates of the message. *)
    let ocamuse_structure = default_context () in
    LTerm_ui.create term (fun lt_matrix size -> draw lt_matrix size ocamuse_structure)
    >>= fun ui ->
    Lwt.finalize (fun () -> loop ui ocamuse_structure) (fun () -> LTerm_ui.quit ui)
  end

let () = Lwt_main.run (main ())
