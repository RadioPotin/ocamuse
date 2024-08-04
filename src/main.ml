open Lwt
open LTerm_geom
open LTerm_key
open Types

module MENU = struct

  let menu view =
    match view with
    | _ -> {| Use arrow keys to change color - Up Down Left Righ for color change - Enter for Pattern view - Escape to return |}

end

let rec loop ui view =
  let open Types in
  LTerm_ui.wait ui >>= function
  | LTerm_event.Key{ code = Up; _ } ->
    (* blue fretboard *)
    view := Fretted Up;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Down; _ } ->
    (* red fretboard *)
    view := Fretted Down;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Left; _ } ->
    (* green fretboard *)
    view := Plain Left;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Right; _ } ->
    (* green fretboard *)
    view := Interline Right;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Enter; _ } ->
    (* green fretboard *)
    view := Pattern C_mode;
    LTerm_ui.draw ui;
    loop_pattern_view ui view

  | LTerm_event.Key{ code = Escape; _ } ->
    return ()
  | _ ->
    loop ui view

and loop_pattern_view ui view =
  LTerm_ui.wait ui >>= function
  | LTerm_event.Key{ code = Enter; _ } ->
    (* green fretboard *)
    view := Pattern C_mode;
    LTerm_ui.draw ui;
    loop_pattern_view ui view
  | LTerm_event.Key{ code = Escape; _ } ->
    return ()
  | _ ->
    loop_pattern_view ui view

(* ********************************************** *)
(* Initialize a default fretboard for development *)
(* ********************************************** *)
let fretboard  =
  let default_tuning () : Types.tuning =
    List.map
      (fun note -> Types.{ base = note; alteration = 0 })
      [ E; A; D; G; B; E ]
  in
  let tuning = Some (default_tuning ()) in
  Fretboard.init
    ~tuning:(Option.value tuning ~default:(default_tuning ()))
    ~range:13 ()
(* ********************************************** *)
(* ********************************************** *)
let select_display_mode event size ctx fretboard =
  let color = Display.COLOR.event_to_color_full_view event in
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
    Display.DRAW.MATRIX.write_rows_with_no_interline sub_ctx fretboard color
  | Types.Interline _ ->
    Display.DRAW.MATRIX.write_rows_with_interlines sub_ctx fretboard color
  | Types.Plain _ ->
    let open LTerm_geom in
    let offset_of_sub_context = 18 in
    let position =
      {
        row1 = offset_of_sub_context + 1;
        col1 = offset_of_sub_context * 2 + 4;
        row2 = size.rows - 1;
        col2 = size.cols - 1;
      }
    in
    let ctx = LTerm_draw.sub ctx position in
    Display.DRAW.MATRIX.write_plain_rows ctx fretboard color
  | Pattern _ ->
    Display.DRAW.MATRIX.write_rows_with_interlines sub_ctx fretboard color

let view_fretboard ctx event fretboard size =
  let display_mode =
    select_display_mode event size
  in
  display_mode ctx fretboard

(* ********************************************** *)
(* ********************************************** *)

let draw lt_matrix m view =
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
    (Zed_string.of_utf8 (MENU.menu view))
    LTerm_draw.Light;
  view_fretboard ctx view fretboard size

let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->
  (* Coordinates of the message. *)
  let view = ref (Plain Up) in
  LTerm_ui.create term (fun lt_matrix size -> draw lt_matrix size !view)
  >>= fun ui ->
  Lwt.finalize (fun () -> loop ui view) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
