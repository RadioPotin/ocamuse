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
    view := Plain Up;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Down; _ } ->
    (* red fretboard *)
    view := Plain Down;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Left; _ } ->
    (* green fretboard *)
    view := Fretted Left;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Right; _ } ->
    (* green fretboard *)
    view := Fretted Right;
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
  match view with
  | view -> Pp.DISPLAY.DRAW.fretboard_with_frets ctx view fretboard

let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->
  (* Coordinates of the message. *)
  let view = ref (Plain Up) in
  LTerm_ui.create term (fun lt_matrix size -> draw lt_matrix size !view)
  >>= fun ui ->
  Lwt.finalize (fun () -> loop ui view) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
