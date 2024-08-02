let fretboard =
  let default_tuning () : Types.tuning =
    List.map
      (fun note -> Types.{ base = note; alteration = 0 })
      [ E; A; D; G; B; E ]
  in
  let tuning = Some (default_tuning ()) in
  Fretboard.init
    ~tuning:(Option.value tuning ~default:(default_tuning ()))
    ~range:13 ()

open Lwt
open LTerm_geom
open LTerm_key
open Types

let rec loop ui view =
  LTerm_ui.wait ui >>= function
  | LTerm_event.Key{ code = Up; _ } ->
    (* blue fretboard *)
    view := Up;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Down; _ } ->
    (* red fretboard *)
    view := Down;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Left; _ } ->
    (* green fretboard *)
    view := Left;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Right; _ } ->
    (* green fretboard *)
    view := Right;
    LTerm_ui.draw ui;
    loop ui view

  | LTerm_event.Key{ code = Escape; _ } ->
    return ()
  | _ ->
    loop ui view

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
    (Zed_string.of_utf8 "Use arrow keys to change view") LTerm_draw.Light;
  (* make functions to display fretboard in matrix, add functions to print slightly differently *)
  Pp.DISPLAY.simple_fretboard_with_frets ctx size view fretboard

let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->

  (* Coordinates of the message. *)
  let view = ref Up in

  LTerm_ui.create term (fun lt_matrix size -> draw lt_matrix size !view)
  >>= fun ui ->
  Lwt.finalize (fun () -> loop ui view) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
