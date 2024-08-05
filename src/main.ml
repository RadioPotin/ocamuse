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

let rec loop ui display =
  let open Types in
  begin
    match !display with
    | Flat mode ->
      let color = Display.COLOR.bubble_color mode in
      begin
        LTerm_ui.wait ui >>= function
        | LTerm_event.Key{ code = Up; _ } ->
          (* blue fretboard *)
          display := Flat (Fretted color);
          LTerm_ui.draw ui;
          loop ui display

        | LTerm_event.Key{ code = Left; _ } ->
          (* green fretboard *)
          display := Flat (Plain color);
          LTerm_ui.draw ui;
          loop ui display
        | LTerm_event.Key{ code = Right; _ } ->
          (* green fretboard *)
          display := Flat (Interline color);
          LTerm_ui.draw ui;
          loop ui display
        | LTerm_event.Key{ code = Prev_page; _ } ->
          (* blue fretboard *)
          display := Flat ( update_color Display.COLOR.rotate_to_prev mode);
          LTerm_ui.draw ui;
          loop ui display

        | LTerm_event.Key{ code = Next_page; _ } ->
          (* green fretboard *)
          display := Flat ( update_color Display.COLOR.rotate_to_next mode);
          LTerm_ui.draw ui;
          loop ui display

        | LTerm_event.Key{ code = Escape; _ } ->
          return ()
        | _ ->
          loop ui display
      end
    | Pattern _ -> assert false
  end


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
open LTerm_geom
let select_display_mode event size ctx fretboard =
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
    Display.MATRIX.DRAW.write_rows_with_no_interline sub_ctx fretboard color
  | Types.Interline _ ->
    Display.MATRIX.DRAW.write_rows_with_interlines sub_ctx fretboard color
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
    Display.MATRIX.DRAW.write_plain_rows ctx fretboard color

let view_fretboard ctx display fretboard size =
  let open Types in
  begin
    match display with
    | Flat e ->
      let display_mode =
        select_display_mode e size
      in
      display_mode ctx fretboard
    | Pattern _m -> assert false
  end

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
  let open Types in
  begin
    Lazy.force LTerm.stdout
    >>= fun term ->
    (* Coordinates of the message. *)
    let display_mode = ref (Flat (Plain Lwhite)) in
    LTerm_ui.create term (fun lt_matrix size -> draw lt_matrix size !display_mode)
    >>= fun ui ->
    Lwt.finalize (fun () -> loop ui display_mode) (fun () -> LTerm_ui.quit ui)
  end

let () = Lwt_main.run (main ())
