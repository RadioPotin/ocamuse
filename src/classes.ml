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
  function Flat view -> view | Pattern (view, _mode) -> view

class fretboard_widget ocamuse_context =
  object (self)
    inherit LTerm_widget.t "fretboard"

    initializer
      self#on_event (fun event ->
        let open LTerm_event in
        let open Types in
        match !(ocamuse_context.display_mode) with
        | Flat view -> begin
          match event with
          | Key { code = Prev_page; _ } ->
            (* Rotate color backward *)
            ocamuse_context.base_colour :=
              Color.rotate_to_prev !(ocamuse_context.base_colour);
            ocamuse_context.display_mode :=
              Flat
                ( update_color Color.rotate_to_prev
                @@ bubble_view !(ocamuse_context.display_mode) );
            self#queue_draw;
            true
          | Key { code = Next_page; _ } ->
            (* Rotate color forward *)
            ocamuse_context.base_colour :=
              Color.rotate_to_next !(ocamuse_context.base_colour);
            ocamuse_context.display_mode :=
              Flat
                ( update_color Color.rotate_to_next
                @@ bubble_view !(ocamuse_context.display_mode) );
            self#queue_draw;
            true
          | Key { code = Enter; _ } ->
            begin
              match !(ocamuse_context.display_mode) with
              | Flat (Plain color) ->
                ocamuse_context.display_mode := Flat (Interline color)
              | Flat (Fretted color) ->
                ocamuse_context.display_mode := Flat (Plain color)
              | Flat (Interline color) ->
                ocamuse_context.display_mode := Flat (Fretted color)
              | Pattern _ -> assert false
            end;
            self#queue_draw;
            true
          | Key { code = Backspace; _ } ->
            ocamuse_context.display_mode := Flat view;
            self#queue_draw;
            true
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
        end
        | Pattern (view, mode) -> begin
          let color = Color.bubble_color view in
          ocamuse_context.base_colour := color;
          match event with
          | Key { code = Enter; _ } ->
            begin
              match !(ocamuse_context.display_mode) with
              | Pattern (Plain color, mode) ->
                ocamuse_context.display_mode := Pattern (Interline color, mode)
              | Pattern (Fretted color, mode) ->
                ocamuse_context.display_mode := Pattern (Plain color, mode)
              | Pattern (Interline color, mode) ->
                ocamuse_context.display_mode := Pattern (Fretted color, mode)
              | Flat _ -> assert false
            end;
            self#queue_draw;
            true
          | Key { code = Prev_page; _ } ->
            (* blue fretboard *)
            ocamuse_context.display_mode :=
              Pattern (update_color Color.rotate_to_prev view, mode);
            self#queue_draw;
            true
          | Key { code = Next_page; _ } ->
            (* green fretboard *)
            ocamuse_context.display_mode :=
              Pattern (update_color Color.rotate_to_next view, mode);
            self#queue_draw;
            true
          | Key { code = Backspace; _ } ->
            ocamuse_context.display_mode := Flat view;
            self#queue_draw;
            true
          | _ -> false
        end )
  end

class frame_board ocamuse_context =
  (* This function sets the location (and size) of the ctx inside which a fretboard is drawn *)
  let allocate_view_ctx ctx ocacont =
    let open Types in
    let open LTerm_draw in
    let open LTerm_geom in
    let allocate view =
      let { rows; cols } = size ctx in
      let center_row = rows / 2 in
      let center_col = cols / 2 in
      begin
        match view with
        | Plain _ ->
          let best_effort_print_width =
            let s_len = Array.length ocamuse_context.fretboard.(0) in
            ((s_len - 1) * 3) + 3
            (* number of frets minus 0th * the plain spacing by fret + length of fret 0 *)
          in
          let number_of_strings = Array.length ocacont.fretboard in
          let row1 = max 0 (center_row - (number_of_strings / 2)) in
          let row2 = min rows (row1 + number_of_strings + 2) in
          let col1 = max 0 (center_col - (best_effort_print_width / 2)) in
          let col2 = min cols (col1 + best_effort_print_width) in
          { row1; row2; col1; col2 }
        | Fretted _ ->
          let number_of_strings = Array.length ocacont.fretboard in
          let best_effort_print_width =
            let s_len = Array.length ocamuse_context.fretboard.(0) in
            ((s_len - 1) * 8) + 4
            (* number of frets minus 0th * the fretted spacing by fret + length of fret 0 *)
          in
          let row1 = max 0 (center_row - (number_of_strings / 2)) in
          let row2 = min rows (row1 + number_of_strings + 2) in
          let col1 = max 0 (center_col - (best_effort_print_width / 2)) in
          let col2 = min cols (col1 + best_effort_print_width) in
          { row1; row2; col1; col2 }
        | Interline _ ->
          let number_of_strings = Array.length ocacont.fretboard in
          let best_effort_print_height =
            (number_of_strings * 2) + 1 + 1
            (* number of strings * the interline spacing + top spacing after fret nb + bottom spacing after string 1 *)
          in
          let best_effort_print_width =
            let s_len = Array.length ocamuse_context.fretboard.(0) in
            ((s_len - 1) * 8) + 4
            (* number of frets minus 0th * the interline spacing by fret + length of fret 0 *)
          in
          let row1 =
            max 0 (center_row - ((best_effort_print_height + 1) / 2))
          in
          let row2 = min rows (row1 + best_effort_print_height) in
          let col1 = max 0 (center_col - (best_effort_print_width / 2)) in
          let col2 = min cols (col1 + best_effort_print_width) in
          { row1; row2; col1; col2 }
      end
    in
    match !(ocacont.display_mode) with
    | Flat view -> allocate view
    | Pattern (view, _mode) -> allocate view
  in

  (*
    this function sets the location and size of the frame used for the contour of the keyboard drawing
    this frame is drawn AFTER the keyboard, be wary of invalid coordinates that might overlap with the drawing ctx
   *)
  let allocate_frame allocation ocacont =
    let open Types in
    let open LTerm_geom in
    let check_view view =
      match view with
      | Plain _ ->
        { row1 = allocation.row1 - 1
        ; row2 = allocation.row2 - 1
        ; col1 = allocation.col1 - 1
        ; col2 = allocation.col2 + 1
        }
      | Fretted _ ->
        { row1 = allocation.row1 + 1
        ; row2 = allocation.row2 + 1
        ; col1 = allocation.col1 + 1
        ; col2 = allocation.col2 + 1
        }
      | Interline _ ->
        { row1 = allocation.row1 + 1
        ; row2 = allocation.row2
        ; col1 = allocation.col1
        ; col2 = allocation.col2
        }
    in
    match !(ocacont.display_mode) with
    | Flat view -> check_view view
    | Pattern (view, _mode) -> check_view view
  in

  object (self)
    inherit LTerm_widget.frame as super

    val mutable fretboard = new fretboard_widget ocamuse_context

    val ocamuse_context = ocamuse_context

    method! draw ctx focused =
      let open LTerm_draw in
      clear ctx;
      let open LTerm_style in
      (* outerframe *)
      draw_frame ctx self#allocation
        ~style:{ none with foreground = Some blue }
        Heavy;
      (* label print *)
      super#draw ctx focused;
      (* zone allocation for the fretboard *)
      let fb_allocation = allocate_view_ctx ctx ocamuse_context in
      let frame_allocation = allocate_frame fb_allocation ocamuse_context in
      let sub_ctx = sub ctx fb_allocation in
      Display.select_view sub_ctx ocamuse_context;
      (* frame autour de la fretboard selon la projection *)
      draw_frame ctx frame_allocation
        ~style:
          { none with background = Some default; foreground = Some default }
        Light

    initializer
      self#set fretboard;
      (* Assign the widget to the frame *)
      self#set_label ~alignment:H_align_center " Fretboard View ";
      self#on_event (fun event ->
        let open LTerm_key in
        match event with
        | Key { code = Escape; _ } ->
          (* Signal termination *)
          super#send_event event;
          false
        | Key { code = Prev_page; _ }
        | Key { code = Next_page; _ }
        | Key { code = Backspace; _ }
        | Key { code = Enter; _ } ->
          (* Propagate the event to the fretboard widget *)
          fretboard#send_event event;
          true
        | _ -> false )
  end

class labeled_frame s =
  object (self)
    inherit LTerm_widget.frame

    initializer self#set_label ~alignment:H_align_center s
  end

class main_box ocamuse_context wakener =
  object (self)
    inherit LTerm_widget.vbox

    val fb_frame = new frame_board ocamuse_context

    val top = new LTerm_widget.vbox

    val down = new labeled_frame "down"

    val up = new labeled_frame "up"

    val bottom = new LTerm_widget.hbox

    initializer
      (* put fretboard display in white box and on top *)
      top#add fb_frame;
      (* Add two widgets beneath *)
      bottom#add up;
      bottom#add down;
      (* display top and then bottom part of the screen *)
      self#add top;
      self#add bottom;
      self#on_event (fun event ->
        let open LTerm_key in
        match event with
        | Key { code = Escape; _ } ->
          Lwt.wakeup wakener ();
          false
        | Key { code = Prev_page; _ }
        | Key { code = Next_page; _ }
        | Key { code = Backspace; _ }
        | Key { code = Enter; _ } ->
          (* Propagate the event to the fretboard widget *)
          fb_frame#send_event event;
          true
        | _ -> false )
  end
