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

    method! draw ctx _focused =
      Display.select_view ctx ocamuse_context

    initializer
      self#on_event (fun event ->
        let open LTerm_event in
        let open Types in
        match !(ocamuse_context.display_mode) with
        | Flat view -> begin
            match event with
            | Key { code = Up; _ } ->
              (* Blue fretboard *)
              ocamuse_context.display_mode :=
                Flat (Fretted !(ocamuse_context.base_colour));
              self#queue_draw;
              true
            | Key { code = Left; _ } ->
              (* Green fretboard *)
              ocamuse_context.display_mode :=
                Flat (Plain !(ocamuse_context.base_colour));
              self#queue_draw;
              true
            | Key { code = Right; _ } ->
              (* Interline fretboard *)
              ocamuse_context.display_mode :=
                Flat (Interline !(ocamuse_context.base_colour));
              self#queue_draw;
              true
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
              (* Switch to Pattern view *)
              begin
                match !(ocamuse_context.display_mode) with
                | Flat mode ->
                  ocamuse_context.display_mode := Pattern (mode, C_mode);
                  self#queue_draw;
                  true
                | _ -> true
              end
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
            | Key { code = Up; _ } ->
              (* blue fretboard *)
              ocamuse_context.display_mode := Pattern (Fretted color, mode);
              self#queue_draw;
              true
            | Key { code = Left; _ } ->
              (* green fretboard *)
              ocamuse_context.display_mode := Pattern (Plain color, mode);
              self#queue_draw;
              true
            | Key { code = Right; _ } ->
              (* green fretboard *)
              ocamuse_context.display_mode := Pattern (Interline color, mode);
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
            | Key { code = Enter; _ } ->
              ocamuse_context.display_mode := Pattern (view, mode);
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

  let allocate_view_ctx ctx ocacont =
    let open Types in
    let open LTerm_draw in
    let open LTerm_geom in
    let allocate view =
      let {rows; cols} = size ctx in
      let center_row = rows / 2 in
      let center_col = cols / 2 in
      let number_of_strings = Array.length ocacont.fretboard in
      let best_effort_print_width =
        let s_len =
          Array.length ocamuse_context.fretboard.(0)
        in
        (s_len - 1 ) * 8 + 4
      in
      begin
        match view with
        | Plain _ ->
          let number_of_strings = Array.length ocacont.fretboard in
          let row1 = max 0 (center_row - number_of_strings / 2) in
          let row2 = min rows (row1 + number_of_strings) in
          let col1 = max 0 (center_col - 35 / 2) in
          let col2 = min cols (col1 + 35) in
          {
            row1;
            row2;
            col1;
            col2;
          }
        | Fretted _ ->
          let row1 = max 0 (center_row - number_of_strings / 2) in
          let row2 = min rows (row1 + number_of_strings + 2) in
          let col1 = max 0 (center_col - best_effort_print_width / 2) in
          let col2 = min cols (col1 + best_effort_print_width) in
          {
            row1;
            row2;
            col1;
            col2;
          }
        | Interline _ ->
          let best_effort_print_height =
            let height =
              number_of_strings * 2 + 2
            in
            height (* 1 for bottom padding row + 1 for frets *)
          in
          let row1 = max 0 (center_row - best_effort_print_height / 2) in
          let row2 = min rows (row1 + best_effort_print_height) in
          let col1 = max 0 (center_col - best_effort_print_width / 2) in
          let col2 = min cols (col1 + best_effort_print_width) in
          {
            row1;
            row2;
            col1;
            col2;
          }
      end
    in
    match !(ocacont.display_mode) with
    | Flat view ->
      allocate view
    | Pattern (view, _mode) ->
      allocate view
  in

  let allocate_frame allocation ocacont =
    let open Types in
    let open LTerm_geom in
    let check_view view =
      match view with
      | Plain _ ->
        {
          row1 = allocation.row1 - 1;
          row2 = allocation.row2 + 1;
          col1 = allocation.col1 - 1;
          col2 = allocation.col2 + 2
        }
      | Fretted _ ->
        {
          row1 = allocation.row1 + 1;
          row2 = allocation.row2 + 1;
          col1 = allocation.col1 + 1;
          col2 = allocation.col2 + 1
        }
      | Interline _ ->
        {
          row1 = allocation.row1 + 1;
          row2 = allocation.row2;
          col1 = allocation.col1;
          col2 = allocation.col2
        }
    in
    match !(ocacont.display_mode) with
    | Flat view ->
      check_view view
    | Pattern (view, _mode) ->
      check_view view

  in

  object (self)
    inherit LTerm_widget.frame as super

    val mutable fretboard = new LTerm_widget.t "fretboard_widget"

    val ocamuse_context = ocamuse_context

    method set_fretboard fb =
      fretboard <- fb;
      self#set fb (* Assign the widget to the frame *)

    method! draw ctx focused =
      let open LTerm_style in
      let open LTerm_draw in
      clear ctx;
      (* outerframe *)
      draw_frame ctx self#allocation  ~style:{none with foreground = Some blue} Light;
      (* zone allocation for the fretboard *)
      let allocation = allocate_view_ctx ctx ocamuse_context in
      let sub_ctx = sub ctx allocation in
      fretboard#draw sub_ctx focused;
      (* frame autour de la fretboard selon la projection *)
      let allocation = allocate_frame allocation ocamuse_context in
      draw_frame ctx allocation ~style:{none with foreground = Some white} Light

    initializer
      self#set_label ~alignment:H_align_center " fretboard view ";
      self#on_event (fun event ->
        let open LTerm_key in
        match event with
        | Key { code = Escape; _ } ->
          (* Signal termination *)
          super#send_event event;
          false
        | Key { code = Up; _ }
        | Key { code = Left; _ }
        | Key { code = Right; _ }
        | Key { code = Prev_page; _ }
        | Key { code = Next_page; _ }
        | Key { code = Backspace; _ }
        | Key { code = Enter; _ } ->
          (* Propagate the event to the fretboard widget *)
          fretboard#send_event event;
          true
        | _ -> false )
  end


class labeled_frame s = object (self)
  inherit LTerm_widget.frame
  val label = new LTerm_widget.label s
  initializer
    self#set_label ~alignment:H_align_center s;
    self#set label
end

class main_box ocamuse_context wakener =
  object (self)
    inherit LTerm_widget.vbox

    val fretboard_widget = new fretboard_widget ocamuse_context
    val fb_frame = new frame_board ocamuse_context
    val top = new LTerm_widget.vbox

    val down = new labeled_frame "down"
    val up = new labeled_frame "up"
    val bottom = new LTerm_widget.vbox

    initializer
      (* put fretboard display in white box and on top *)
      fb_frame#set_fretboard fretboard_widget;
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
        | Key { code = Up; _ }
        | Key { code = Left; _ }
        | Key { code = Right; _ }
        | Key { code = Prev_page; _ }
        | Key { code = Next_page; _ }
        | Key { code = Backspace; _ }
        | Key { code = Enter; _ } ->
          (* Propagate the event to the fretboard widget *)
          fb_frame#send_event event;
          true
        | _ -> false )
  end
