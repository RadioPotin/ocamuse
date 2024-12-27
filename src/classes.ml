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
      LTerm_draw.clear ctx;
      Display.select_view ctx ocamuse_context
      (*
      let open LTerm_draw in
      let open LTerm_style in
      let open LTerm_geom in
      let allocation = { row1 = 0; col1 = 0; row2 = 6; col2 = 35 } in
      let ctx = LTerm_draw.sub ctx allocation in
      let style = { none with foreground = Some white; background = Some white } in
      fill_style ctx style
      *)

    initializer
      self#on_event (fun event ->
        let open LTerm_event in
        let open Types in
        match !(ocamuse_context.display_mode) with
        | Flat _ -> begin
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

class frame_board = object (self)
  inherit LTerm_widget.frame

  initializer
    self#set_label ~alignment:LTerm_geom.H_align_center " Fretboard view "
end

class labeled_frame s = object (self)
  inherit LTerm_widget.frame
  val label = new LTerm_widget.label s
  initializer
    self#set label
end

class main_box ocamuse_context wakener =
  object (self)
    inherit LTerm_widget.vbox

    val fretboard_widget = new fretboard_widget ocamuse_context

    val fb_frame = new frame_board

    val bottom = new LTerm_widget.vbox

    val up = new labeled_frame "down"
    val right = new labeled_frame "up"

    val top = new LTerm_widget.vbox

    initializer
      (* put fretboard display in white box and on top *)
      fb_frame#set fretboard_widget;
      top#add fb_frame;
      (* Add two widgets beneath *)
      bottom#add right;
      bottom#add up;
      (* display top and then bottom part of the screen *)
      self#add top;
      self#add bottom;
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
        | Key { code = Backspace; _ }
        | Key { code = Enter; _ } ->
          (* Propagate the event to the fretboard widget *)
          fretboard_widget#send_event event;
          true
        | _ -> false )
  end
