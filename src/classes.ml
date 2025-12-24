(** Main UI classes for ocamuse - Multi-view architecture *)

let update_color rotate =
  let open Types in
  function
  | Fretted c -> Fretted (rotate c)
  | Plain c -> Plain (rotate c)
  | Interline c -> Interline (rotate c)

let bubble_view =
  let open Types in
  function Flat view -> view | Pattern (view, _mode) -> view

(** Fretboard widget - handles fretboard-specific events *)
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
            ocamuse_context.base_colour :=
              Color.rotate_to_prev !(ocamuse_context.base_colour);
            ocamuse_context.display_mode :=
              Flat (update_color Color.rotate_to_prev @@ bubble_view !(ocamuse_context.display_mode));
            self#queue_draw;
            true
          | Key { code = Next_page; _ } ->
            ocamuse_context.base_colour :=
              Color.rotate_to_next !(ocamuse_context.base_colour);
            ocamuse_context.display_mode :=
              Flat (update_color Color.rotate_to_next @@ bubble_view !(ocamuse_context.display_mode));
            self#queue_draw;
            true
          | Key { code = Enter; _ } ->
            begin
              match !(ocamuse_context.display_mode) with
              | Flat (Plain color) -> ocamuse_context.display_mode := Flat (Interline color)
              | Flat (Fretted color) -> ocamuse_context.display_mode := Flat (Plain color)
              | Flat (Interline color) -> ocamuse_context.display_mode := Flat (Fretted color)
              | Pattern _ -> ()
            end;
            self#queue_draw;
            true
          | Key { code = Backspace; _ } ->
            ocamuse_context.display_mode := Flat view;
            self#queue_draw;
            true
          | _ -> false
        end
        | Pattern (view, mode) -> begin
          let color = Color.bubble_color view in
          ocamuse_context.base_colour := color;
          match event with
          | Key { code = Enter; _ } ->
            begin
              match !(ocamuse_context.display_mode) with
              | Pattern (Plain color, mode) -> ocamuse_context.display_mode := Pattern (Interline color, mode)
              | Pattern (Fretted color, mode) -> ocamuse_context.display_mode := Pattern (Plain color, mode)
              | Pattern (Interline color, mode) -> ocamuse_context.display_mode := Pattern (Fretted color, mode)
              | Flat _ -> ()
            end;
            self#queue_draw;
            true
          | Key { code = Prev_page; _ } ->
            ocamuse_context.display_mode := Pattern (update_color Color.rotate_to_prev view, mode);
            self#queue_draw;
            true
          | Key { code = Next_page; _ } ->
            ocamuse_context.display_mode := Pattern (update_color Color.rotate_to_next view, mode);
            self#queue_draw;
            true
          | Key { code = Backspace; _ } ->
            ocamuse_context.display_mode := Flat view;
            self#queue_draw;
            true
          | _ -> false
        end)
  end

(** Fretboard frame - contains fretboard widget with frame and label *)
class frame_board ocamuse_context =
  let allocate_view_ctx ctx ocacont =
    let open Types in
    let open LTerm_draw in
    let open LTerm_geom in
    let allocate view =
      let { rows; cols } = size ctx in
      let center_row = rows / 2 in
      let center_col = cols / 2 in
      match view with
      | Plain _ ->
        let best_effort_print_width =
          let s_len = Array.length ocamuse_context.fretboard.notes.(0) in
          ((s_len - 1) * 3) + 3
        in
        let number_of_strings = Array.length ocacont.fretboard.notes in
        let row1 = max 0 (center_row - (number_of_strings / 2)) in
        let row2 = min rows (row1 + number_of_strings + 2) in
        let col1 = max 0 (center_col - (best_effort_print_width / 2)) in
        let col2 = min cols (col1 + best_effort_print_width) in
        { row1; row2; col1; col2 }
      | Fretted _ ->
        let number_of_strings = Array.length ocacont.fretboard.notes in
        let best_effort_print_width =
          let s_len = Array.length ocamuse_context.fretboard.notes.(0) in
          ((s_len - 1) * 8) + 4
        in
        let row1 = max 0 (center_row - (number_of_strings / 2)) in
        let row2 = min rows (row1 + number_of_strings + 2) in
        let col1 = max 0 (center_col - (best_effort_print_width / 2)) in
        let col2 = min cols (col1 + best_effort_print_width) in
        { row1; row2; col1; col2 }
      | Interline _ ->
        let number_of_strings = Array.length ocacont.fretboard.notes in
        let best_effort_print_height = (number_of_strings * 2) + 1 + 1 in
        let best_effort_print_width =
          let s_len = Array.length ocamuse_context.fretboard.notes.(0) in
          ((s_len - 1) * 8) + 4
        in
        let row1 = max 0 (center_row - ((best_effort_print_height + 1) / 2)) in
        let row2 = min rows (row1 + best_effort_print_height) in
        let col1 = max 0 (center_col - (best_effort_print_width / 2)) in
        let col2 = min cols (col1 + best_effort_print_width) in
        { row1; row2; col1; col2 }
    in
    match !(ocacont.display_mode) with
    | Flat view -> allocate view
    | Pattern (view, _mode) -> allocate view
  in

  let allocate_frame allocation ocacont =
    let open Types in
    let open LTerm_geom in
    let check_view view =
      match view with
      | Plain _ ->
        { row1 = allocation.row1 - 1; row2 = allocation.row2 - 1;
          col1 = allocation.col1 - 1; col2 = allocation.col2 + 1 }
      | Fretted _ ->
        { row1 = allocation.row1 + 1; row2 = allocation.row2 + 1;
          col1 = allocation.col1 + 1; col2 = allocation.col2 + 1 }
      | Interline _ ->
        { row1 = allocation.row1 + 1; row2 = allocation.row2;
          col1 = allocation.col1; col2 = allocation.col2 }
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
      let open LTerm_style in
      clear ctx;
      draw_frame ctx self#allocation ~style:{ none with foreground = Some blue } Heavy;
      super#draw ctx focused;
      let fb_allocation = allocate_view_ctx ctx ocamuse_context in
      let frame_allocation = allocate_frame fb_allocation ocamuse_context in
      let sub_ctx = sub ctx fb_allocation in
      Display.select_view sub_ctx ocamuse_context;
      draw_frame ctx frame_allocation
        ~style:{ none with background = Some default; foreground = Some default } Light

    initializer
      self#set fretboard;
      self#set_label ~alignment:H_align_center " Fretboard ";
      self#on_event (fun event ->
        let open LTerm_key in
        match event with
        | Key { code = Escape; _ } -> super#send_event event; false
        | Key { code = Prev_page; _ }
        | Key { code = Next_page; _ }
        | Key { code = Backspace; _ }
        | Key { code = Enter; _ } -> fretboard#send_event event; true
        | _ -> false)
  end

(** Status bar widget - shows current mode and key info *)
class status_bar (ocamuse_context : Types.ocamuse_structure) app_state =
  object
    inherit LTerm_widget.t "status_bar"

    method! size_request = { LTerm_geom.rows = 1; cols = 80 }

    method! draw ctx _focused =
      let open LTerm_draw in
      let open LTerm_style in
      let open LTerm_geom in
      clear ctx;
      let { cols; _ } = size ctx in

      (* Mode indicator *)
      let mode_str = App_state.mode_name app_state in
      let root_str = Pp.NOTES.FMT.sprint_note ocamuse_context.root_note in
      let scale_name = Display.scale_name ocamuse_context.scale in
      let status = Printf.sprintf " [%s] %s %s | 1-7:Diatonic c:Lookup t:Key m:Scale ?:Help Esc:Quit "
        mode_str root_str scale_name
      in
      let len = min (String.length status) cols in
      for i = 0 to len - 1 do
        LTerm_draw.draw_char ctx 0 i (Zed_char.unsafe_of_char status.[i])
          ~style:{ none with foreground = Some black; background = Some white }
      done
  end

(** Main application container - Multi-view architecture *)
class main_box ocamuse_context exit push_layer pop_layer =
  object (self)
    inherit LTerm_widget.vbox

    val app_state = App_state.make ocamuse_context
    val fb_frame = new frame_board ocamuse_context

    (* Main content area - hbox for side panel + fretboard *)
    val main_content = new LTerm_widget.hbox

    (* Side panel frame for multi-view *)
    val side_panel_frame = new LTerm_widget.frame

    (* Status bar at bottom *)
    val mutable status_bar_widget = None

    (* Multi-view state *)
    val mutable harmony_widget : LTerm_widget.t option = None
    val mutable side_panel_visible = false

    (* Bottom selector panel *)
    val selector_panel_frame = new LTerm_widget.frame
    val mutable selector_visible = false

    method private show_selector_panel (widget : LTerm_widget.t) title =
      if not selector_visible then begin
        (* Remove status bar, add selector, re-add status bar *)
        (match status_bar_widget with Some sb -> self#remove (sb :> LTerm_widget.t) | None -> ());
        selector_panel_frame#set widget;
        selector_panel_frame#set_label ~alignment:LTerm_geom.H_align_center title;
        self#add ~expand:false (selector_panel_frame :> LTerm_widget.t);
        (match status_bar_widget with Some sb -> self#add ~expand:false (sb :> LTerm_widget.t) | None -> ());
        selector_visible <- true
      end else begin
        (* Just update the widget content *)
        selector_panel_frame#set widget
      end

    method private hide_selector_panel =
      if selector_visible then begin
        self#remove selector_panel_frame;
        selector_visible <- false
      end

    method private show_harmony_panel =
      if not side_panel_visible then begin
        let harmony_content : Multi_view.Panel.harmony_content = {
          current_key = None;
          current_chord = None;
          suggestions = [];
          progression = [];
          selected_suggestion = None;
        } in
        let panel : Multi_view.Panel.panel = {
          id = "harmony_explorer";
          mode = Multi_view.Panel.HarmonyExplorer;
          content = Multi_view.Panel.HarmonyContent harmony_content;
          position = Multi_view.Panel.Left 40;
          sync_mode = Multi_view.Panel.Watch;
          visible = true;
          focused = true;
        } in
        let widget = Multi_view_views.Harmony_view.create_widget panel in
        harmony_widget <- Some (widget :> LTerm_widget.t);
        main_content#remove fb_frame;
        side_panel_frame#set (widget :> LTerm_widget.t);
        side_panel_frame#set_label ~alignment:LTerm_geom.H_align_center " Harmony Explorer ";
        main_content#add ~expand:false side_panel_frame;
        main_content#add ~expand:true fb_frame;
        side_panel_visible <- true
      end

    method private hide_harmony_panel =
      if side_panel_visible then begin
        main_content#remove side_panel_frame;
        side_panel_visible <- false;
        harmony_widget <- None
      end

    initializer
      (* Create status bar *)
      let sb = new status_bar ocamuse_context app_state in
      status_bar_widget <- Some sb;

      (* Main content starts with just the fretboard *)
      main_content#add ~expand:true fb_frame;

      (* Layout: main content area + status bar *)
      self#add ~expand:true main_content;
      self#add ~expand:false (sb :> LTerm_widget.t);

      self#on_event (fun event ->
        let open LTerm_key in
        let open LTerm_event in

        match !(app_state.mode) with
        | App_state.Normal -> begin
          match event with
          | Key { code = Escape; _ } ->
            exit ();
            true

          | Key { code = Char c; control = false; _ } when Uchar.to_int c = Char.code 'v' ->
            (* Toggle multi-view panel *)
            App_state.enter_multi_view app_state;
            self#show_harmony_panel;
            self#queue_draw;
            true

          | Key { code = Char c; control = false; _ } when Uchar.to_int c = Char.code 'h' || Uchar.to_int c = Char.code '?' ->
            (* Show help modal *)
            if !(app_state.keybindings_modal_visible) then begin
              App_state.toggle_keybindings_modal app_state;
              pop_layer ();
              true
            end else begin
              App_state.toggle_keybindings_modal app_state;
              let close_fn () =
                App_state.toggle_keybindings_modal app_state;
                pop_layer ()
              in
              let modal = new Widgets.keybindings_modal close_fn in
              (push_layer modal) ();
              true
            end

          | Key { code = Char c; _ } when Uchar.to_int c = Char.code 't' ->
            App_state.enter_tonality_selection app_state;
            let tstate = match !(app_state.mode) with
              | App_state.TonalitySelection ts -> ts
              | _ -> failwith "Expected TonalitySelection"
            in
            let widget = new Selectors.tonality_selector_widget tstate app_state in
            self#show_selector_panel (widget :> LTerm_widget.t) " Select Tonality ";
            self#queue_draw;
            true

          | Key { code = Char c; _ } when Uchar.to_int c = Char.code 'u' ->
            App_state.enter_tuning_selection app_state;
            let tstate = match !(app_state.mode) with
              | App_state.TuningSelection ts -> ts
              | _ -> failwith "Expected TuningSelection"
            in
            let widget = new Selectors.tuning_selector_widget tstate app_state in
            self#show_selector_panel (widget :> LTerm_widget.t) " Select Tuning ";
            self#queue_draw;
            true

          | Key { code = Char c; _ } when Uchar.to_int c = Char.code 'm' ->
            (* Cycle through scales in current category *)
            let current_category = Display.category_of_scale ocamuse_context.scale in
            let scales = Display.scales_in_category current_category in
            let rec find_idx i = function
              | [] -> 0
              | s :: _ when s = ocamuse_context.scale -> i
              | _ :: rest -> find_idx (i + 1) rest
            in
            let current_idx = find_idx 0 scales in
            let next_idx = (current_idx + 1) mod List.length scales in
            let next_scale = List.nth scales next_idx in
            ocamuse_context.scale <- next_scale;
            (* Update display mode and highlight source *)
            ocamuse_context.highlight_source <- Types.Tonality (next_scale, ocamuse_context.root_note);
            let view = match !(ocamuse_context.display_mode) with
              | Types.Flat v -> v | Types.Pattern (v, _) -> v
            in
            ocamuse_context.display_mode := Types.Pattern (view, next_scale);
            self#queue_draw;
            true

          | Key { code = Char c; _ } when Uchar.to_int c = Char.code 'k' ->
            (* Enter theme selection mode *)
            App_state.enter_theme_selection app_state;
            let tstate = match !(app_state.mode) with
              | App_state.ThemeSelection ts -> ts
              | _ -> failwith "Expected ThemeSelection"
            in
            let widget = new Selectors.theme_selector_widget tstate app_state in
            self#show_selector_panel (widget :> LTerm_widget.t) " Select Theme ";
            self#queue_draw;
            true

          | Key { code = Char c; _ } when Uchar.to_int c = Char.code 'c' ->
            (* Enter chord lookup mode *)
            App_state.enter_chord_lookup app_state;
            let cstate = match !(app_state.mode) with
              | App_state.ChordLookup cs -> cs
              | _ -> failwith "Expected ChordLookup"
            in
            let widget = new Selectors.chord_lookup_widget cstate app_state in
            self#show_selector_panel (widget :> LTerm_widget.t) " Chord Lookup ";
            self#queue_draw;
            true

          | Key { code = Char c; _ } when Uchar.to_int c = Char.code 'r' ->
            (* Reset to tonality highlighting *)
            ocamuse_context.highlight_source <- Types.Tonality (ocamuse_context.scale, ocamuse_context.root_note);
            let view = match !(ocamuse_context.display_mode) with
              | Types.Flat v -> v | Types.Pattern (v, _) -> v
            in
            ocamuse_context.display_mode := Types.Pattern (view, ocamuse_context.scale);
            self#queue_draw;
            true

          | Key { code = Char c; _ } when Uchar.to_int c >= Char.code '1' && Uchar.to_int c <= Char.code '7' ->
            (* Quick select diatonic chord by degree (1-7) *)
            let degree = Uchar.to_int c - Char.code '0' in
            let diatonic = Display.diatonic_triads ocamuse_context.scale ocamuse_context.root_note in
            if List.length diatonic >= degree then begin
              let (note, _triad, chord, _deg) = List.nth diatonic (degree - 1) in
              ocamuse_context.highlight_source <- Types.Chord (note, chord);
              self#queue_draw;
              true
            end else false

          | Key { code = Char c; _ } when Uchar.to_int c = Char.code 'd' ->
            App_state.toggle_debug app_state;
            self#queue_draw;
            true

          | Key { code = Prev_page; _ }
          | Key { code = Next_page; _ }
          | Key { code = Backspace; _ }
          | Key { code = Enter; _ } ->
            fb_frame#send_event event;
            true

          | _ -> false
        end

        | App_state.MultiView _ ->
          (match event with
          | Key { code = Escape; _ } ->
            self#hide_harmony_panel;
            App_state.return_to_normal app_state;
            self#queue_draw;
            true
          | Key { code = Char c; _ } when Uchar.to_int c = Char.code 'v' ->
            self#hide_harmony_panel;
            App_state.return_to_normal app_state;
            self#queue_draw;
            true
          | Key { code = Prev_page; _ }
          | Key { code = Next_page; _ }
          | Key { code = Backspace; _ }
          | Key { code = Enter; _ } ->
            fb_frame#send_event event;
            true
          | _ -> false)

        | App_state.TonalitySelection _ ->
          if Selectors.handle_tonality_input app_state event then begin
            (* Check if we returned to normal mode *)
            (match !(app_state.mode) with
            | App_state.Normal ->
                self#hide_selector_panel
            | App_state.TonalitySelection new_tstate ->
                (* Refresh widget with updated state *)
                let widget = new Selectors.tonality_selector_widget new_tstate app_state in
                self#show_selector_panel (widget :> LTerm_widget.t) " Select Tonality "
            | _ -> ());
            self#queue_draw;
            true
          end else false

        | App_state.TuningSelection _ ->
          if Selectors.handle_tuning_input app_state event then begin
            (* Check if we returned to normal mode *)
            (match !(app_state.mode) with
            | App_state.Normal ->
                self#hide_selector_panel
            | App_state.TuningSelection new_tstate ->
                (* Refresh widget with updated state *)
                let widget = new Selectors.tuning_selector_widget new_tstate app_state in
                self#show_selector_panel (widget :> LTerm_widget.t) " Select Tuning "
            | _ -> ());
            self#queue_draw;
            true
          end else false

        | App_state.ThemeSelection _ ->
          if Selectors.handle_theme_input app_state event then begin
            (* Check if we returned to normal mode *)
            (match !(app_state.mode) with
            | App_state.Normal ->
                self#hide_selector_panel
            | App_state.ThemeSelection new_tstate ->
                (* Refresh widget with updated state - theme preview is automatic *)
                let widget = new Selectors.theme_selector_widget new_tstate app_state in
                self#show_selector_panel (widget :> LTerm_widget.t) " Select Theme "
            | _ -> ());
            self#queue_draw;
            true
          end else false

        | App_state.ChordLookup _ ->
          if Selectors.handle_chord_lookup_input app_state event then begin
            (* Check if we returned to normal mode *)
            (match !(app_state.mode) with
            | App_state.Normal ->
                self#hide_selector_panel
            | App_state.ChordLookup new_cstate ->
                (* Refresh widget with updated state *)
                let widget = new Selectors.chord_lookup_widget new_cstate app_state in
                self#show_selector_panel (widget :> LTerm_widget.t) " Chord Lookup "
            | _ -> ());
            self#queue_draw;
            true
          end else false
      )
  end
