(** Layout calculation system for multi-view panels *)

open Panel

(** Screen size type *)
type screen_size = {
  width: int;
  height: int;
}

(** Rectangle type for panel bounds *)
type rect = {
  row: int;
  col: int;
  rows: int;
  cols: int;
}

(** Layout configuration *)
type layout_config = {
  min_fretboard_width: int;   (** Minimum width for main fretboard *)
  min_fretboard_height: int;  (** Minimum height for main fretboard *)
  min_panel_width: int;       (** Minimum width for side panels *)
  min_panel_height: int;      (** Minimum height for bottom panels *)
  panel_margin: int;          (** Margin between panels *)
  border_width: int;          (** Width of panel borders *)
}

(** Default layout configuration *)
let default_config = {
  min_fretboard_width = 80;
  min_fretboard_height = 20;
  min_panel_width = 30;
  min_panel_height = 10;
  panel_margin = 1;
  border_width = 1;
}

(** Calculate the bounds for a positioned panel *)
let calculate_panel_rect screen_size config position available_rect =
  match position with
  | Left pct ->
      let width = max config.min_panel_width
        ((available_rect.cols * pct) / 100) in
      {
        row = available_rect.row;
        col = available_rect.col;
        rows = available_rect.rows;
        cols = width;
      }

  | Right pct ->
      let width = max config.min_panel_width
        ((available_rect.cols * pct) / 100) in
      {
        row = available_rect.row;
        col = available_rect.col + available_rect.cols - width;
        rows = available_rect.rows;
        cols = width;
      }

  | Bottom pct ->
      let height = max config.min_panel_height
        ((available_rect.rows * pct) / 100) in
      {
        row = available_rect.row + available_rect.rows - height;
        col = available_rect.col;
        rows = height;
        cols = available_rect.cols;
      }

  | Floating r ->
      (* Ensure floating panel stays within screen bounds *)
      let clamped = {
        row = max 0 r.row;
        col = max 0 r.col;
        rows = min r.rows (screen_size.height - r.row);
        cols = min r.cols (screen_size.width - r.col);
      } in
      (* Clamp to screen bounds *)
      {
        clamped with
        rows = min clamped.rows (screen_size.height - clamped.row);
        cols = min clamped.cols (screen_size.width - clamped.col);
      }

(** Subtract a panel area from available rectangle *)
let subtract_rect available panel_rect position =
  match position with
  | Left _ ->
      {
        available with
        col = available.col + panel_rect.cols;
        cols = available.cols - panel_rect.cols;
      }

  | Right _ ->
      {
        available with
        cols = available.cols - panel_rect.cols;
      }

  | Bottom _ ->
      {
        available with
        rows = available.rows - panel_rect.rows;
      }

  | Floating _ ->
      (* Floating panels don't affect available space *)
      available

(** Calculate layout for all visible panels *)
let calculate_layout screen_size config panels =
  let visible_panels = List.filter (fun p -> p.visible) panels in

  (* Start with full screen as available space *)
  let initial_rect = {
    row = 0;
    col = 0;
    rows = screen_size.height;
    cols = screen_size.width;
  } in

  (* Process panels in order: Left, Right, Bottom, Floating *)
  let sorted_panels =
    let order = function
      | Left _ -> 0
      | Right _ -> 1
      | Bottom _ -> 2
      | Floating _ -> 3
    in
    List.sort (fun p1 p2 ->
      compare (order p1.position) (order p2.position)
    ) visible_panels
  in

  (* Calculate bounds for each panel *)
  let _, panel_rects =
    List.fold_left (fun (available, acc) panel ->
      let panel_rect = calculate_panel_rect screen_size config panel.position available in
      let new_available = subtract_rect available panel_rect panel.position in
      (new_available, (panel, panel_rect) :: acc)
    ) (initial_rect, []) sorted_panels
  in

  (* The remaining available space is for the main fretboard *)
  let fretboard_rect = snd (List.fold_left (fun (available, _) panel ->
    let panel_rect = calculate_panel_rect screen_size config panel.position available in
    let new_available = subtract_rect available panel_rect panel.position in
    (new_available, new_available)
  ) (initial_rect, initial_rect) sorted_panels) in

  (fretboard_rect, List.rev panel_rects)

(** Check if a new panel can fit in the current layout *)
let can_fit_panel screen_size config existing_panels new_panel =
  let all_panels = new_panel :: existing_panels in
  let fretboard_rect, _ = calculate_layout screen_size config all_panels in

  (* Check if fretboard still has minimum required size *)
  fretboard_rect.cols >= config.min_fretboard_width &&
  fretboard_rect.rows >= config.min_fretboard_height

(** Get the total space used by panels (excluding fretboard) *)
let total_panel_space screen_size config panels =
  let fretboard_rect, _ = calculate_layout screen_size config panels in
  let total_area = screen_size.width * screen_size.height in
  let fretboard_area = fretboard_rect.cols * fretboard_rect.rows in
  total_area - fretboard_area

(** Calculate optimal position for a new panel *)
let suggest_panel_position screen_size config existing_panels mode =
  (* Try positions in order of preference *)
  let positions = [
    Right 30;  (* Default to right sidebar *)
    Left 30;
    Bottom 25;
  ] in

  let test_panel = Panel.create mode (Right 30) in

  List.find_opt (fun pos ->
    let panel = { test_panel with position = pos } in
    can_fit_panel screen_size config existing_panels panel
  ) positions

(** Get human-readable layout statistics *)
let layout_stats screen_size config panels =
  let fretboard_rect, panel_rects = calculate_layout screen_size config panels in
  let total_area = screen_size.width * screen_size.height in
  let fretboard_area = fretboard_rect.cols * fretboard_rect.rows in
  let fretboard_pct = (fretboard_area * 100) / total_area in

  Printf.sprintf "Fretboard: %dx%d (%d%%), Panels: %d visible"
    fretboard_rect.cols fretboard_rect.rows fretboard_pct
    (List.length panel_rects)
