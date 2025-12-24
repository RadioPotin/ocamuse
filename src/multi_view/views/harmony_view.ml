(** Harmony Explorer view - intelligent chord suggestions and progression builder *)

open Multi_view.Panel
open Music_theory

(** Update harmony content with new suggestions *)
let update_suggestions content =
  match content.current_key with
  | None -> content
  | Some scale ->
      let suggestions =
        Chord_suggestions.get_top_suggestions 6 scale content.current_chord
      in
      { content with suggestions }

(** Select a suggestion by index *)
let select_suggestion content index =
  if index >= 0 && index < List.length content.suggestions then
    { content with selected_suggestion = Some index }
  else
    content

(** Apply selected suggestion to current chord *)
let apply_selected_suggestion content =
  match content.selected_suggestion with
  | None -> content
  | Some idx ->
      match List.nth_opt content.suggestions idx with
      | Some (chord, _, _) ->
          let new_progression = content.progression @ [chord] in
          { content with
            current_chord = Some chord;
            progression = new_progression;
            selected_suggestion = None;
          }
          |> update_suggestions
      | None -> content

(** Add current chord to progression *)
let add_to_progression content =
  match content.current_chord with
  | None -> content
  | Some chord ->
      let new_progression = content.progression @ [chord] in
      { content with progression = new_progression }

(** Clear progression *)
let clear_progression content =
  { content with progression = [] }

(** Remove last chord from progression *)
let pop_progression content =
  match List.rev content.progression with
  | [] -> content
  | _ :: rest -> { content with progression = List.rev rest }

(** Set the current key *)
let set_key content scale =
  { content with current_key = Some scale }
  |> update_suggestions

(** Set the current chord *)
let set_chord content chord =
  { content with current_chord = Some chord }
  |> update_suggestions

(** Helper to draw a text line *)
let draw_text_line ctx row_pos col_pos text style =
  let open LTerm_geom in
  let { cols; _ } = LTerm_draw.size ctx in
  let available = max 0 (cols - col_pos) in
  let len = min (String.length text) available in
  for i = 0 to len - 1 do
    LTerm_draw.draw_char ctx row_pos (col_pos + i)
      (Zed_char.unsafe_of_char text.[i]) ~style
  done

(** Render the harmony view *)
let render ctx content =
  let open LTerm_draw in
  let open LTerm_style in
  let open LTerm_geom in
  clear ctx;

  let { rows; cols } = size ctx in
  if rows < 10 || cols < 40 then () (* Too small to render *)
  else
    let row = ref 0 in

    (* Helper to draw a line and advance *)
    let draw_line text style =
      if !row < rows then begin
        draw_text_line ctx !row 1 text style;
        incr row
      end
    in

    (* Title *)
    draw_line "HARMONY EXPLORER" { none with bold = Some true; foreground = Some lcyan };
    incr row;

    (* Current key *)
    (match content.current_key with
    | Some scale ->
        let key_str = Scales.Scale.to_string scale in
        draw_line (Printf.sprintf "Key: %s" key_str) { none with foreground = Some lyellow }
    | None ->
        draw_line "Key: (none - press 't' to select)" { none with foreground = Some lblack });

    (* Current chord *)
    (match content.current_chord with
    | Some chord ->
        let chord_str = Harmony.Chord.to_string chord in
        draw_line (Printf.sprintf "Current: %s" chord_str) { none with foreground = Some lgreen }
    | None ->
        draw_line "Current: (none)" { none with foreground = Some lblack });

    incr row;

    (* Chord suggestions *)
    if List.length content.suggestions > 0 then begin
      draw_line "SUGGESTIONS:" { none with bold = Some true; foreground = Some lmagenta };
      incr row;

      List.iteri (fun i (chord, description, strength) ->
        if !row < rows - 3 then begin
          let chord_str = Harmony.Chord.to_string chord in
          let strength_str = String.make strength '*' in
          let selected = match content.selected_suggestion with
            | Some idx when idx = i -> "> "
            | _ -> "  "
          in

          let suggestion_text = Printf.sprintf "%s%d. %s - %s %s"
            selected (i + 1) chord_str description strength_str
          in

          let style =
            if Some i = content.selected_suggestion then
              { none with foreground = Some lwhite; bold = Some true; background = Some blue }
            else
              { none with foreground = Some lwhite }
          in

          draw_line suggestion_text style;
        end
      ) content.suggestions;
    end else begin
      draw_line "No suggestions available" { none with foreground = Some lblack };
    end;

    incr row;

    (* Progression *)
    if List.length content.progression > 0 then begin
      draw_line "PROGRESSION:" { none with bold = Some true; foreground = Some lcyan };

      let prog_str = String.concat " -> "
        (List.map Harmony.Chord.to_string content.progression)
      in
      draw_line prog_str { none with foreground = Some lgreen };
    end;

    (* Help text at bottom *)
    let help_row = rows - 3 in
    if help_row > !row then begin
      row := help_row;
      draw_line "Keys: ↑/↓ Navigate  Enter Apply  1-6 Quick Select"
        { none with foreground = Some lblue };
      incr row;
      draw_line "      c Clear Progression  u Undo Last"
        { none with foreground = Some lblue };
    end

(** Create a LTerm widget for harmony view *)
let create_widget panel =
  match panel.content with
  | HarmonyContent content ->
      (* Extract width from panel position *)
      let panel_width = match panel.position with
        | Left pct -> max 40 (pct * 2)  (* Convert percentage to chars, min 40 *)
        | Right pct -> max 40 (pct * 2)
        | Bottom _ -> 80
        | Floating rect -> rect.cols
      in
      object
        inherit LTerm_widget.t "harmony_view"

        val mutable content = content

        method! can_focus = true

        method! size_request = { LTerm_geom.rows = 20; cols = panel_width }

        method! draw ctx _focused =
          render ctx content

        method update_content new_content =
          content <- new_content

        method get_content = content
      end

  | _ ->
      failwith "harmony_view.create_widget called with non-harmony content"

(** Handle keyboard input for harmony view *)
let handle_key content key =
  match key with
  | LTerm_key.Up ->
      let current = Option.value content.selected_suggestion ~default:0 in
      let new_idx = max 0 (current - 1) in
      Some (select_suggestion content new_idx)

  | LTerm_key.Down ->
      let current = Option.value content.selected_suggestion ~default:(-1) in
      let max_idx = List.length content.suggestions - 1 in
      let new_idx = min max_idx (current + 1) in
      Some (select_suggestion content new_idx)

  | LTerm_key.Enter ->
      Some (apply_selected_suggestion content)

  | LTerm_key.Char c when Uchar.equal c (Uchar.of_char 'c') ->
      Some (clear_progression content)

  | LTerm_key.Char c when Uchar.equal c (Uchar.of_char 'u') ->
      Some (pop_progression content)

  | LTerm_key.Char c when Uchar.to_int c >= Uchar.to_int (Uchar.of_char '1') &&
                          Uchar.to_int c <= Uchar.to_int (Uchar.of_char '6') ->
      let idx = Uchar.to_int c - Uchar.to_int (Uchar.of_char '1') in
      if idx < List.length content.suggestions then
        Some (select_suggestion content idx |> apply_selected_suggestion)
      else
        None

  | _ -> None
