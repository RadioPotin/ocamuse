(** Mini fretboard widget for panel previews *)

open LTerm_draw
open LTerm_style
open LTerm_geom
open Music_theory

(** Helper to transpose a note by semitones *)
let transpose_by_semitones note semitones =
  let pitch = Core.Note.to_pitch note in
  let pitch_int = (pitch :> int) in
  let new_pitch_int = (pitch_int + semitones) mod 12 in
  let new_pitch_int = if new_pitch_int < 0 then new_pitch_int + 12 else new_pitch_int in
  let new_pitch = Core.Pitch.of_int new_pitch_int in
  Core.Note.of_pitch new_pitch ~prefer:`Sharp

(** Mini fretboard configuration *)
type config = {
  frets: int;        (** Number of frets to show (typically 5-7) *)
  strings: int;      (** Number of strings *)
  width: int;        (** Widget width in columns *)
  height: int;       (** Widget height in rows *)
  show_note_names: bool;  (** Show note names or just dots *)
}

(** Default mini fretboard config *)
let default_config = {
  frets = 5;
  strings = 6;
  width = 25;
  height = 10;
  show_note_names = false;
}

(** Highlighted note on mini fretboard *)
type highlight = {
  string: int;  (** String number (0-based from top) *)
  fret: int;    (** Fret number (0 = open) *)
  note: Core.Note.t;
  color: LTerm_style.color;
  label: string option;  (** Optional label (e.g., "R" for root) *)
}

(** Draw a simplified fretboard grid *)
let draw_grid ctx config =
  let size = LTerm_draw.size ctx in
  let rows = size.LTerm_geom.rows in
  let cols = size.LTerm_geom.cols in

  (* Calculate spacing *)
  let fret_width = (cols - 2) / config.frets in
  let string_spacing = if config.strings > 1 then
      (rows - 2) / (config.strings - 1)
    else 1
  in

  (* Draw horizontal strings *)
  for s = 0 to config.strings - 1 do
    let row = 1 + (s * string_spacing) in
    for col = 1 to cols - 2 do
      draw_char ctx row col (Zed_char.unsafe_of_char '-')
        ~style:{ none with foreground = Some lblack }
    done
  done;

  (* Draw vertical frets *)
  for f = 0 to config.frets do
    let col = 1 + (f * fret_width) in
    for s = 0 to config.strings - 1 do
      let row = 1 + (s * string_spacing) in
      draw_char ctx row col (Zed_char.unsafe_of_char '|')
        ~style:{ none with foreground = Some lwhite }
    done
  done;

  (* Draw nut (first fret is thicker) *)
  for s = 0 to config.strings - 1 do
    let row = 1 + (s * string_spacing) in
    draw_char ctx row 1 (Zed_char.unsafe_of_char '|')
      ~style:{ none with foreground = Some lwhite; bold = Some true }
  done

(** Draw highlights on the fretboard *)
let draw_highlights ctx config highlights =
  let size = LTerm_draw.size ctx in
  let rows = size.LTerm_geom.rows in
  let cols = size.LTerm_geom.cols in

  let fret_width = (cols - 2) / config.frets in
  let string_spacing = if config.strings > 1 then
      (rows - 2) / (config.strings - 1)
    else 1
  in

  List.iter (fun h ->
    let row = 1 + (h.string * string_spacing) in
    let col = 1 + (h.fret * fret_width) + (fret_width / 2) in

    let style = {
      none with
      foreground = Some h.color;
      bold = Some true;
      background = Some (LTerm_style.rgb 40 40 40);
    } in

    (* Draw the highlight marker *)
    let marker = match h.label with
      | Some lbl when String.length lbl > 0 -> Zed_char.unsafe_of_char lbl.[0]
      | _ ->
          if config.show_note_names then
            Zed_char.unsafe_of_char (String.get (Core.Note.to_string h.note) 0)
          else
            Zed_char.unsafe_of_char 'o'  (* Simple circle marker *)
    in

    draw_char ctx row col marker ~style
  ) highlights

(** Draw fret numbers at the top *)
let draw_fret_numbers ctx config start_fret =
  let size = LTerm_draw.size ctx in
  let cols = size.LTerm_geom.cols in

  let fret_width = (cols - 2) / config.frets in

  for f = 0 to min config.frets 12 do
    let fret_num = start_fret + f in
    if fret_num > 0 && fret_num <= 24 then (
      let col = 1 + (f * fret_width) + (fret_width / 2) in
      let num_str = string_of_int fret_num in
      let style = { none with foreground = Some lblue; bold = Some false } in

      (* Draw each character *)
      for i = 0 to String.length num_str - 1 do
        draw_char ctx 0 (col - String.length num_str / 2 + i)
          (Zed_char.unsafe_of_char num_str.[i]) ~style
      done
    )
  done

(** Convert chord to highlights *)
let chord_to_highlights chord tuning =
  let notes = Harmony.Chord.notes chord in
  let root = Harmony.Chord.root chord in

  (* Find positions on fretboard for each note *)
  let highlights = ref [] in

  Array.iteri (fun string_idx open_note ->
    (* Try frets 0-12 on this string *)
    for fret = 0 to 12 do
      let fret_note = transpose_by_semitones open_note fret in

      (* Check if this note is in the chord *)
      List.iter (fun chord_note ->
        if Core.Note.enharmonic_eq fret_note chord_note then (
          let color = if Core.Note.enharmonic_eq chord_note root then
            lred  (* Root note *)
          else
            lyellow  (* Other chord tones *)
          in
          let label = if Core.Note.enharmonic_eq chord_note root then
            Some "R"
          else
            None
          in
          highlights := {
            string = string_idx;
            fret = fret;
            note = fret_note;
            color;
            label;
          } :: !highlights
        )
      ) notes
    done
  ) tuning;

  !highlights

(** Draw a complete mini fretboard with chord *)
let draw_chord ctx config chord tuning =
  (* Clear the area *)
  clear ctx;

  (* Draw grid *)
  draw_grid ctx config;

  (* Convert chord to highlights *)
  let highlights = chord_to_highlights chord tuning in

  (* Find optimal fret range to show *)
  let min_fret = List.fold_left (fun acc h ->
    min acc (if h.fret = 0 then 99 else h.fret)
  ) 99 highlights in

  let start_fret = if min_fret = 99 then 0 else max 0 (min_fret - 1) in

  (* Adjust highlights for display window *)
  let adjusted_highlights = List.map (fun h ->
    { h with fret = h.fret - start_fret }
  ) highlights in

  (* Filter to visible frets *)
  let visible_highlights = List.filter (fun h ->
    h.fret >= 0 && h.fret <= config.frets
  ) adjusted_highlights in

  (* Draw highlights *)
  draw_highlights ctx config visible_highlights;

  (* Draw fret numbers *)
  draw_fret_numbers ctx config start_fret

(** Draw a scale on mini fretboard *)
let draw_scale ctx config scale tuning =
  clear ctx;
  draw_grid ctx config;

  let scale_notes = scale.Scales.Scale.degrees in
  let root = scale.Scales.Scale.root in

  let highlights = ref [] in

  Array.iteri (fun string_idx open_note ->
    for fret = 0 to min 12 config.frets do
      let fret_note = transpose_by_semitones open_note fret in

      List.iter (fun scale_note ->
        if Core.Note.enharmonic_eq fret_note scale_note then (
          let color = if Core.Note.enharmonic_eq scale_note root then
            lred
          else
            lgreen
          in
          highlights := {
            string = string_idx;
            fret = fret;
            note = fret_note;
            color;
            label = if Core.Note.enharmonic_eq scale_note root then Some "R" else None;
          } :: !highlights
        )
      ) scale_notes
    done
  ) tuning;

  draw_highlights ctx config !highlights;
  draw_fret_numbers ctx config 0

(** Create a LTerm widget for mini fretboard *)
let create_widget config content =
  let size_request = { rows = config.height; cols = config.width } in

  object
    inherit LTerm_widget.t "mini_fretboard"

    method! can_focus = false

    method! size_request = size_request

    method! draw ctx _focused =
      match content with
      | `Chord (chord, tuning) ->
          draw_chord ctx config chord tuning
      | `Scale (scale, tuning) ->
          draw_scale ctx config scale tuning
      | `Empty ->
          clear ctx;
          draw_grid ctx config
  end
