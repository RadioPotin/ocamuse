(** Display module - renders fretboard with highlighting *)

(** Build a tonality (scale notes) from mode and root note *)
let build_tonality mode root =
  let open Types in
  (* Major scale intervals: W W H W W W H = 2 2 1 2 2 2 1 *)
  let major_intervals = [| 2; 2; 1; 2; 2; 2; 1 |] in
  (* Mode rotation: how many steps to rotate the intervals *)
  let rotation = match mode with
    | C_mode -> 0  (* Ionian *)
    | D_mode -> 1  (* Dorian *)
    | E_mode -> 2  (* Phrygian *)
    | F_mode -> 3  (* Lydian *)
    | G_mode -> 4  (* Mixolydian *)
    | A_mode -> 5  (* Aeolian *)
    | B_mode -> 6  (* Locrian *)
  in
  (* Rotate intervals for the mode *)
  let intervals = Array.init 7 (fun i -> major_intervals.((i + rotation) mod 7)) in
  (* Build scale starting from root *)
  let base_notes = [| C; D; E; F; G; A; B |] in
  let base_to_int = function C -> 0 | D -> 1 | E -> 2 | F -> 3 | G -> 4 | A -> 5 | B -> 6 in
  let int_to_base i = base_notes.(i mod 7) in

  (* Calculate semitones from C for a note *)
  let note_to_semitone note =
    let base_semitones = match note.base with
      | C -> 0 | D -> 2 | E -> 4 | F -> 5 | G -> 7 | A -> 9 | B -> 11
    in
    (base_semitones + note.alteration + 12) mod 12
  in

  (* Build scale notes *)
  let scale = Array.make 7 root in
  let current_semitone = ref (note_to_semitone root) in
  let current_base_idx = ref (base_to_int root.base) in

  for i = 1 to 6 do
    current_semitone := (!current_semitone + intervals.(i - 1)) mod 12;
    current_base_idx := (!current_base_idx + 1) mod 7;
    let next_base = int_to_base !current_base_idx in
    let natural_semitone = match next_base with
      | C -> 0 | D -> 2 | E -> 4 | F -> 5 | G -> 7 | A -> 9 | B -> 11
    in
    let alteration = (!current_semitone - natural_semitone + 12) mod 12 in
    let alteration = if alteration > 6 then alteration - 12 else alteration in
    scale.(i) <- { base = next_base; alteration }
  done;
  Array.to_list scale

(** Build degree table: maps pitch class (0-11) to scale degree (0-6) *)
let build_degree_tbl tonality =
  let tbl = Hashtbl.create 12 in
  List.iteri (fun degree note ->
    (* Use pitch class as key to handle all enharmonic equivalents *)
    let pitch_class = Conv.note_to_int note in
    Hashtbl.replace tbl pitch_class degree
  ) tonality;
  tbl

(** Build color table: maps degrees to colors based on theme *)
let build_degree_colour_tbl tonality color_theme =
  let open LTerm_style in
  let tbl = Hashtbl.create 7 in
  let colors = match color_theme with
    | Types.ChromaticGradient ->
      [| lred; lyellow; lgreen; lcyan; lblue; lmagenta; lwhite |]
    | Types.DiatonicDegrees ->
      [| lred; lyellow; lgreen; lcyan; lblue; lmagenta; lwhite |]
    | Types.CustomPalette _ ->
      [| lred; lyellow; lgreen; lcyan; lblue; lmagenta; lwhite |]
  in
  List.iteri (fun degree _note ->
    Hashtbl.add tbl degree colors.(degree mod 7)
  ) tonality;
  tbl

(** Build empty highlight tables (for chord/arpeggio - stubbed for now) *)
let build_empty_highlight_tables () =
  (Hashtbl.create 1, Hashtbl.create 1)

let select_view ctx ocamuse_context =
  let open Types in
  let bubble_view = function
    | Flat view -> view
    | Pattern (view, _mode) -> view
  in
  let make_struc ctx mode view notes_to_degree_tbl degree_to_color_tbl
    ocamuse_context : Types.pattern_view_draw_struc =
    let cursor_i = ref 0 in
    let cursor_j = ref 0 in
    let offset_for_frets_numbers =
      match view with Plain _ -> 0 | Fretted _ -> 2 | Interline _ -> 1
    in
    let fretboard = ocamuse_context.fretboard.notes in
    let offset = ref offset_for_frets_numbers in
    let number_of_strings = Array.length ocamuse_context.fretboard.notes in
    let color = Color.event_to_base_color !(ocamuse_context.base_colour) in
    let number_of_frets = Array.length ocamuse_context.fretboard.notes.(0) in
    { ctx
    ; mode
    ; view
    ; color
    ; offset
    ; cursor_i
    ; cursor_j
    ; fretboard
    ; string = ref 0
    ; number_of_frets
    ; number_of_strings
    ; notes_to_degree_tbl
    ; degree_to_color_tbl
    }
  in
  LTerm_draw.clear ctx;
  (* Build highlighting tables based on highlight_source *)
  let (notes_to_degree_tbl, degree_to_colour_tbl) =
    match ocamuse_context.highlight_source with
    | Tonality (mode, root) ->
      let tonality = build_tonality mode root in
      let notes_tbl = build_degree_tbl tonality in
      let colour_tbl = build_degree_colour_tbl tonality ocamuse_context.color_theme in
      (notes_tbl, colour_tbl)
    | Chord (_root, _chord_type) ->
      (* Stubbed for now - will use ocaml-music-theory later *)
      build_empty_highlight_tables ()
    | Arpeggio (_root, _chord_type) ->
      (* Stubbed for now - will use ocaml-music-theory later *)
      build_empty_highlight_tables ()
  in
  let display_mode = !(ocamuse_context.display_mode) in
  let view = bubble_view display_mode in
  begin
    match display_mode with
    | Flat _base_colour ->
      let struc =
        make_struc ctx C_mode view notes_to_degree_tbl degree_to_colour_tbl
          ocamuse_context
      in
      Draw.PATTERN.pattern struc
    | Pattern (view, mode) ->
      let struc =
        make_struc ctx mode view notes_to_degree_tbl degree_to_colour_tbl
          ocamuse_context
      in
      Draw.PATTERN.pattern struc
  end
