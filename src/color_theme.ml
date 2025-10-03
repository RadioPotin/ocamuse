(** Color theme system for mapping notes to colors *)

(** Get color from a palette by pitch class *)
let palette_color palette pitch_class =
  let pc = pitch_class mod 12 in
  palette.Config.Palettes.colors.(pc)

(** Map pitch class (0-11) to color using chromatic gradient *)
let pitch_class_to_chromatic_color pitch_class =
  let palette = Config.Palettes.chromatic_rainbow in
  palette_color palette pitch_class

(** Map degree (0-6) to traditional diatonic color scheme *)
let degree_to_diatonic_color degree =
  let open LTerm_style in
  match degree with
  | 0 -> lred      (* Tonic - Bright red *)
  | 1 -> blue      (* Supertonic *)
  | 2 -> lgreen    (* Mediant *)
  | 3 -> lblue     (* Subdominant *)
  | 4 -> lyellow   (* Dominant *)
  | 5 -> lmagenta  (* Submediant *)
  | 6 -> lcyan     (* Leading tone *)
  | _ -> white

(** Convert note to pitch class (0-11) *)
let note_to_pitch_class note =
  let open Types in
  let base_semitones = match note.base with
    | C -> 0 | D -> 2 | E -> 4 | F -> 5 | G -> 7 | A -> 9 | B -> 11
  in
  let pc = (base_semitones + note.alteration) mod 12 in
  (* Handle negative modulo *)
  if pc < 0 then pc + 12 else pc

(** Get color for a note based on theme and context *)
let get_note_color theme note degree_opt =
  match theme with
  | Types.ChromaticGradient ->
      (* Use pitch class for chromatic gradient *)
      let pc = note_to_pitch_class note in
      pitch_class_to_chromatic_color pc
  | Types.DiatonicDegrees ->
      (* Use degree if available, otherwise pitch class *)
      (match degree_opt with
       | Some degree -> degree_to_diatonic_color degree
       | None ->
           let pc = note_to_pitch_class note in
           pitch_class_to_chromatic_color pc)
  | Types.CustomPalette palette_name ->
      (* Use custom palette from config *)
      let pc = note_to_pitch_class note in
      (match Config.Palettes.find_by_name palette_name with
       | Some palette -> palette_color palette pc
       | None ->
           (* Fallback to chromatic if palette not found *)
           pitch_class_to_chromatic_color pc)

(** Cycle to next color theme *)
let cycle_theme current =
  (* Create a list of all themes *)
  let all_themes =
    Types.ChromaticGradient ::
    Types.DiatonicDegrees ::
    (List.map (fun p -> Types.CustomPalette p.Config.Palettes.name) Config.Palettes.all_palettes)
  in
  (* Find current position and get next *)
  let rec find_next themes =
    match themes with
    | [] -> Types.ChromaticGradient  (* Fallback *)
    | [_] -> List.hd all_themes  (* Wrap around *)
    | x :: y :: rest ->
        if x = current then y
        else find_next (y :: rest)
  in
  find_next all_themes

(** Get theme name for display *)
let theme_name = function
  | Types.ChromaticGradient -> "Chromatic Gradient"
  | Types.DiatonicDegrees -> "Diatonic Degrees"
  | Types.CustomPalette name -> name
