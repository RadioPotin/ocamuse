(** Intelligent chord suggestion engine using harmonic analysis *)

open Music_theory

(** A chord suggestion with contextual information *)
type suggestion = {
  chord: Harmony.Chord.t;
  description: string;  (** Human-readable description of why this chord is suggested *)
  strength: int;        (** Strength score 0-10, higher = more common/expected *)
  category: suggestion_category;
}

(** Categories of suggestions *)
and suggestion_category =
  | CommonProgression  (** Part of a common chord progression *)
  | HarmonicFunction  (** Based on tonic/dominant/subdominant relationships *)
  | ModalInterchange  (** Borrowed from parallel mode *)
  | Substitution      (** Tritone or related substitution *)
  | Chromatic         (** Chromatic movement *)

(** Generate suggestions from tonic chord (I) *)
let suggest_from_tonic scale =
  let triads = Scales.Scale.diatonic_triads scale in

  (* I -> IV (subdominant) *)
  let iv_chord = List.nth_opt triads 3 in  (* 4th degree *)
  let iv_suggestions = match iv_chord with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.major root;
            description = "Subdominant - warm, stable move";
            strength = 9;
            category = HarmonicFunction;
          }
        ]
    | None -> []
  in

  (* I -> V (dominant) *)
  let v_chord = List.nth_opt triads 4 in  (* 5th degree *)
  let v_suggestions = match v_chord with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.dominant7 root;
            description = "Dominant - creates tension, wants resolution";
            strength = 10;
            category = HarmonicFunction;
          };
          { chord = Harmony.Chord.major root;
            description = "Dominant triad - lighter tension";
            strength = 8;
            category = HarmonicFunction;
          }
        ]
    | None -> []
  in

  (* I -> vi (relative minor) *)
  let vi_chord = List.nth_opt triads 5 in  (* 6th degree *)
  let vi_suggestions = match vi_chord with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.minor root;
            description = "Relative minor - smooth, emotional transition";
            strength = 8;
            category = CommonProgression;
          }
        ]
    | None -> []
  in

  (* I -> ii (supertonic) *)
  let ii_chord = List.nth_opt triads 1 in  (* 2nd degree *)
  let ii_suggestions = match ii_chord with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.minor root;
            description = "Supertonic - gentle move, often goes to V";
            strength = 7;
            category = CommonProgression;
          }
        ]
    | None -> []
  in

  iv_suggestions @ v_suggestions @ vi_suggestions @ ii_suggestions

(** Generate suggestions from dominant chord (V or V7) *)
let suggest_from_dominant scale =
  let triads = Scales.Scale.diatonic_triads scale in

  (* V -> I (resolution) *)
  let i_chord = List.nth_opt triads 0 in
  let i_suggestions = match i_chord with
    | Some (root, quality) ->
        let chord = match quality with
          | Scales.Scale.Major -> Harmony.Chord.major root
          | Scales.Scale.Minor -> Harmony.Chord.minor root
          | Scales.Scale.Diminished -> Harmony.Chord.diminished root
          | Scales.Scale.Augmented -> Harmony.Chord.major root  (* Fallback *)
        in
        [ { chord;
            description = "Tonic - strong resolution, feels like home";
            strength = 10;
            category = HarmonicFunction;
          }
        ]
    | None -> []
  in

  (* V -> vi (deceptive cadence) *)
  let vi_chord = List.nth_opt triads 5 in
  let vi_suggestions = match vi_chord with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.minor root;
            description = "Deceptive cadence - unexpected, emotionally rich";
            strength = 7;
            category = CommonProgression;
          }
        ]
    | None -> []
  in

  (* V -> IV (plagal feel, less common from V) *)
  let iv_chord = List.nth_opt triads 3 in
  let iv_suggestions = match iv_chord with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.major root;
            description = "Retrogression - unusual, dreamy quality";
            strength = 4;
            category = Chromatic;
          }
        ]
    | None -> []
  in

  i_suggestions @ vi_suggestions @ iv_suggestions

(** Generate suggestions from subdominant chord (IV) *)
let suggest_from_subdominant scale =
  let triads = Scales.Scale.diatonic_triads scale in

  (* IV -> I (plagal cadence) *)
  let i_chord = List.nth_opt triads 0 in
  let i_suggestions = match i_chord with
    | Some (root, quality) ->
        let chord = match quality with
          | Scales.Scale.Major -> Harmony.Chord.major root
          | Scales.Scale.Minor -> Harmony.Chord.minor root
          | Scales.Scale.Diminished -> Harmony.Chord.diminished root
          | Scales.Scale.Augmented -> Harmony.Chord.major root
        in
        [ { chord;
            description = "Plagal cadence (Amen) - peaceful resolution";
            strength = 9;
            category = HarmonicFunction;
          }
        ]
    | None -> []
  in

  (* IV -> V (pre-dominant function) *)
  let v_chord = List.nth_opt triads 4 in
  let v_suggestions = match v_chord with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.dominant7 root;
            description = "Setup for resolution - classic IV-V-I";
            strength = 10;
            category = CommonProgression;
          };
          { chord = Harmony.Chord.major root;
            description = "Lighter dominant approach";
            strength = 8;
            category = CommonProgression;
          }
        ]
    | None -> []
  in

  (* IV -> ii (circle progression) *)
  let ii_chord = List.nth_opt triads 1 in
  let ii_suggestions = match ii_chord with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.minor root;
            description = "Circle of fifths movement";
            strength = 6;
            category = HarmonicFunction;
          }
        ]
    | None -> []
  in

  i_suggestions @ v_suggestions @ ii_suggestions

(** Generate suggestions from other diatonic chords *)
let suggest_from_other scale _chord =
  let triads = Scales.Scale.diatonic_triads scale in

  (* Suggest common neighboring chords *)
  let suggestions = ref [] in

  (* Add suggestions for each diatonic chord *)
  List.iteri (fun i (root, quality) ->
    if i >= 0 && i < 7 then (
      let chord = match quality with
        | Scales.Scale.Major -> Harmony.Chord.major root
        | Scales.Scale.Minor -> Harmony.Chord.minor root
        | Scales.Scale.Diminished -> Harmony.Chord.diminished root
        | Scales.Scale.Augmented -> Harmony.Chord.major root
      in

      (* Prioritize I, IV, V chords *)
      let (desc, strength) = match i with
        | 0 -> ("Tonic - return to home", 8)
        | 3 -> ("Subdominant - stable movement", 7)
        | 4 -> ("Dominant - creates tension", 8)
        | _ -> ("Diatonic movement", 5)
      in

      suggestions := {
        chord;
        description = desc;
        strength;
        category = HarmonicFunction;
      } :: !suggestions
    )
  ) triads;

  List.rev !suggestions

(** Generate suggestions for starting a progression (no current chord) *)
let suggest_starting_chords scale =
  let triads = Scales.Scale.diatonic_triads scale in

  (* I chord - most common start *)
  let i_suggestions = match List.nth_opt triads 0 with
    | Some (root, quality) ->
        let chord = match quality with
          | Scales.Scale.Major -> Harmony.Chord.major root
          | Scales.Scale.Minor -> Harmony.Chord.minor root
          | Scales.Scale.Diminished -> Harmony.Chord.diminished root
          | Scales.Scale.Augmented -> Harmony.Chord.major root
        in
        [ { chord;
            description = "Tonic - establishes key, feels like home";
            strength = 10;
            category = HarmonicFunction;
          }
        ]
    | None -> []
  in

  (* IV chord - strong alternative start *)
  let iv_suggestions = match List.nth_opt triads 3 with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.major root;
            description = "Subdominant start - gentle, anticipatory";
            strength = 7;
            category = CommonProgression;
          }
        ]
    | None -> []
  in

  (* vi chord - relative minor start *)
  let vi_suggestions = match List.nth_opt triads 5 with
    | Some (root, _quality) ->
        [ { chord = Harmony.Chord.minor root;
            description = "Relative minor - darker, emotional opening";
            strength = 6;
            category = CommonProgression;
          }
        ]
    | None -> []
  in

  i_suggestions @ iv_suggestions @ vi_suggestions

(** Main suggestion generation function *)
let generate_suggestions scale current_chord =
  match current_chord with
  | None -> suggest_starting_chords scale
  | Some chord ->
      match Harmony.Chord.function_in_key scale chord with
      | Some Harmony.Chord.Tonic ->
          suggest_from_tonic scale
      | Some Harmony.Chord.Dominant ->
          suggest_from_dominant scale
      | Some Harmony.Chord.Subdominant ->
          suggest_from_subdominant scale
      | _ ->
          suggest_from_other scale chord

(** Sort suggestions by strength (descending) *)
let sort_suggestions suggestions =
  List.sort (fun a b -> compare b.strength a.strength) suggestions

(** Convert suggestion to display tuple (chord, description, strength) *)
let to_display_tuple suggestion =
  (suggestion.chord, suggestion.description, suggestion.strength)

(** Get top N suggestions *)
let get_top_suggestions n scale current_chord =
  generate_suggestions scale current_chord
  |> sort_suggestions
  |> (fun lst -> List.filteri (fun i _ -> i < n) lst)
  |> List.map to_display_tuple
