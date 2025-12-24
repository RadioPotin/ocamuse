(** Configuration module for tunings, color palettes, and default settings *)

(** Color palette definitions *)
module Palettes = struct
  open LTerm_style

  (** A color palette maps pitch classes (0-11) to colors *)
  type palette =
    { name : string
    ; colors : LTerm_style.color array (* 12 colors for 12 pitch classes *)
    ; description : string
    }

  (** Classic chromatic rainbow gradient *)
  let chromatic_rainbow =
    { name = "Chromatic Rainbow"
    ; description = "Full color wheel mapped to 12 semitones"
    ; colors =
        [| lred
         ; (* C - Red *)
           red
         ; (* C#/Db - Dark Red *)
           lyellow
         ; (* D - Light Yellow *)
           yellow
         ; (* D#/Eb - Yellow *)
           lgreen
         ; (* E - Light Green *)
           green
         ; (* F - Green *)
           lcyan
         ; (* F#/Gb - Cyan (tritone) *)
           lblue
         ; (* G - Light Blue *)
           blue
         ; (* G#/Ab - Blue *)
           lmagenta
         ; (* A - Light Magenta *)
           magenta
         ; (* A#/Bb - Magenta *)
           lred
           (* B - Back to red *)
        |]
    }

  (** Warm palette - reds, oranges, yellows *)
  let warm_palette =
    { name = "Warm Tones"
    ; description = "Warm colors: reds, oranges, yellows"
    ; colors =
        [| lred
         ; (* C *)
           red
         ; (* C# *)
           lred
         ; (* D *)
           lyellow
         ; (* D# *)
           yellow
         ; (* E *)
           lyellow
         ; (* F *)
           yellow
         ; (* F# *)
           lred
         ; (* G *)
           red
         ; (* G# *)
           lred
         ; (* A *)
           lyellow
         ; (* A# *)
           yellow
           (* B *)
        |]
    }

  (** Cool palette - blues, greens, cyans *)
  let cool_palette =
    { name = "Cool Tones"
    ; description = "Cool colors: blues, greens, cyans"
    ; colors =
        [| lcyan
         ; (* C *)
           cyan
         ; (* C# *)
           lblue
         ; (* D *)
           blue
         ; (* D# *)
           lgreen
         ; (* E *)
           green
         ; (* F *)
           lcyan
         ; (* F# *)
           cyan
         ; (* G *)
           lblue
         ; (* G# *)
           blue
         ; (* A *)
           lgreen
         ; (* A# *)
           green
           (* B *)
        |]
    }

  (** Monochrome palette - shades of blue *)
  let monochrome_blue =
    { name = "Monochrome Blue"
    ; description = "Various shades of blue"
    ; colors =
        [| lblue
         ; blue
         ; lcyan
         ; cyan
         ; lblue
         ; blue
         ; lcyan
         ; cyan
         ; lblue
         ; blue
         ; lcyan
         ; cyan
        |]
    }

  (** High contrast palette *)
  let high_contrast =
    { name = "High Contrast"
    ; description = "Maximum color distinction"
    ; colors =
        [| lred
         ; (* C *)
           lyellow
         ; (* C# *)
           lgreen
         ; (* D *)
           lcyan
         ; (* D# *)
           lblue
         ; (* E *)
           lmagenta
         ; (* F *)
           red
         ; (* F# *)
           yellow
         ; (* G *)
           green
         ; (* G# *)
           cyan
         ; (* A *)
           blue
         ; (* A# *)
           magenta
           (* B *)
        |]
    }

  (** Pastel palette - light, soft colors *)
  let pastel =
    { name = "Pastel"
    ; description = "Soft, light colors"
    ; colors =
        [| lred
         ; lyellow
         ; lgreen
         ; lcyan
         ; lblue
         ; lmagenta
         ; lred
         ; lyellow
         ; lgreen
         ; lcyan
         ; lblue
         ; lmagenta
        |]
    }

  (** Circle of fifths palette - related keys have similar colors *)
  let circle_of_fifths =
    { name = "Circle of Fifths"
    ; description = "Colors follow the circle of fifths"
    ; colors =
        [| lred
         ; (* C - Tonic *)
           lmagenta
         ; (* C# *)
           lyellow
         ; (* D *)
           lgreen
         ; (* D# *)
           lblue
         ; (* E *)
           lcyan
         ; (* F *)
           cyan
         ; (* F# - Tritone, opposite color *)
           lblue
         ; (* G - Dominant *)
           lgreen
         ; (* G# *)
           lyellow
         ; (* A *)
           lmagenta
         ; (* A# *)
           red
           (* B *)
        |]
    }

  (** Colorblind-friendly palette - Deuteranopia (red-green blindness)
      Uses blue-yellow axis which is preserved in most color vision deficiencies *)
  let colorblind_deuteranopia =
    { name = "Colorblind Safe"
    ; description = "Blue-yellow safe for red-green color blindness"
    ; colors =
        [| lblue    (* C - Primary blue *)
         ; blue     (* C# *)
         ; lcyan    (* D *)
         ; cyan     (* D# *)
         ; lyellow  (* E *)
         ; yellow   (* F *)
         ; lwhite   (* F# - Tritone, neutral *)
         ; lblue    (* G *)
         ; blue     (* G# *)
         ; lcyan    (* A *)
         ; lyellow  (* A# *)
         ; yellow   (* B *)
        |]
    }

  (** Tritanopia-friendly palette - for blue-yellow color blindness
      Uses red-cyan axis *)
  let colorblind_tritanopia =
    { name = "Tritanopia Safe"
    ; description = "Red-cyan safe for blue-yellow color blindness"
    ; colors =
        [| lred     (* C - Primary red *)
         ; red      (* C# *)
         ; lmagenta (* D *)
         ; magenta  (* D# *)
         ; lcyan    (* E *)
         ; cyan     (* F *)
         ; lwhite   (* F# - Tritone, neutral *)
         ; lred     (* G *)
         ; red      (* G# *)
         ; lmagenta (* A *)
         ; lcyan    (* A# *)
         ; cyan     (* B *)
        |]
    }

  (** Grayscale palette - works for all color vision types
      Uses brightness differences only *)
  let grayscale =
    { name = "Grayscale"
    ; description = "Brightness-only for maximum accessibility"
    ; colors =
        [| lwhite   (* C - Brightest - tonic *)
         ; white    (* C# *)
         ; lblack   (* D *)
         ; lwhite   (* D# *)
         ; white    (* E *)
         ; lblack   (* F *)
         ; black    (* F# - Tritone, darkest *)
         ; lwhite   (* G - Bright - dominant *)
         ; white    (* G# *)
         ; lblack   (* A *)
         ; lwhite   (* A# *)
         ; white    (* B *)
        |]
    }

  (** All available palettes *)
  let all_palettes =
    [ chromatic_rainbow
    ; warm_palette
    ; cool_palette
    ; monochrome_blue
    ; high_contrast
    ; pastel
    ; circle_of_fifths
    ; colorblind_deuteranopia
    ; colorblind_tritanopia
    ; grayscale
    ]

  (** Get palette by name *)
  let find_by_name name = List.find_opt (fun p -> p.name = name) all_palettes

  (** Get default palette *)
  let default () = circle_of_fifths
end

module Tunings = struct
  open Types

  (** Standard 6-string guitar tuning: E A D G B E (low to high) *)
  let standard_6 = [ E; A; D; G; B; E ]

  (** Standard 6-string guitar tuning: D A D G B E (low to high) *)
  let drop_d_6 = [ D; A; D; G; B; E ]

  (** Standard 7-string guitar tuning: B E A D G B E (low to high) *)
  let standard_7 = [ B; E; A; D; G; B; E ]

  (** Standard 7-string guitar tuning: A E A D G B E (low to high) *)
  let drop_a_7 = [ A; E; A; D; G; B; E ]

  (** Drop D tuning: D A D G B E *)
  let drop_d = [ D; A; D; G; B; E ]

  (** Open G tuning: D G D G B D *)
  let open_g = [ D; G; D; G; B; D ]

  (** Open D tuning: D A D F# A D *)
  let open_d =
    [ { base = D; alteration = 0 }
    ; { base = A; alteration = 0 }
    ; { base = D; alteration = 0 }
    ; { base = F; alteration = 1 }
    ; { base = A; alteration = 0 }
    ; { base = D; alteration = 0 }
    ]

  (** Convert simple base notes to full notes with no alteration *)
  let to_tuning base_notes =
    List.map (fun base -> { base; alteration = 0 }) base_notes
end

(** Default configuration values *)
let default_fret_range = 13

let default_tuning () = Tunings.to_tuning Tunings.standard_6
