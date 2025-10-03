(** Configuration module for tunings and default settings *)

module Tunings = struct
  open Types

  (** Standard 6-string guitar tuning: E A D G B E (low to high) *)
  let standard_6 = [ E; A; D; G; B; E ]

  (** Standard 7-string guitar tuning: B E A D G B E (low to high) *)
  let standard_7 = [ B; E; A; D; G; B; E ]

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
