(** OCaml Music Theory Library *)

module Core = struct
  module Pitch = Pitch
  module Interval = Interval
  module Note = Note
end

module Scales = struct
  module Mode = Mode
  module Scale = Scale
  module Circle_of_fifths = Circle_of_fifths
end

module Harmony = struct
  module Chord = Chord
end

(* Future modules: *)
(* module Instruments = ... *)
(* module Io = ... *)
