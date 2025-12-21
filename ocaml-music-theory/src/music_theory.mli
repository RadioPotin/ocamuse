(** OCaml Music Theory Library *)

module Core : sig
  module Pitch = Pitch
  module Interval = Interval
  module Note = Note
end

module Scales : sig
  module Mode = Mode
  module Scale = Scale
  module Circle_of_fifths = Circle_of_fifths
end

module Harmony : sig
  module Chord = Chord
end
