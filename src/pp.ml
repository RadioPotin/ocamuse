module Color = struct
  let tablet =
    [| "" (* no clour *)
     ; "\027[0m" (* reset *)
     ; "\027[31m" (* red *)
     ; "\027[32m" (* green *)
     ; "\027[33m" (* yellow *)
     ; "\027[34m" (* blue *)
     ; "\027[35m" (* magenta *)
     ; "\027[36m" (* cyan *)
     ; "\027[37m" (* white *)
    |]

  let colour = ref false

  let colour_prefix fmt color =
    if !colour then Format.fprintf fmt "%s" tablet.(color) else ()

  let colour_suffix fmt () =
    if !colour then colour_prefix fmt 1 else colour_prefix fmt 0

  let colour_wrap fmt (colour, f) =
    Format.fprintf fmt "%a%a%a%a" colour_prefix colour f () colour_suffix ()
      Format.pp_print_flush ()

  let null fmt f =
    Format.fprintf fmt "%a%a%a%a" colour_prefix 0 f () colour_suffix ()
      Format.pp_print_flush ()

  let red fmt f =
    Format.fprintf fmt "%a%a%a%a" colour_prefix 1 f () colour_suffix ()
      Format.pp_print_flush ()

  let green fmt f =
    Format.fprintf fmt "%a%a%a%a" colour_prefix 2 f () colour_suffix ()
      Format.pp_print_flush ()

  let yellow fmt f =
    Format.fprintf fmt "%a%a%a%a" colour_prefix 3 f () colour_suffix ()
      Format.pp_print_flush ()

  let blue fmt f =
    Format.fprintf fmt "%a%a%a%a" colour_prefix 4 f () colour_suffix ()
      Format.pp_print_flush ()

  let magenta fmt f =
    Format.fprintf fmt "%a%a%a%a" colour_prefix 5 f () colour_suffix ()
      Format.pp_print_flush ()

  let cyan fmt f =
    Format.fprintf fmt "%a%a%a%a" colour_prefix 6 f () colour_suffix ()
      Format.pp_print_flush ()

  let white fmt f =
    Format.fprintf fmt "%a%a%a%a" colour_prefix 7 f () colour_suffix ()
      Format.pp_print_flush ()
end

module Notes = struct
  let print_base_note fmt =
    let open Types in
    function
    | A -> Format.fprintf fmt "A"
    | B -> Format.fprintf fmt "B"
    | C -> Format.fprintf fmt "C"
    | D -> Format.fprintf fmt "D"
    | E -> Format.fprintf fmt "E"
    | F -> Format.fprintf fmt "F"
    | G -> Format.fprintf fmt "G"

  let rec print_alteration fmt n =
    match n with
    | 0 -> ()
    | n when n > 0 ->
      Format.fprintf fmt "#";
      print_alteration fmt (n - 1)
    | _ ->
      Format.fprintf fmt "b";
      print_alteration fmt (n + 1)

  let sprint_note =
    let open Types in
    fun { base; alteration } ->
      Format.asprintf "%a%a" print_base_note base print_alteration alteration

  let print_note =
    let open Types in
    fun fmt { base; alteration } ->
      Format.fprintf fmt "%a%a" print_base_note base print_alteration alteration

  let print_notes fmt notes =
    Format.fprintf fmt "- %a@\n"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
         print_note )
      notes

  let print_diatonic_chord fmt =
    let open Types in
    fun (chord : diatonic_triad) ->
      match chord with
      | Major -> Format.fprintf fmt ""
      | Minor -> Format.fprintf fmt "m"
      | Diminished -> Format.fprintf fmt "dim"

  let print_chord_quality fmt =
    let open Types in
    fun (chord : chord) -> Format.fprintf fmt "%s" @@ Conv.chord_to_string chord

  let print_chord fmt (note, chord) =
    Format.fprintf fmt "%a%a" print_note note print_chord_quality chord

  let print_diatonic_chord =
    let open Types in
    fun fmt ({ base; alteration }, chord) ->
      Format.fprintf fmt "%a%a%a" print_base_note base print_alteration
        alteration print_diatonic_chord chord

  let print_diatonic_chords fmt chords =
    Format.fprintf fmt "- %a@\n"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
         print_diatonic_chord )
      chords
end

module Fretboard = struct end

module Tones = struct end

module Chord_diagrams = struct end
