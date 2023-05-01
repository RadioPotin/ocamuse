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

module Fretboard = struct
  let fret_numbering fmt ~range =
    Format.fprintf fmt "  ||   %d  |" 0;
    for i = 1 to range - 1 do
      Format.fprintf fmt "|  %2d  |" i
    done;
    Format.fprintf fmt "@\n"

  let dots fmt ~range =
    for i = 0 to range - 1 do
      if i = 0 || i = 1 || i = 2 then Format.fprintf fmt "         "
      else if i = 3 || i = 5 || i = 7 || i = 9 || i = 12 then
        Format.fprintf fmt "    ·   "
      else Format.fprintf fmt "        "
    done;
    Format.pp_print_newline fmt ()

  let place_string_number fmt (i, f, s) : unit =
    Format.fprintf fmt "%d |%a" i f s

  let display_fret fmt (fret_nb, note) : unit =
    if fret_nb = 0 then Format.fprintf fmt "|%a" Notes.print_note note
    else
      match Conv.note_to_int note with
      | 1 | 4 | 6 | 9 | 11 ->
        Format.fprintf fmt {||--%a--||} Notes.print_note note
      | _n -> Format.fprintf fmt {||---%a--||} Notes.print_note note

  let iteri_down_strings fmt (fi : Format.formatter -> 'a -> unit) l : unit =
    let rec iteri_down_strings i = function
      | [] -> ()
      | a :: l ->
        fi fmt (i, a);
        iteri_down_strings (i - 1) l
    in
    iteri_down_strings (List.length l) l

  let guitar_string fmt (f, l) : unit = iteri_down_strings fmt f l

  let plain_string fmt string_ : unit = guitar_string fmt (display_fret, string_)

  let print_board fmt (board, f) : unit =
    let range = List.length @@ List.hd board in
    List.iteri
      (fun i corde ->
        let real_nb = i + 1 in
        if i = 0 then begin
          fret_numbering fmt ~range;
          place_string_number fmt (real_nb, f, corde)
        end
        else place_string_number fmt (real_nb, f, corde);
        Format.fprintf fmt "@\n" )
      board;
    dots fmt ~range

  let plain fmt b : unit = print_board fmt (b, plain_string)

  let fb = print_board
end

module Tones = struct end

module Chord_diagrams = struct end
