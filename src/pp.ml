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

  let code_of_string = function
    | "null" -> "\027[0m" (* reset *)
    | "red" -> "\027[31m" (* red *)
    | "green" -> "\027[32m" (* green *)
    | "yellow" -> "\027[33m" (* yellow *)
    | "blue" -> "\027[34m" (* blue *)
    | "magenta" -> "\027[35m" (* magenta *)
    | "cyan" -> "\027[36m" (* cyan *)
    | "white" -> "\027[37m" (* white *)
    | _ -> assert false

  let colour = ref false

  let colour_prefix fmt color =
    if !colour then Format.fprintf fmt "%s" tablet.(color) else ()

  let colour_suffix fmt () =
    if !colour then colour_prefix fmt 1 else colour_prefix fmt 0

  let colour_wrap fmt (colour, s) =
    Format.fprintf fmt "%a%s%a%a" colour_prefix colour s colour_suffix ()
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
  let display_fret fmt note =
    match Conv.note_to_int note with
    | 1 | 4 | 6 | 9 | 11 ->
      Format.fprintf fmt {||  %a  ||} Notes.print_note note
    | _n -> Format.fprintf fmt {||  %a   ||} Notes.print_note note

  let guitar_string fmt l =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
      display_fret fmt l

  let fret_numbering fmt ~range =
    Format.fprintf fmt "   |  %2d  |" 1;
    for i = 2 to range - 1 do
      Format.fprintf fmt "|  %2d  |" i
    done;
    Format.fprintf fmt "@\n"

  let print_board fmt board =
    let place_string_number fmt (i, s) =
      Format.fprintf fmt "%d |%a" i guitar_string s
    in

    List.iteri
      (fun i string ->
        if i = 0 then begin
          let range = List.length @@ List.hd board in
          fret_numbering fmt ~range;
          place_string_number fmt (i + 1, string)
        end
        else place_string_number fmt (i + 1, string);
        Format.fprintf fmt "@\n" )
      board

  let fb fmt ~tuning =
    let top_of_fretboard = Fretboard.init_fretboard ~tuning () in
    Format.fprintf fmt "- Fretmap:@\n%a" print_board top_of_fretboard
end
