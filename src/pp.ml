module NOTES = struct

  module FMT = struct

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

    let print_notes fmt (notes : Types.note list) =
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
end

module FRETBOARD = struct

  (* ************************************** *)
  (** plain fretboard print iteri functions *)
  (* ************************************** *)

  let fret_print_iteri ~pp_sep ppf pp_v arr range =
    Array.iteri
      (fun i v ->
          Format.fprintf ppf "%d" (i + 1);
          pp_v ppf (i, v);
          if i < range - 1 then pp_sep i ppf () )
      arr

  let iterate_over_plain_string ~pp_sep fmt ppv arr =
    let range = Array.length arr in
    fret_print_iteri ~pp_sep fmt ppv arr range

  let plain_matrix_iteri ~pp_sep fmt ppv arr =
    iterate_over_plain_string ~pp_sep fmt ppv arr

  (* **************************************** *)
  (** Fretted fretboard print iteri functions *)
  (* **************************************** *)

  let fret_print_iteri ~pp_sep ppf pp_v arr range =
    Array.iteri
      (fun i v ->
          pp_v ppf (i, v);
          if i < range - 1 then pp_sep i ppf ()
      )
      arr

  let iterate_over_string ~pp_sep fmt ppv arr =
    let range = Array.length arr in
    fret_print_iteri ~pp_sep fmt ppv arr range

  let fret_matrix_iteri ~pp_sep fmt ppv arr =
    iterate_over_string ~pp_sep fmt ppv arr

  module FMT = struct

    (* ************************************** *)
    (*   General Purpose e.g. fmt conversion  *)
    (* ************************************** *)

    let stringify f arg =
      Format.asprintf "%a" f arg

    module PLAIN = struct
      let pp_fret fmt () =
        Format.fprintf fmt "|"
      let pp_space fmt () =
        Format.fprintf fmt " "
      let pp_sep fmt () =
        Format.fprintf fmt "-"
      let pp_plain_note note =
        Format.asprintf{|%a|} NOTES.FMT.print_note note
      let pp_note_fret_no_space note =
        Format.asprintf {|%a%a|} NOTES.FMT.print_note note pp_fret ()
      let pp_note_fret_with_space note =
        Format.asprintf {|%a%a%a|} NOTES.FMT.print_note note pp_space () pp_fret ()
      let pp_note_with_separator note =
        Format.asprintf {|%a%a|} NOTES.FMT.print_note note pp_sep ()
      let pp_note_with_separators note =
        Format.asprintf {|%a%a%a|} NOTES.FMT.print_note note pp_sep () pp_sep ()
    end

    module FRET = struct
      let pp_space_fmt fmt () =
        Format.fprintf fmt " "
      let pp_bar_fmt fmt () =
        Format.fprintf fmt "|"

      module NUMBERS = struct

        let pp_fret_nb_fmt fmt i =
          Format.fprintf fmt "%2d" i

        let pp_fret_fmt fmt n =
          Format.fprintf fmt {|%a%a%a%a%a%a%a|}
            pp_space_fmt ()
            pp_fret_nb_fmt n
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()

        let pp_last_fret_fmt fmt n =
          Format.fprintf fmt {|%a%a%a%a%a%a%a|}
            pp_space_fmt ()
            pp_space_fmt ()
            pp_fret_nb_fmt n
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()

        let pp_before fmt i =
          if i = 0 then
            Format.fprintf fmt "%a" pp_space_fmt ()
          else if i < 10 then
            Format.fprintf fmt "%a" pp_space_fmt ()
          else
            Format.fprintf fmt "%a%a" pp_space_fmt () pp_space_fmt ()

        let pp_after fmt i =
          if i = 0 then
            Format.fprintf fmt "%a" pp_space_fmt ()
          else if i < 10 then
            Format.fprintf fmt {|%a%a%a%a%a|}
              pp_space_fmt ()
              pp_space_fmt ()
              pp_space_fmt ()
              pp_space_fmt ()
              pp_space_fmt ()
          else
            Format.fprintf fmt {|%a%a%a%a|}
              pp_space_fmt ()
              pp_space_fmt ()
              pp_space_fmt ()
              pp_space_fmt ()

        let pp_fret_0 () =
          Format.asprintf {|%a0%a|} pp_space_fmt () pp_space_fmt ()

        let pp_fret n =
          Format.asprintf {|%a%a%a%a%a%a%a|}
            pp_space_fmt ()
            pp_fret_nb_fmt n
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()

        let pp_last_fret n =
          Format.asprintf {|%a%a%a%a%a%a%a|}
            pp_space_fmt ()
            pp_space_fmt ()
            pp_fret_nb_fmt n
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()
            pp_space_fmt ()
      end

      let pp_sep_fmt fmt () =
        Format.fprintf fmt "-"

      let pp_before fmt (fret_nb, _note) =
        if fret_nb = 0 then
          Format.fprintf fmt "%a" pp_bar_fmt ()
        else
          Format.fprintf fmt {|%a%a%a|} pp_bar_fmt () pp_sep_fmt () pp_space_fmt ()

      let pp_after fmt (fret_nb, note) =
        if fret_nb = 0 then
          ()
        else
          match Conv.note_to_int note with
          | 1 | 4 | 6 | 9 | 11 ->
            Format.fprintf fmt {|%a%a%a|}  pp_space_fmt () pp_sep_fmt () pp_bar_fmt ()
          | _n ->
            Format.fprintf fmt {|%a%a%a%a|} pp_space_fmt () pp_space_fmt () pp_sep_fmt () pp_bar_fmt ()

    end

    (* ************************************** *)
    (*         display notes plainly          *)
    (* ************************************** *)

    let print_plain_notes =
      fun fmt ((fret_nb, (note:Types.note)), string_number)
        ->
          let should_space =
            Fretboard.scan_column_for_alterations (fret_nb, string_number)
          in
          let note_int = Conv.note_to_int note in
          let plain_note =
            if fret_nb = 0 then begin
              match note_int with
              | 1 | 4 | 6 | 9 | 11 ->
                if should_space then
                  PLAIN.pp_note_fret_no_space note
                else
                  PLAIN.pp_note_fret_with_space note
              | _ ->
                PLAIN.pp_note_fret_with_space note
            end
            else if fret_nb < 10 then begin
              match note_int with
              | 1 | 4 | 6 | 9 | 11 ->
                PLAIN.pp_note_with_separator note
              | _ ->
                PLAIN.pp_note_with_separators note
            end
            else if fret_nb = 11 then begin
              match note_int with
              | 1 | 4 | 6 | 9 | 11 ->
                PLAIN.pp_note_fret_no_space note
              | _ ->
                if should_space then
                  PLAIN.pp_note_fret_with_space note
                else
                  PLAIN.pp_note_with_separator note
            end
            else
              PLAIN.pp_note_with_separator note
          in
          Format.fprintf fmt "%s" plain_note

    let print_plain_frets fmt (string_nb, string) =
      iterate_over_string
        ~pp_sep:(fun _string_nb fmt () -> Format.fprintf fmt "")
        fmt (fun fmt arg -> print_plain_notes fmt (arg, string_nb)) string;
      Format.pp_print_newline fmt ()

    let stringify_plain_string guitar_string =
      stringify print_plain_frets guitar_string

    (* ************************************** *)
    (* ************************************** *)

    let print_top_numbers fmt (fret_nb, _note) =
      let s =
        if fret_nb = 0 then FRET.NUMBERS.pp_fret_0 ()
        else if fret_nb < 10 then FRET.NUMBERS.pp_fret fret_nb
        else FRET.NUMBERS.pp_last_fret fret_nb
      in
      Format.fprintf fmt "%s" s

    let print_line_of_frets_numbers fmt (_string_nb, string) =
      iterate_over_string
        ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "")
        fmt print_top_numbers string;
      Format.pp_print_newline fmt ()

    let stringify_frets_numbers guitar_string =
      stringify print_line_of_frets_numbers guitar_string

    (* ************************************** *)
    (* ************************************** *)

    let display_fret fmt (fret_nb, note) =
      if fret_nb = 0 then
        Format.fprintf fmt "|%a" NOTES.FMT.print_note note
      else
        match Conv.note_to_int note with
        | 1 | 4 | 6 | 9 | 11 ->
          Format.fprintf fmt {||- %a -||} NOTES.FMT.print_note note
        | _n -> Format.fprintf fmt {||- %a  -||} NOTES.FMT.print_note note

    let display_frets fmt (_string_nb, string) =
      iterate_over_string
        ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "")
        fmt display_fret string;
      Format.pp_print_newline fmt ()

    let stringify_frets guitar_string =
      stringify display_frets guitar_string

    (* ************************************** *)
    (* ************************************** *)

    let display_box fmt (fret_nb, _note) =
      if fret_nb = 0 then begin
        Format.fprintf fmt "| ";
      end;
      if fret_nb < 11 then
        Format.fprintf fmt "|      |"
      else ()

    let display_line fmt (_string_nb, string) =
      iterate_over_string
        ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "")
        fmt display_box string;
      Format.pp_print_newline fmt ()

    let stringify_interline guitar_string =
      stringify display_line guitar_string

    (* ************************************** *)
    (* ************************************** *)

  end

end

module COLOR = struct
  (** module COLOR is destined to hold all functions and operations on displaying
        a colourful output *)
  let bubble_color =
    let open Types in
    begin
      function
      | Fretted view_color -> view_color
      | Plain view_color -> view_color
      | Interline view_color -> view_color
    end

  let rotate_to_prev =
    let open Types in
    function
    | Black ->    Lwhite
    | Red ->      Lblack
    | Green ->    Lred
    | Yellow ->   Lgreen
    | Blue ->     Lyellow
    | Magenta ->  Blue
    | Cyan ->     Magenta
    | White ->    Cyan
    | Lblack ->   White
    | Lred ->     Lblack
    | Lgreen ->   Lred
    | Lyellow ->  Lgreen
    | Lblue ->    Lyellow
    | Lmagenta -> Lblue
    | Lcyan ->    Lmagenta
    | Lwhite ->   Lcyan

  let rotate_to_next =
    let open Types in
    function
    | Black ->   Lred
    | Red ->     Lgreen
    | Green ->   Lyellow
    | Yellow ->  Blue
    | Blue ->    Magenta
    | Magenta -> Cyan
    | Cyan ->    White
    | White ->   Lblack
    | Lblack ->  Lred
    | Lred ->    Lgreen
    | Lgreen ->  Lyellow
    | Lyellow -> Lblue
    | Lblue ->   Lmagenta
    | Lmagenta-> Lcyan
    | Lcyan ->   Lwhite
    | Lwhite ->  Lblack

  let event_to_color =
    let open LTerm_style in
    let open Types in
    function
    | Black -> black
    | Red -> red
    | Green -> green
    | Yellow -> yellow
    | Blue -> blue
    | Magenta -> magenta
    | Cyan -> cyan
    | White -> white
    | Lblack -> lblack
    | Lred -> lred
    | Lgreen -> lgreen
    | Lyellow -> lyellow
    | Lblue -> lblue
    | Lmagenta -> lmagenta
    | Lcyan -> lcyan
    | Lwhite -> lwhite

  let event_to_color_flat_view =
    let open Types in
    function
    | Fretted event -> event_to_color event
    | Plain event -> event_to_color event
    | Interline event -> event_to_color event

  let is_equal color colour : bool =
    let open LTerm_style in
    let s1 = {none with foreground = Some color} in
    let s2 = {none with foreground = Some colour} in
    LTerm_style.equal s1 s2

  let find_color struc note =
    let open Types in
    match Hashtbl.find_opt struc.notes_to_degree_tbl note with
    | None -> struc.color
    | Some degree ->
      Hashtbl.find struc.degree_to_color_tbl degree

end
