module NOTES = struct
  module FMT = struct
    let print_base_note fmt =
      let open Types in
      function
      | A -> Fmt.pf fmt "A"
      | B -> Fmt.pf fmt "B"
      | C -> Fmt.pf fmt "C"
      | D -> Fmt.pf fmt "D"
      | E -> Fmt.pf fmt "E"
      | F -> Fmt.pf fmt "F"
      | G -> Fmt.pf fmt "G"

    let rec print_alteration fmt n =
      match n with
      | 0 -> ()
      | n when n > 0 ->
        Fmt.pf fmt "#";
        print_alteration fmt (n - 1)
      | _ ->
        Fmt.pf fmt "b";
        print_alteration fmt (n + 1)

    let sprint_note =
      let open Types in
      fun { base; alteration } ->
        Fmt.str "%a%a" print_base_note base print_alteration alteration

    let print_note =
      let open Types in
      fun fmt { base; alteration } ->
        Fmt.pf fmt "%a%a" print_base_note base print_alteration alteration

    let print_notes fmt (notes : Types.note list) =
      Fmt.pf fmt "- %a@\n"
        (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ") print_note)
        notes

    let print_diatonic_chord fmt =
      let open Types in
      fun (chord : diatonic_triad) ->
        match chord with
        | Major -> Fmt.pf fmt ""
        | Minor -> Fmt.pf fmt "m"
        | Diminished -> Fmt.pf fmt "dim"

    let print_chord_quality fmt =
      let open Types in
      fun (chord : chord) -> Fmt.pf fmt "%s" @@ Conv.chord_to_string chord

    let print_chord fmt (note, chord) =
      Fmt.pf fmt "%a%a" print_note note print_chord_quality chord

    let print_diatonic_chord =
      let open Types in
      fun fmt ({ base; alteration }, chord) ->
        Fmt.pf fmt "%a%a%a" print_base_note base print_alteration alteration
          print_diatonic_chord chord

    let print_diatonic_chords fmt chords =
      Fmt.pf fmt "- %a@\n"
        (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ") print_diatonic_chord)
        chords
  end
end

module FRETBOARD = struct
  module FMT = struct
    (* ************************************** *)
    (*   General Purpose e.g. fmt conversion  *)
    (* ************************************** *)

    let stringify f arg = Fmt.str "%a" f arg

    module PLAIN = struct
      let pp_fret fmt () = Fmt.pf fmt "|"

      let pp_space fmt () = Fmt.pf fmt " "

      let pp_sep fmt () = Fmt.pf fmt "-"

      let pp_plain_note note = Fmt.str {|%a|} NOTES.FMT.print_note note

      let pp_note_fret_no_space note =
        Fmt.str {|%a%a|} NOTES.FMT.print_note note pp_fret ()

      let pp_note_fret_with_space note =
        Fmt.str {|%a%a%a|} NOTES.FMT.print_note note pp_space () pp_fret ()

      let pp_note_with_separator note =
        Fmt.str {|%a%a|} NOTES.FMT.print_note note pp_sep ()

      let pp_note_with_separators note =
        Fmt.str {|%a%a%a|} NOTES.FMT.print_note note pp_sep () pp_sep ()
    end

    module FRET = struct
      let pp_space_fmt fmt () = Fmt.pf fmt " "

      let pp_bar_fmt fmt () = Fmt.pf fmt "|"

      let pp_sep_fmt fmt () = Fmt.pf fmt "-"

      let pp_before fmt (fret_nb, _note) =
        if fret_nb = 0 then Fmt.pf fmt "%a" pp_bar_fmt ()
        else Fmt.pf fmt {|%a%a%a|} pp_bar_fmt () pp_sep_fmt () pp_space_fmt ()

      let pp_after fmt (fret_nb, note) =
        if fret_nb = 0 then ()
        else
          match Conv.note_to_int note with
          | 1 | 4 | 6 | 9 | 11 ->
            Fmt.pf fmt {|%a%a%a|} pp_space_fmt () pp_sep_fmt () pp_bar_fmt ()
          | _n ->
            Fmt.pf fmt {|%a%a%a%a|} pp_space_fmt () pp_space_fmt () pp_sep_fmt
              () pp_bar_fmt ()

      module NUMBERS = struct
        let pp_fret_nb_fmt fmt i = Fmt.pf fmt "%2d" i

        let pp_fret_fmt fmt n =
          Fmt.pf fmt {|%a%a%a%a%a%a%a|} pp_space_fmt () pp_fret_nb_fmt n
            pp_space_fmt () pp_space_fmt () pp_space_fmt () pp_space_fmt ()
            pp_space_fmt ()

        let pp_last_fret_fmt fmt n =
          Fmt.pf fmt {|%a%a%a%a%a%a%a|} pp_space_fmt () pp_space_fmt ()
            pp_fret_nb_fmt n pp_space_fmt () pp_space_fmt () pp_space_fmt ()
            pp_space_fmt ()

        let pp_before fmt i =
          if i = 0 then Fmt.pf fmt "%a" pp_space_fmt ()
          else if i < 10 then Fmt.pf fmt "%a" pp_space_fmt ()
          else Fmt.pf fmt "%a%a" pp_space_fmt () pp_space_fmt ()

        let pp_after fmt i =
          if i = 0 then Fmt.pf fmt "%a" pp_space_fmt ()
          else if i < 10 then
            Fmt.pf fmt {|%a%a%a%a%a|} pp_space_fmt () pp_space_fmt ()
              pp_space_fmt () pp_space_fmt () pp_space_fmt ()
          else
            Fmt.pf fmt {|%a%a%a%a|} pp_space_fmt () pp_space_fmt () pp_space_fmt
              () pp_space_fmt ()

        let pp_fret_0 () = Fmt.str {|%a0%a|} pp_space_fmt () pp_space_fmt ()

        let pp_fret n =
          Fmt.str {|%a%a%a%a%a%a%a|} pp_space_fmt () pp_fret_nb_fmt n
            pp_space_fmt () pp_space_fmt () pp_space_fmt () pp_space_fmt ()
            pp_space_fmt ()

        let pp_last_fret n =
          Fmt.str {|%a%a%a%a%a%a%a|} pp_space_fmt () pp_space_fmt ()
            pp_fret_nb_fmt n pp_space_fmt () pp_space_fmt () pp_space_fmt ()
            pp_space_fmt ()
      end
    end

    module INTERLINE = struct
      let pp_box fmt fret_nb =
        if fret_nb = 0 then begin
          Fmt.pf fmt {|%a%a|} FRET.pp_bar_fmt () FRET.pp_space_fmt ()
        end;
        if fret_nb < 11 then
          Fmt.pf fmt {|%a%a%a%a%a%a%a%a|} FRET.pp_bar_fmt () FRET.pp_space_fmt
            () FRET.pp_space_fmt () FRET.pp_space_fmt () FRET.pp_space_fmt ()
            FRET.pp_space_fmt () FRET.pp_space_fmt () FRET.pp_bar_fmt ()
        else ()
    end

    (* ************************************** *)
    (** plain fretboard print iteri functions *)
    (* ************************************** *)

    let fret_print_iteri ~pp_sep ppf pp_v arr range =
      Array.iteri
        (fun i v ->
          Fmt.pf ppf "%d" (i + 1);
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
          if i < range - 1 then pp_sep i ppf () )
        arr

    let iterate_over_string ~pp_sep fmt ppv arr =
      let range = Array.length arr in
      fret_print_iteri ~pp_sep fmt ppv arr range

    let fret_matrix_iteri ~pp_sep fmt ppv arr =
      iterate_over_string ~pp_sep fmt ppv arr

    (* ************************************** *)
    (*         display notes plainly          *)
    (* ************************************** *)

    let print_plain_notes =
     fun fmt ((fret_nb, (note : Types.note)), string_number) ->
      let should_space =
        Fretboard.scan_column_for_alterations (fret_nb, string_number)
      in
      let note_int = Conv.note_to_int note in
      let plain_note =
        if fret_nb = 0 then begin
          match note_int with
          | 1 | 4 | 6 | 9 | 11 ->
            if should_space then PLAIN.pp_note_fret_no_space note
            else PLAIN.pp_note_fret_with_space note
          | _ -> PLAIN.pp_note_fret_with_space note
        end
        else if fret_nb < 10 then begin
          match note_int with
          | 1 | 4 | 6 | 9 | 11 -> PLAIN.pp_note_with_separator note
          | _ -> PLAIN.pp_note_with_separators note
        end
        else if fret_nb = 11 then begin
          match note_int with
          | 1 | 4 | 6 | 9 | 11 -> PLAIN.pp_note_fret_no_space note
          | _ ->
            if should_space then PLAIN.pp_note_fret_with_space note
            else PLAIN.pp_note_with_separator note
        end
        else PLAIN.pp_note_with_separator note
      in
      Fmt.pf fmt "%s" plain_note

    let print_plain_frets fmt (string_nb, string) =
      iterate_over_string
        ~pp_sep:(fun _string_nb fmt () -> Fmt.pf fmt "")
        fmt
        (fun fmt arg -> print_plain_notes fmt (arg, string_nb))
        string;
      Fmt.pf fmt "@\n"

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
      Fmt.pf fmt "%s" s

    let print_line_of_frets_numbers fmt (_string_nb, string) =
      iterate_over_string
        ~pp_sep:(fun _fret_nb fmt () -> Fmt.pf fmt "")
        fmt print_top_numbers string;
      Fmt.pf fmt "@\n"

    let stringify_frets_numbers guitar_string =
      stringify print_line_of_frets_numbers guitar_string

    (* ************************************** *)
    (* ************************************** *)

    let display_fret fmt (fret_nb, note) =
      if fret_nb = 0 then Fmt.pf fmt "|%a" NOTES.FMT.print_note note
      else
        match Conv.note_to_int note with
        | 1 | 4 | 6 | 9 | 11 ->
          Fmt.pf fmt {||- %a -||} NOTES.FMT.print_note note
        | _n -> Fmt.pf fmt {||- %a  -||} NOTES.FMT.print_note note

    let display_frets fmt (_string_nb, string) =
      iterate_over_string
        ~pp_sep:(fun _fret_nb fmt () -> Fmt.pf fmt "")
        fmt display_fret string;
      Fmt.pf fmt "@\n"

    let stringify_frets guitar_string = stringify display_frets guitar_string

    (* ************************************** *)
    (* ************************************** *)

    let display_box fmt (fret_nb, _note) =
      if fret_nb = 0 then begin
        Fmt.pf fmt "| "
      end;
      if fret_nb < 11 then Fmt.pf fmt "|      |" else ()

    let display_line fmt (_string_nb, string) =
      iterate_over_string
        ~pp_sep:(fun _fret_nb fmt () -> Fmt.pf fmt "")
        fmt display_box string;
      Fmt.pf fmt "@\n"

    let stringify_interline guitar_string = stringify display_line guitar_string

    (* ************************************** *)
    (* ************************************** *)
  end
end

module OCAMUSE = struct
  open Fmt
  open Types

  let pp_mode fmt = function
    | C_mode -> pf fmt "C_mode"
    | D_mode -> pf fmt "D_mode"
    | E_mode -> pf fmt "E_mode"
    | F_mode -> pf fmt "F_mode"
    | G_mode -> pf fmt "G_mode"
    | A_mode -> pf fmt "A_mode"
    | B_mode -> pf fmt "B_mode"

  let pp_base_colour fmt = function
    | Black -> pf fmt "Black"
    | Lblack -> pf fmt "Lblack"
    | White -> pf fmt "White"
    | Lwhite -> pf fmt "Lwhite"

  let pp_view fmt = function
    | Plain base_colour -> pf fmt "%a" pp_base_colour base_colour
    | Fretted base_colour -> pf fmt "%a" pp_base_colour base_colour
    | Interline base_colour -> pf fmt "%a" pp_base_colour base_colour

  let pp_display_mode fmt dm =
    let aux fmt = function
      | Flat view -> pf fmt "View:&&&Flat:%a" pp_view view
      | Pattern (view, mode) ->
        pf fmt "View:&&&Pattern:%a&&&Mode:%a" pp_view view pp_mode mode
    in
    pf fmt "DISPLAY:%a" aux dm

  let pp_tuning fmt n_l =
    list ~sep:(fun fmt () -> string fmt "=") NOTES.FMT.print_note fmt n_l

  let context fmt { display_mode; base_colour; tuning; root_note; mode; _ } =
    pf fmt "%a|||" pp_display_mode !display_mode;
    pf fmt "%a|||" pp_base_colour !base_colour;
    pf fmt "%a|||" pp_mode mode;
    pf fmt "%a|||" pp_tuning tuning;
    pf fmt "%a|||" NOTES.FMT.print_note root_note;
    pf fmt "@\n"
end
