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

    (* ************************************** *)
    (*         display notes plainly          *)
    (* ************************************** *)

    let print_plain_notes =
      let coord_to_note_tbl = Fretboard.coord_to_note_tbl in
      fun fmt ((fret_nb, (note:Types.note)), string_number)
        ->
          let is_in_bound k =
            match Hashtbl.find_opt coord_to_note_tbl k with
            | None -> false
            | Some _note -> true
          in
          let set_alteration_flag flag key =
            flag :=
              match Hashtbl.find_opt coord_to_note_tbl key with
              | None -> false
              | Some note -> note.alteration <> 0 || !flag

          in

          let scan_column ((_fret_nb, string_nb) as key ) =
            let i = ref 0 in
            let alteration_detected = ref false in
            let key = ref key in
            while is_in_bound !key do
              set_alteration_flag alteration_detected !key;
              i := !i + 1;
              key := !i, string_nb + 1
            done;
            !alteration_detected
          in
          let should_space = scan_column (fret_nb, string_number) in
          if should_space then
            if fret_nb < 11 then
              match Conv.note_to_int note with
              | 1 | 4 | 6 | 9 | 11 ->
                if should_space then
                  Format.fprintf fmt {|%a-|} NOTES.FMT.print_note note
                else
                  Format.fprintf fmt {|%a-|} NOTES.FMT.print_note note
              | _ ->
                if should_space then
                  Format.fprintf fmt {|%a--|} NOTES.FMT.print_note note
                else
                  Format.fprintf fmt {|%a|} NOTES.FMT.print_note note
            else
              Format.fprintf fmt {|%a|} NOTES.FMT.print_note note

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
      if fret_nb = 0 then Format.fprintf fmt " 0 "
      else if fret_nb < 10 then Format.fprintf fmt " %2d     " fret_nb
      else Format.fprintf fmt "  %2d    " fret_nb

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
