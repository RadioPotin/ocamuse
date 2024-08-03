(** module COLOR is destined to hold all functions and operations on displaying
    a colourful output *)
module COLOR = struct
end

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
          Format.fprintf fmt {||--%a--||} NOTES.FMT.print_note note
        | _n -> Format.fprintf fmt {||--%a---||} NOTES.FMT.print_note note

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
        Format.fprintf fmt "||";
      end;
      if fret_nb < 11 then
        Format.fprintf fmt "|------|"
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

  module DEBUG = struct

    (*
    open Types

    let print_struc fmt {cursor_i; offset; number_of_strings; guitar_string; _;} =
      Format.fprintf fmt
        {| |} struc
    *)

  end

end

module DISPLAY = struct

  module MATRIX = struct
    open LTerm_text
    open LTerm_geom
    open Types

    let write_interline struc =
      let open LTerm_draw in
      let guitar_string = struc.guitar_string in
      let string_line =
        FRETBOARD.FMT.stringify_interline (struc.string, !guitar_string)
      in
      draw_styled struc.ctx
        !(struc.cursor_i)
        !(struc.offset)
        (eval [B_fg struc.color; S string_line; E_fg])

    let write_frets struc =
      let open LTerm_draw in
      let guitar_string = struc.guitar_string in
      let string_line =
        FRETBOARD.FMT.stringify_frets (struc.string, !guitar_string)
      in
      draw_styled struc.ctx
        !(struc.cursor_i)
        !(struc.offset)
        (eval [B_fg struc.color; S string_line; E_fg])

    let write_fret_numbers struc =
      let guitar_string = struc.guitar_string in
      let open LTerm_draw in
      let fret_line =
        FRETBOARD.FMT.stringify_frets_numbers
          (struc.string, !guitar_string)
      in
      draw_styled struc.ctx
        !(struc.cursor_i)
        !(struc.offset)
        (eval [B_fg struc.color ; S fret_line; E_fg])

    let write_rows struc =
      let update_field field i = field := i in
      let move_cursor struc =
        update_field struc.cursor_i (!(struc.cursor_i) + 1)
      in
      (* top fret numbers *)
      write_fret_numbers struc;
      move_cursor struc;
      (* interline *)
      write_interline struc;
      move_cursor struc;
      (* iterate over rest of strings *)
      write_frets struc;
      move_cursor struc;
      write_interline struc;
      move_cursor struc;
      let i = ref 1 in
      while !i < struc.number_of_strings do
        update_field struc.guitar_string struc.fretboard.(!i);
        (* frets and notes *)
        write_frets struc;
        move_cursor struc;
        update_field struc.string !i;
        (* interline *)
        write_interline struc;
        move_cursor struc;

        incr i
      done

    module PLAIN = struct

      let fretboard_with_frets ctx size view fretboard =

        (* plain fretted displayVy *)
        let no_interline_display ctx color fretboard =
          Array.iteri (fun string_nb string ->
            let offset = 4 in
            if string_nb = 0 then
              begin
                let fret_line =
                  FRETBOARD.FMT.stringify_frets_numbers (string_nb, string)
                in
                LTerm_draw.draw_styled ctx
                  (offset - 1)
                  offset
                  (eval [B_fg color ; S fret_line; E_fg])
              end;
            let string_line =
              FRETBOARD.FMT.stringify_frets (string_nb, string)
            in
            LTerm_draw.draw_styled ctx
              (string_nb + offset)
              offset
              (eval [B_fg color; S string_line; E_fg]);
          ) fretboard
        in

        (* interlined fretted display *)
        let interline_display ctx color fretboard =
          (* top row of the display *)
          let offset_for_frets_numbers = 4 in
          (* run along strings *)
          let number_of_strings = Array.length fretboard in
          let cursor_i = ref 0 in
          let offset = ref offset_for_frets_numbers in
          let struc =
            {
              ctx;
              color;
              offset;
              cursor_i;
              fretboard;
              string = ref 0;
              number_of_strings;
              guitar_string = ref fretboard.(0);
            }
          in
          write_rows struc

        in
        let offset_of_sub_context = 15 in
        let position =
          {
            row1 = offset_of_sub_context;
            col1 = offset_of_sub_context;
            row2 = size.rows - 1;
            col2 = size.cols - 1;
          }
        in
        let ctx = LTerm_draw.sub ctx position in
        let color, display_mode =
          let open Types in
          let open LTerm_style in
          match view with
          | Up -> default, no_interline_display
          | Down -> lblue, interline_display
          | Left -> lgreen, no_interline_display
          | Right -> lred, no_interline_display
          | Interline -> lred, interline_display
        in
        display_mode ctx color fretboard
    end

    module PATTERNS = struct

      let pattern _ctx _size _pattern _fb = ()

      let fretboard _ctx _size pattern _fb =
        let open Types in
        match pattern with
        | C_mode | _ -> failwith "PATTERNS.fretboard"

    end
  end
end


module Tones = struct end

module Chord_diagrams = struct end
