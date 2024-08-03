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

  module FMT = struct
    (* ************************************** *)
    (** plain fretboard print iteri functions *)
    (* ************************************** *)

    let fret_print_iteri ~pp_sep ppf pp_v arr range =
      Array.iteri
        (fun i v ->
            if i = 0 then
              Format.fprintf ppf "%d" (i + 1);
            pp_v ppf (i, v);
            if i < range - 1 then pp_sep i ppf ()
        )
        arr

    let iterate_over_string ~pp_sep fmt ppv arr =
      let range = Array.length arr in
      fret_print_iteri ~pp_sep fmt ppv arr range

    let fret_matrix_iteri ~pp_sep fmt ppv arr =
      iterate_over_string ~pp_sep fmt ppv arr

    (* ************************************** *)
    (*   General Purpose e.g. fmt conversion  *)
    (* ************************************** *)

    let stringify f arg =
      Format.asprintf "%a" f arg

    (* ************************************** *)
    (* ************************************** *)

    let print_top_numbers fmt (fret_nb, _note) =
      if fret_nb = 0 then Format.fprintf fmt "||-"
      else
        Format.fprintf fmt {||------||}

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
      if fret_nb = 0 then
        Format.fprintf fmt "||-";
      Format.fprintf fmt "|------|"

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

module DISPLAY = struct

  module FMT = struct
    open LTerm_text
    open LTerm_geom

    type struc =
      {
        string: int ref;
        offset : int ref;
        cursor_i : int ref;
        number_of_strings: int;
        ctx : LTerm_draw.context;
        color : LTerm_style.color;
        guitar_string : Types.note array;
      }

    let write_interline struc =
      let open LTerm_draw in
      let string_line =
        FRETBOARD.FMT.stringify_interline (0, struc.guitar_string)
      in
      let cursor_i = struc.cursor_i in
      let offset = struc.offset in
      draw_styled struc.ctx
        (!cursor_i + !offset)
        0
        (eval [B_fg struc.color; S string_line; E_fg])

    let write_frets struc =
      let open LTerm_draw in
      let string_line =
        FRETBOARD.FMT.stringify_frets (0, struc.guitar_string)
      in
      let cursor_i = struc.cursor_i in
      let offset = struc.offset in
      draw_styled struc.ctx
        (!cursor_i + !offset)
        !offset
        (eval [B_fg struc.color; S string_line; E_fg])

    let write_fret_numbers struc =
      let open LTerm_draw in
      let fret_line =
        FRETBOARD.FMT.stringify_frets_numbers (struc.string, struc.guitar_string)
      in
      let cursor_i = struc.cursor_i in
      draw_styled struc.ctx
        !cursor_i
        0
        (eval [B_fg struc.color ; S fret_line; E_fg])

    let write_first_row struc =
      write_fret_numbers struc

    let write_rows struc =
      write_first_row struc;
      let i = ref 1 in
      while !i < struc.number_of_strings do

        incr i
      done
      (*
      while !string_number < struc.number_of_strings do
        if !string_number = 0 then
          write_first_row struc;

        string_number := !string_number + 1
done
        *)

    let interline_display ctx color fretboard =
      (* top row of the display *)
      let offset_for_frets_numbers = 1 in
      let offset = offset_for_frets_numbers + 1 in
      (* run along strings *)
      let number_of_strings = Array.length fretboard in
      let cursor_i = ref 0 in
      let offset = ref offset in
      let struc =
        {
          ctx;
          color;
          offset;
          cursor_i;
          string = ref 0;
          number_of_strings;
          guitar_string = fretboard.(0);
        }
      in
      write_rows struc

    let no_interline_display ctx color fretboard =
      let offset_for_frets_numbers = 1 in
      Array.iteri (fun string_nb string ->
        if string_nb = 0 then
          begin
            let fret_line =
              FRETBOARD.FMT.stringify_frets_numbers (string_nb, string)
            in
            LTerm_draw.draw_styled ctx
              0
              0
              (eval [B_fg color ; S fret_line; E_fg]);
          end;
        let string_line =
          FRETBOARD.FMT.stringify_frets (string_nb, string)
        in
        LTerm_draw.draw_styled ctx
          (string_nb + offset_for_frets_numbers)
          offset_for_frets_numbers
          (eval [B_fg color; S string_line; E_fg])
      ) fretboard

    let simple_fretboard_with_frets ctx size view fretboard =
      let offset_of_sub_context = 1 in
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
        | Left -> lgreen,no_interline_display
        | Right -> lred, no_interline_display
        | Interline -> lred, interline_display
      in
      display_mode ctx color fretboard
  end

  module MATRIX = struct


  end

  module PATTERNS = struct

    let pattern _ctx _size _pattern _fb = ()

    let fretboard _ctx _size pattern _fb =
      let open Types in
      match pattern with
      | C_mode | _ -> failwith "PATTERNS.fretboard"

  end

end


module Tones = struct end

module Chord_diagrams = struct end
