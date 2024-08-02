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
    let plain_print_iteri ~pp_sep iteri ppf pp_v arr
        (range, (need_string_nb : bool)) =
      iteri
        (fun i v ->
            if need_string_nb then Format.fprintf ppf "%d" (i + 1);
            pp_v ppf (i, v);
            if i < range - 1 then pp_sep i ppf () )
        arr

    let plain_array_iteri ~pp_sep fmt ppv arr (string_nb : bool) =
      let range = Array.length arr in
      plain_print_iteri ~pp_sep Array.iteri fmt ppv arr (range, string_nb)

    let plain_matrix_iteri ~pp_sep fmt ppv arr =
      plain_array_iteri ~pp_sep fmt ppv arr true

    (* ************************************** *)
    (*         display notes plainly          *)
    (* ************************************** *)
    let plain fmt arr =
      let display_plain_notes fmt (_string_nb, string) =
        plain_array_iteri
          ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "-")
          fmt
          (fun fmt (_i, v) -> NOTES.FMT.print_note fmt v)
          string false
      in
      plain_matrix_iteri
        ~pp_sep:(fun _string_nb fmt () -> Format.pp_print_newline fmt ())
        fmt display_plain_notes arr;
      Format.pp_print_newline fmt ()

    let plain_stdout fretboard =
      let fmt = Format.std_formatter in
      plain fmt fretboard
    (* ************************************** *)
    (* ************************************** *)

    (* ************************************** *)
    (*     display frets of the keyboards     *)
    (* ************************************** *)
    let display_box fmt (fret_nb, _note) =
      if fret_nb = 0 then
        Format.fprintf fmt "||-";
      Format.fprintf fmt "|------|"

    let display_fret fmt (fret_nb, note) =
      if fret_nb = 0 then Format.fprintf fmt "|%a" NOTES.FMT.print_note note
      else
        match Conv.note_to_int note with
        | 1 | 4 | 6 | 9 | 11 ->
          Format.fprintf fmt {||--%a--||} NOTES.FMT.print_note note
        | _n -> Format.fprintf fmt {||--%a---||} NOTES.FMT.print_note note

    let print_line_of_frets_numbers fmt arr =
      Array.iteri
        (fun i _v ->
            if i = 0 then Format.fprintf fmt "  0 "
            else if i < 10 then Format.fprintf fmt " %2d     " i
            else Format.fprintf fmt "  %2d    " i )
        arr;
      Format.pp_print_newline fmt ()

    (* ************************************** *)

    (* ************************************** *)
    (* fretted keyboard print iteri functions *)
    (* ************************************** *)
    let fret_print_iteri ~pp_sep ppf pp_v arr (range, string_nb) =
      Array.iteri
        (fun i v ->
            if string_nb then Format.fprintf ppf "%d" (i + 1);
            pp_v ppf (i, v);
            if i < range - 1 then pp_sep i ppf ()
        )
        arr

    let fret_array_iteri ~pp_sep fmt ppv arr lines =
      let range = Array.length arr in
      fret_print_iteri ~pp_sep fmt ppv arr (range, lines)

    let fret_matrix_iteri ~pp_sep fmt ppv arr =
      fret_array_iteri ~pp_sep fmt ppv arr false

    (* ************************************** *)
    (*           uses no interline            *)
    (* ************************************** *)
    let display_frets fmt (_string_nb, string) =
      fret_array_iteri
        ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "")
        fmt display_fret string false

    let plain_frets fmt arr =
      print_line_of_frets_numbers fmt arr.(0);
      fret_matrix_iteri
        ~pp_sep:(fun _string_nb fmt () -> Format.pp_print_newline fmt ())
        fmt display_frets arr;
      Format.pp_print_newline fmt ()

    let frets_stdout fretboard : unit =
      let fmt = Format.std_formatter in
      plain_frets fmt fretboard
    (* ************************************** *)
    (* ************************************** *)

    (* ************************************************ *)
    (* fretted interline keyboard print iteri functions *)
    (* ************************************************ *)
    type interline =
      | Fret_nb of bool
      | Interline

    let fret_print_iteri_interline _pp_sep _ppf _pp_v arr _range =
      Array.iteri
        (fun _i _v ->
            ()
        )
        arr

    let fret_array_iteri_with_interline ~pp_sep fmt ppv arr =
      let range = Array.length arr in
      fret_print_iteri_interline pp_sep fmt ppv arr range

    let fret_matrix_iteri_with_interline ~pp_sep fmt ppv arr =
      fret_array_iteri_with_interline ~pp_sep fmt ppv arr

    (* ************************************** *)
    (*             uses interline             *)
    (* ************************************** *)

    let display_frets_and_interline fmt (_string_nb, string) =
      fret_array_iteri_with_interline
        ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "")
        fmt display_box string

    let plain_frets_with_interline fmt arr =
      print_line_of_frets_numbers fmt arr.(0);
      fret_matrix_iteri_with_interline
        ~pp_sep:(fun _string_nb fmt () -> Format.pp_print_newline fmt ())
        fmt display_frets_and_interline arr;
      Format.pp_print_newline fmt ()
    (* ************************************** *)
    (* ************************************** *)

    (* ************************************** *)
    (*   General Purpose e.g. fmt conversion  *)
    (* ************************************** *)
    let stringify f arg =
      Format.asprintf "%a" f arg

    let stringify_frets_numbers guitar_string =
      stringify print_line_of_frets_numbers guitar_string

    let display_frets fmt (_string_nb, string : int * Types.note array) =
      fret_array_iteri
        ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "")
        fmt display_fret string false

    let stringify_frets (guitar_string : int * Types.note array) = stringify display_frets guitar_string

    let display_line fmt ((_string_nb, string) : int * Types.note array) =
      fret_array_iteri
        ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "")
        fmt display_box string false

    let stringify_interline (guitar_string : int * Types.note array) = stringify display_line guitar_string
    (* ************************************** *)
    (* ************************************** *)

  end

end

module DISPLAY = struct

  module FMT = struct
    open LTerm_text
    open LTerm_geom
    open LTerm_draw

    let write_fret_numbers ctx color _offset cursor_i guitar_string =
      let draw =
        let fret_line =
          FRETBOARD.FMT.stringify_frets_numbers guitar_string
        in
        draw_styled ctx
          !cursor_i
          0
          (eval [B_fg color ; S fret_line; E_fg]);
      in
      incr cursor_i;
      draw

    let write_interline ctx color offset cursor_i guitar_string =
      let string_line =
        FRETBOARD.FMT.stringify_interline guitar_string
      in
      draw_styled ctx
        (!cursor_i + offset )
        offset
        (eval [B_fg color; S string_line; E_fg])

    let write_frets ctx color offset cursor_i guitar_string =
      let string_line =
        FRETBOARD.FMT.stringify_frets guitar_string
      in
      draw_styled ctx
        (!cursor_i + offset)
        offset
        (eval [B_fg color; S string_line; E_fg])



    (* Something wrong with write functions' back that does not type *)
    let write_first_row ctx color offset cursor_i guitar_string =
      write_fret_numbers ctx color offset cursor_i guitar_string;
      write_frets ctx color offset cursor_i guitar_string

    let write_row ctx color offset cursor_i string_nb guitar_string =
      if string_nb = 0 then
        write_first_row ctx color offset cursor_i guitar_string

    let interline_display ctx color fretboard =
      (* top row of the display *)
      let offset_for_frets_numbers = 1 in
      (* run along strings *)
      let nb_of_guitar_str = Array.length fretboard in
      let cursor_i = ref 0 in
      for i = 0 to nb_of_guitar_str do
        write_row ctx color offset_for_frets_numbers cursor_i i fretboard.(i);
        incr cursor_i
      done

    let no_interline_display ctx color fretboard =
      let offset_for_frets_numbers = 1 in
      Array.iteri (fun string_nb guitar_string ->
        if string_nb = 0 then
          begin
            let fret_line =
              FRETBOARD.FMT.stringify_frets_numbers guitar_string
            in
            draw_styled ctx
              0
              0
              (eval [B_fg color ; S fret_line; E_fg]);
          end;
        let string_line =
          FRETBOARD.FMT.stringify_frets (string_nb, guitar_string)
        in
        draw_styled ctx
          (string_nb + offset_for_frets_numbers)
          offset_for_frets_numbers
          (eval [B_fg color; S string_line; E_fg])
      ) fretboard


    let simple_fretboard_with_frets ctx size view fretboard =
      let offset_of_sub_context = 2 in
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
        | Down -> lblue, no_interline_display
        | Left -> lgreen, no_interline_display
        | Right -> lred, interline_display
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
