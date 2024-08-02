(** module COLOR is destined to hold all functions and operations on displaying
    a colourful output using termcaps for now *)
module COLOR = struct
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

module NOTES = struct
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

module FRETBOARD = struct
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
  (* ************************************** *)

  (* ************************************** *)
  (*         display notes plainly          *)
  (* ************************************** *)
  let plain fmt arr =
    let display_plain_notes fmt (_string_nb, string) =
      plain_array_iteri
        ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "-")
        fmt
        (fun fmt (_i, v) -> NOTES.print_note fmt v)
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
  (* fretted keyboard print iteri functions *)
  (* ************************************** *)
  let fret_print_iteri ~pp_sep iteri ppf pp_v arr
      (range, (need_string_nb : bool)) =
    iteri
      (fun i v ->
          if need_string_nb then Format.fprintf ppf "%d" (i + 1);
          pp_v ppf (i, v);
          if i < range - 1 then pp_sep i ppf () )
      arr

  let fret_array_iteri ~pp_sep fmt ppv arr (string_nb : bool) =
    let range = Array.length arr in
    fret_print_iteri ~pp_sep Array.iteri fmt ppv arr (range, string_nb)

  let fret_matrix_iteri ~pp_sep fmt ppv arr =
    fret_array_iteri ~pp_sep fmt ppv arr true
  (* ************************************** *)
  (* ************************************** *)


  (* ************************************** *)
  (*     display frets of the keyboards     *)
  (* ************************************** *)
  let display_fret fmt (fret_nb, note) =
    if fret_nb = 0 then Format.fprintf fmt "|%a" NOTES.print_note note
    else
      match Conv.note_to_int note with
      | 1 | 4 | 6 | 9 | 11 ->
        Format.fprintf fmt {||--%a--||} NOTES.print_note note
      | _n -> Format.fprintf fmt {||--%a---||} NOTES.print_note note

  let display_frets fmt (_string_nb, string) =
    fret_array_iteri
      ~pp_sep:(fun _fret_nb fmt () -> Format.fprintf fmt "")
      fmt display_fret string false

  let print_line_of_frets_numbers fmt arr =
    Array.iteri
      (fun i _v ->
          if i = 0 then Format.fprintf fmt "  0 "
          else if i < 10 then Format.fprintf fmt " %2d     " i
          else Format.fprintf fmt "  %2d    " i )
      arr;
    Format.pp_print_newline fmt ()

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

end

module DISPLAY = struct

  open LTerm_text
  open LTerm_geom
  open LTerm_key
  open LTerm_draw

  let fretboard_with_frets ctx size view fretboard =
    let offset_of_sub_context = 2 in
    let ctx =
      LTerm_draw.sub ctx
        {
          row1 = offset_of_sub_context;
          col1 = offset_of_sub_context;
          row2 = size.rows - 1;
          col2 = size.cols - 1;
        }
    in
    let color =
      let open Types in
      let open LTerm_style in
      match view with
      | Up -> default
      | Down -> lblue
      | Left -> lgreen
      | Right -> lred
    in
    let fill_matrix_with_fretboard () =
      let offset_for_frets = 1 in
      let fmt = Format.str_formatter in
      Array.iteri (fun string_nb guitar_string ->
        if string_nb = 0 then
          begin
            FRETBOARD.print_line_of_frets_numbers fmt fretboard.(0);
            let fret_line = Format.flush_str_formatter () in
            draw_styled ctx 0 0
              (eval [B_fg color ; S fret_line; E_fg]);
          end;
        FRETBOARD.display_frets fmt (string_nb, guitar_string);
        let string_line =  Format.flush_str_formatter () in
        draw_styled ctx (string_nb + offset_for_frets) offset_for_frets
          (eval [B_fg color; S string_line; E_fg])
      ) fretboard
      (*  LTerm_draw.draw_styled ctx 0 0 (eval [B_fg LTerm_style.lblue; S s; E_fg]) *)
    in fill_matrix_with_fretboard ()

end

module Tones = struct end

module Chord_diagrams = struct end
