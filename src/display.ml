(** module COLOR is destined to hold all functions and operations on displaying
      a colourful output *)
module COLOR : sig
  val event_to_color_full_view : (Types.view -> LTerm_style.color)
end
= struct
  let event_to_color =
    let open LTerm_style in
    let open Types in
    function
    | Up -> lblue
    | Left -> lred
    | Right -> lgreen
    | Down -> lcyan

  let event_to_color_full_view =
    let open LTerm_style in
    let open Types in
    function
    | Fretted event -> event_to_color event
    | Plain event -> event_to_color event
    | Interline event -> event_to_color event
    | _ -> assert false
end

module DRAW = struct

  module MATRIX = struct
    open Types
    open LTerm_text

    let write_interline struc =
      let open LTerm_draw in
      let guitar_string = struc.guitar_string in
      let string_line =
        Pp.FRETBOARD.FMT.stringify_interline (struc.string, !guitar_string)
      in
      draw_styled struc.ctx
        !(struc.cursor_i)
        !(struc.offset)
        (eval [B_fg struc.color; S string_line; E_fg])

    let write_frets struc =
      let open LTerm_draw in
      let guitar_string = struc.guitar_string in
      let string_line =
        Pp.FRETBOARD.FMT.stringify_frets (struc.string, !guitar_string)
      in
      draw_styled struc.ctx
        !(struc.cursor_i)
        !(struc.offset)
        (eval [B_fg struc.color; S string_line; E_fg])

    let write_fret_numbers struc =
      let guitar_string = struc.guitar_string in
      let open LTerm_draw in
      let fret_line =
        Pp.FRETBOARD.FMT.stringify_frets_numbers
          (struc.string, !guitar_string)
      in
      draw_styled struc.ctx
        !(struc.cursor_i)
        !(struc.offset)
        (eval [B_fg struc.color ; S fret_line; E_fg])

    let write_plain_frets struc =
      let open LTerm_draw in
      let string_line =
        Pp.FRETBOARD.FMT.stringify_plain_string (!(struc.string), !(struc.guitar_string))
      in
      draw_styled struc.ctx
        !(struc.cursor_i)
        !(struc.offset)
        (eval [B_fg struc.color; S string_line; E_fg])

    (* ***************************** *)
    (* main current writing function *)
    (* ***************************** *)
    let make_struc ctx fretboard color  =
      let offset_for_frets_numbers = 4 in
      let number_of_strings = Array.length fretboard in
      let cursor_i = ref 0 in
      let offset = ref offset_for_frets_numbers in
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

    let write_rows_with_interlines ctx fretboard color =
      let struc = make_struc ctx fretboard color in
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

    let write_plain_rows ctx fretboard color =
      let struc = make_struc ctx fretboard color in
      let update_field field i = field := i in
      let move_cursor struc =
        update_field struc.cursor_i (!(struc.cursor_i) + 1)
      in
      let i = ref 0 in
      while !i < struc.number_of_strings do
        write_plain_frets struc;
        move_cursor struc;
        update_field struc.string !i;
        update_field struc.guitar_string struc.fretboard.(!i) ;
        i := !i + 1
      done

    let write_rows_with_no_interline ctx fretboard color =
      let struc = make_struc ctx fretboard color in
      Array.iteri (fun string_nb string ->
        let offset = !(struc.offset) in
        if string_nb = 0 then
          begin
            let fret_line =
              Pp.FRETBOARD.FMT.stringify_frets_numbers (string_nb, string)
            in
            LTerm_draw.draw_styled struc.ctx
              (offset - 1)
              offset
              (eval [B_fg struc.color ; S fret_line; E_fg])
          end;
        let string_line =
          Pp.FRETBOARD.FMT.stringify_frets (string_nb, string)
        in
        LTerm_draw.draw_styled struc.ctx
          (string_nb + offset)
          offset
          (eval [B_fg struc.color; S string_line; E_fg]);
      ) struc.fretboard
  end

  let select_display_mode event ctx fretboard color =
    match event with
    | Types.Fretted _ ->
      MATRIX.write_rows_with_no_interline ctx fretboard color
    | Types.Interline _ ->
      MATRIX.write_rows_with_interlines ctx fretboard color
    | Types.Plain _ ->
      MATRIX.write_plain_rows ctx fretboard color
    | Pattern _ ->
      MATRIX.write_rows_with_interlines ctx fretboard color

  let fretboard_with_frets ctx view fretboard =
      (*
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
    *)
    let color = COLOR.event_to_color_full_view view in
    let display_mode =
      select_display_mode view
    in
    display_mode ctx fretboard color

  module PATTERNS = struct

    let pattern _ctx _size _pattern _fb = ()

    let fretboard _ctx _size pattern _fb =
      let open Types in
      match pattern with
      | C_mode | _ -> failwith "PATTERNS.fretboard"
  end
end
