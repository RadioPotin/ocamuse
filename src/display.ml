(** module COLOR is destined to hold all functions and operations on displaying
      a colourful output *)
module COLOR : sig
  val event_to_color_flat_view : Types.view -> LTerm_style.color
  val bubble_color : Types.view -> Types.color_plain_view_event
  val rotate_to_prev : Types.color_plain_view_event -> Types.color_plain_view_event
  val rotate_to_next : Types.color_plain_view_event -> Types.color_plain_view_event
end
= struct

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
    | Black ->  Lwhite
    | Red -> Lblack
    | Green -> Lred
    | Yellow -> Lgreen
    | Blue -> Lyellow
    | Magenta -> Blue
    | Cyan -> Magenta
    | White -> Cyan
    | Lblack -> White
    | Lred -> Lblack
    | Lgreen -> Lred
    | Lyellow -> Lgreen
    | Lblue -> Lyellow
    | Lmagenta -> Lblue
    | Lcyan -> Lmagenta
    | Lwhite -> Lcyan

  let rotate_to_next =
    let open Types in
    function
    | Black -> Lred
    | Red -> Lgreen
    | Green -> Lyellow
    | Yellow -> Blue
    | Blue -> Magenta
    | Magenta -> Cyan
    | Cyan -> White
    | White -> Lblack
    | Lblack -> Lred
    | Lred -> Lgreen
    | Lgreen -> Lyellow
    | Lyellow -> Lblue
    | Lblue -> Lmagenta
    | Lmagenta-> Lcyan
    | Lcyan -> Lwhite
    | Lwhite -> Lblack

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
end


module MATRIX = struct
  module DRAW = struct
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

  module CELL = struct

    (* This module will hold all functions that aim to write a PP fret directly to the keyboard according to view *)

     (*
+      For now, the deal is to implement displays similar to the ones provided
+      by the Plain, Fretted and Interline view modes.
+
+      Instead of [LTerm_draw.draw_styled], we would use the [LTerm_draw.draw_char] to achieve that.
+
+      One way to go would be to print each fretboard view once and save
+      the context-world coordinates of each important note to a Hashtbl with
+      their relative role (Root, Tonic, Major 3rd, etc) for the given type of
+      Pattern then mapping a given role, in the context to a specific color to
+      go style directly in the context.
+
+      We could then later aim at making the color selection customisable that way
+    *)

  end


  module PATTERNS = struct

    (* This module will hold all functions that aim to build and highlight a
          +       given pattern on the fretboard *)


    let pattern _ctx _size _pattern _fb = ()

    let fretboard _ctx _size pattern _fb =
      let open Types in
      match pattern with
      | C_mode | _ -> failwith "PATTERNS.fretboard"
  end
end
