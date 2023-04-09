let mapi_down_strings mapi l =
  let rec iteri_down_strings acc i = function
    | [] -> acc
    | a :: l ->
      let init_s = mapi i a in
      iteri_down_strings (init_s :: acc) (i - 1) l
  in
  iteri_down_strings [] (List.length l) l

let string_fret_to_notes_tbl = Hashtbl.create 512

let get_note =
  let open Types in
  fun string_and_fret_p : note option ->
    Hashtbl.find_opt string_fret_to_notes_tbl string_and_fret_p

let get_next_fret_note alt open_string fret =
  let ( *** ) note fret = Conv.note_to_int note + fret in
  let note_id = open_string *** fret in
  Conv.int_to_note alt note_id

let init_string =
  let open Types in
  fun (guitar_str_nb : int) (open_note : note) (range : int) : note list ->
    let rec aux_init_fb acc note fret_nb =
      if fret_nb = range then begin
        List.rev acc
      end
      else begin
        Hashtbl.add string_fret_to_notes_tbl (guitar_str_nb, fret_nb) note;
        aux_init_fb (note :: acc)
          (get_next_fret_note Sharp open_note (fret_nb + 1))
          (fret_nb + 1)
      end
    in

    aux_init_fb [] open_note 0

let init_fretboard =
  let open Types in
  fun ~(tuning : tuning) ?(range : int = 14) () : note list list ->
    mapi_down_strings
      (fun guitar_str_nb note -> init_string guitar_str_nb note range)
      tuning
