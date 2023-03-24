let iteri_down_strings f l =
  let rec iteri_down_strings i f = function
    | [] -> ()
    | a :: l ->
      f i a;
      iteri_down_strings (i - 1) f l
  in
  iteri_down_strings (List.length l) f l

let get_next_fret_note =
  let open Types.Fretboard in
  function
  | A -> Ash_Bfl
  | Ash_Bfl -> B
  | B -> C
  | C -> Csh_Dfl
  | Csh_Dfl -> D
  | D -> Dsh_Efl
  | Dsh_Efl -> E
  | E -> F
  | F -> Fsh_Gfl
  | Fsh_Gfl -> G
  | G -> Gsh_Afl
  | Gsh_Afl -> A

let string_fret_to_notes_tbl = Hashtbl.create 512

let get_note =
  let open Types.Fretboard in
  fun string_and_fret_p : note option ->
    Hashtbl.find_opt string_fret_to_notes_tbl string_and_fret_p

let init_string =
  let open Types.Fretboard in
  fun (guitar_str_nb : int) (open_note : note) (range : int) : unit ->
    let rec aux_init_fb note fret_nb =
      if fret_nb = range then ()
      else Hashtbl.add string_fret_to_notes_tbl (guitar_str_nb, fret_nb) note;
      aux_init_fb (get_next_fret_note note) (fret_nb + 1)
    in
    aux_init_fb open_note 0

let init_fretboard =
  let open Types.Fretboard in
  fun ?(tuning : tuning = [ E; A; D; G; B; E ]) (range : int) ->
    iteri_down_strings
      (fun guitar_str_nb note -> init_string guitar_str_nb note range)
      tuning
