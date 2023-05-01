let mapi_down_strings mapi strings_l =
  let rec aux_mapi_down_strings strings_mapiacc string_number string_ =
    match string_ with
    | [] -> List.rev strings_mapiacc
    | x :: r ->
      let mapiacc = mapi string_number x in
      aux_mapi_down_strings (mapiacc :: strings_mapiacc) (string_number - 1) r
  in
  let total_nb_of_strings = List.length strings_l - 1 in
  let strings = aux_mapi_down_strings [] total_nb_of_strings strings_l in
  Array.of_list strings

let get_next_fret_note open_string fret =
  let note_id = Conv.note_to_int open_string + fret in
  Conv.int_to_note Sharp note_id

let coord_to_note_tbl = Hashtbl.create 512

let register nb_s range open_note =
  let note = ref open_note in
  for i = 0 to range - 1 do
    Hashtbl.replace coord_to_note_tbl (i, nb_s) !note;
    note := get_next_fret_note open_note i
  done

let map nb_s range open_note =
  Array.init range (fun i ->
      match Hashtbl.find_opt coord_to_note_tbl (i + 1, nb_s) with
      | None -> open_note
      | Some note -> note )

let register_and_map_string nb_s range open_note =
  register nb_s range open_note;
  map nb_s range open_note

let init_string =
  let open Types in
  fun (guitar_str_nb : int) (range : int) (open_string : note) ->
    register_and_map_string guitar_str_nb range open_string

let init =
  let open Types in
  (* range is the number of frets on the board *)
  (* tuning is a list of notes to start a string from *)
  fun ~(tuning : tuning) ?(range : int = 13) () : note array array ->
    mapi_down_strings
      (fun guitar_str_nb open_string ->
        init_string guitar_str_nb range open_string )
      tuning
