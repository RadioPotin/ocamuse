let mapi_down_strings mapi strings_l =
  let rec aux_mapi_down_strings strings_mapiacc string_number string_ =
    if string_number = 0 then strings_mapiacc
    else
      match string_ with
      | [] -> strings_mapiacc
      | x :: r ->
        let mapiacc = mapi string_number x in
        aux_mapi_down_strings (mapiacc :: strings_mapiacc) (string_number - 1) r
  in
  let total_nb_of_strings = List.length strings_l - 1 in
  let strings =
    List.rev @@ aux_mapi_down_strings [] total_nb_of_strings strings_l
  in
  Array.of_list strings

let init_string =
  let open Types in
  fun (_guitar_str_nb : int) (range : int) (open_string : note) : note array ->
    Array.make range open_string

let init =
  let open Types in
  fun ~(tuning : tuning) ?(range : int = 13) () : note array array ->
    (* range is the number of frets on the board *)
    (* tuning is a list of notes to start a string from *)
    mapi_down_strings
      (fun guitar_str_nb open_string ->
        init_string guitar_str_nb range open_string )
      tuning
