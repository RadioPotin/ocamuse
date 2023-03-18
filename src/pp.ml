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

let print_note =
  let open Types in
  fun fmt { base; alteration } ->
    Format.fprintf fmt "%a%a" print_base_note base print_alteration alteration

let print_notes fmt notes =
  Format.fprintf fmt "- %a@\n"
    (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
        print_note) notes

let print_chord fmt =
  let open Types in
  fun (chord : diatonic_triad) ->
    match chord with
    | Major ->
      Format.fprintf fmt ""
    | Minor ->
      Format.fprintf fmt "m"
    | Diminished ->
      Format.fprintf fmt "dim"


let print_chord =
  let open Types in
  fun fmt ({base; alteration}, chord) ->
    Format.fprintf fmt "%a%a%a" print_base_note base print_alteration alteration print_chord chord

let print_chords fmt chords =
  Format.fprintf fmt "- %a@\n"
    (Format.pp_print_list
        ~pp_sep:(fun fmt ()-> Format.fprintf fmt " ")
        print_chord) chords
