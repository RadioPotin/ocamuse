type mode =
  | C_mode
  | D_mode
  | E_mode
  | F_mode
  | G_mode
  | A_mode
  | B_mode

type chord =
  |

type base_note =
  | A
  | B
  | C
  | D
  | E
  | F
  | G

type note = {
  base : base_note;
  alteration: int;
}

let mode_rotation_int =
  function
  | C_mode -> 0
  | D_mode -> 1
  | E_mode -> 2
  | F_mode -> 3
  | G_mode -> 4
  | A_mode -> 5
  | B_mode -> 6

let rec rotate_list_n l n =
  if n = 0 then
    l
  else
    match l with
    | [] -> []
    | x :: r ->
      rotate_list_n (r @ [x]) (n-1)

let intervals_of_mode =
  let base =[2; 2; 1; 2; 2; 2; 1]
  in fun mode ->
    let rotation_needed = mode_rotation_int mode in
    rotate_list_n base rotation_needed

let get_next_base_note_distance = function
  | C -> 2
  | D -> 2
  | E -> 1
  | F -> 2
  | G -> 2
  | A -> 2
  | B -> 1

let get_next_note = function
  | C -> D
  | D -> E
  | E -> F
  | F -> G
  | G -> A
  | A -> B
  | B -> C


let get_next_note {base;alteration} interval =
  let distance = get_next_base_note_distance base in
  { base = get_next_note base; alteration = interval - distance + alteration}

let rec build_tonality mode note =
  match mode with
  | [] -> [note]
  | i :: mode -> let next = get_next_note note i in
    note :: build_tonality mode next

let build_tonality mode note =
  let intervalic_formula = intervals_of_mode mode in
  build_tonality intervalic_formula note

let note_of_string s =
  if String.length s < 2 then
    invalid_arg "note_of_string"
  else
    let base =
      match s.[0] with
      | 'a' -> A
      | 'b' -> B
      | 'c' -> C
      | 'd' -> D
      | 'e' -> E
      | 'f' -> F
      | 'g' -> G
      | _ -> invalid_arg "note_of_string"
    in
    let sub = String.sub s 1 (String.length s - 1)
    in
    let alteration =
      match int_of_string_opt sub with
      | None -> failwith "invalid_arg"
      | Some s -> s
    in {base; alteration}

let mode_of_string = function
  | "A" -> A_mode
  | "B" -> B_mode
  | "C" -> C_mode
  | "D" -> D_mode
  | "E" -> E_mode
  | "F" -> F_mode
  | "G" -> G_mode
  | _ -> invalid_arg "mode_of_string"

let print_base_note fmt = function
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
  | n when n > 0 -> Format.fprintf fmt "#"; print_alteration fmt (n - 1)
  | _ -> Format.fprintf fmt "b"; print_alteration fmt (n + 1)

let print_note fmt {base; alteration} =
  Format.fprintf fmt "%a%a" print_base_note base print_alteration alteration

let print_notes fmt notes =
  Format.fprintf fmt "%a@\n"
    (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
        print_note) notes

(*
  TODO:
    * suite d'accords d'une tonalité donnée
    * Tabs
    * Markov Chord progressions
    * generer du midi
    * generer partoche pdf

 *)

let () =
  if Array.length Sys.argv <> 3 then
    failwith @@ Format.sprintf "Usage: %s <mode> <note>" Sys.argv.(0)
  else
    let mode = mode_of_string Sys.argv.(1) in
    let note = note_of_string Sys.argv.(2) in
    print_notes Format.std_formatter @@ build_tonality mode note
