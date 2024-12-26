(** module COLOR is destined to hold all functions and operations on displaying
      a colourful output *)
let bubble_color =
  let open Types in
  begin
    function
    | Plain view_color -> view_color
    | Fretted view_color -> view_color
    | Interline view_color -> view_color
  end

let rotate_to_prev =
  let open Types in
  function
  | Lwhite -> Black
  | Lblack -> Lwhite
  | White -> Lblack
  | Black -> White

let rotate_to_next =
  let open Types in
  function
  | Lwhite -> Lblack
  | Lblack -> White
  | White -> Black
  | Black -> Lwhite

let event_to_base_color =
  let open LTerm_style in
  let open Types in
  function
  | Lwhite -> lwhite
  | Lblack -> lblack
  | White -> white
  | Black -> black

let event_to_color =
  let open LTerm_style in
  let open Types in
  function
  | Red -> red
  | Green -> green
  | Yellow -> yellow
  | Blue -> blue
  | Magenta -> magenta
  | Cyan -> cyan
  | Lred -> lred
  | Lgreen -> lgreen
  | Lyellow -> lyellow
  | Lblue -> lblue
  | Lmagenta -> lmagenta
  | Lcyan -> lcyan

let event_to_color_flat_view =
  let open Types in
  function
  | Fretted event ->  event_to_base_color event
  | Plain event ->    event_to_base_color event
  | Interline event -> event_to_base_color event

let is_equal color colour : bool =
  let open LTerm_style in
  let s1 = {none with foreground = Some color} in
  let s2 = {none with foreground = Some colour} in
  LTerm_style.equal s1 s2

let find_color struc note =
  let open Types in
  match Hashtbl.find_opt struc.notes_to_degree_tbl note with
  | None -> struc.color
  | Some degree ->
    Hashtbl.find struc.degree_to_color_tbl degree

let random_base_colour =
  let open Types in
  let () = Random.self_init ()
  in fun () : Types.base_colour ->
    match Random.int 4 with
    | 0 -> Lblack
    | 1 -> Black
    | 2 -> White
    | 3 -> Lwhite
    | _n -> assert false
