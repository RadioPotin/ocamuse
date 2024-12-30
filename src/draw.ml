module PATTERN = struct
  open Draw_plain
  open Draw_fretted
  open Draw_interline

  let pattern (struc : Types.pattern_view_draw_struc) =
    match struc.view with
    | Plain _ -> PLAIN.view struc
    | Fretted _ -> FRETTED.view struc
    | Interline _ -> INTERLINE.view struc
end
