(** Shared drawing primitives to eliminate code duplication across draw modules *)

type offset_mode =
  | Standard
  | WithPadding
  | ForFrets
  | ForInterlines

(** Calculate row and column offsets based on offset mode *)
let get_offsets = function
  | Standard -> (0, 0)
  | WithPadding -> (-1, -1)
  | ForFrets -> (-2, -1)
  | ForInterlines -> (0, 0)

(** Draw a string at specified position with given style and offset mode *)
let draw_string ctx row col offset_mode style str =
  let row_offset, col_offset = get_offsets offset_mode in
  String.iteri
    (fun i c ->
      LTerm_draw.draw_char ctx (row + row_offset) (col + col_offset + i)
        (Zed_char.unsafe_of_char c)
        ~style )
    str

(** Cursor type for tracking drawing position *)
type cursor =
  { mutable row : int
  ; mutable col : int
  }

(** Create a new cursor at origin *)
let make_cursor () = { row = 0; col = 0 }

(** Advance cursor column by the length of a string *)
let advance_cursor cursor str = cursor.col <- cursor.col + String.length str

(** Reset cursor to start of next row *)
let next_row cursor =
  cursor.row <- cursor.row + 1;
  cursor.col <- 0

(** Update cursor position *)
let set_cursor cursor row col =
  cursor.row <- row;
  cursor.col <- col
