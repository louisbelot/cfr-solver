(** Just enough JSON to write strategy files. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list

val to_string : t -> string
(** Indented output. Floats use the fewest digits that round-trip. Raises
    [Invalid_argument] on NaN or infinity, which JSON cannot represent. *)
