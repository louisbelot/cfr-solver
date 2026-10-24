type t = Nearest | Arithmetic | Pseudo_harmonic

let all = [ Nearest; Arithmetic; Pseudo_harmonic ]

let name = function
  | Nearest -> "nearest"
  | Arithmetic -> "arithmetic"
  | Pseudo_harmonic -> "pseudo-harmonic"

let of_string s = List.find_opt (fun m -> name m = s) all

let low m ~a ~b x =
  if not (a < b && a <= x && x <= b) then invalid_arg "Translation.low";
  match m with
  | Nearest -> if x -. a <= b -. x then 1. else 0.
  | Arithmetic -> (b -. x) /. (b -. a)
  | Pseudo_harmonic -> (b -. x) *. (1. +. a) /. ((b -. a) *. (1. +. x))
