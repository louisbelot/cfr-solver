type t = Nearest | Arithmetic | Pseudo_harmonic

val all : t list
val name : t -> string
val of_string : string -> t option
val low : t -> a:float -> b:float -> float -> float
