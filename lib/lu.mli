type t

val factor : int -> (int * Rat.t) list array -> t option
val solve : t -> Rat.t array -> Rat.t array
val solve_t : t -> Rat.t array -> Rat.t array
