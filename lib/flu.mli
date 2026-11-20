type t

val drop : float
val factor : int -> (int * float) list array -> t option
val solve : t -> float array -> float array
val solve_t : t -> float array -> float array
val nonzeros : t -> int
