type t = { sizes : int array; u : float array array }

val matrix : Rat.t array array -> Rat.t array * Rat.t array * Rat.t
val count : int array -> int
val decode : int array -> int -> int array
val expected : t -> float array array -> float array
val project : float -> float array -> float array
val prd : ?iterations:int -> ?dt:float -> ?gamma:float -> t -> float array array
val fixation : alpha:float -> m:int -> float -> float
val alpharank : alpha:float -> m:int -> t -> float array
val marginals : t -> float array -> float array array
