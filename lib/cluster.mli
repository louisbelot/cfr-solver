val optimal_1d : ?weights:float array -> float array -> int -> int array
val sse : ?weights:float array -> float array -> int array -> float
val emd : float array -> float array -> float
val median : (float * float) array -> float

val kmedians :
  ?weights:float array ->
  ?restarts:int ->
  Rng.t ->
  float array array ->
  int ->
  int array

val centers :
  ?weights:float array -> float array array -> int array -> float array array

val emd_cost : ?weights:float array -> float array array -> int array -> float
