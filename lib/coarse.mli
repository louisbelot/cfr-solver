type t

val create : ?plus:bool -> ?rng:Rng.t -> Multi_tree.t -> t
val iterate : t -> unit
val run : t -> int -> unit
val iterations : t -> int
val current : t -> float array
val average : t -> float array
val mixture : t -> Correlated.t
