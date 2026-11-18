type stats = { welfare : float; off : float; payment : float }
type t

val create :
  ?alpha:(int -> float) -> ?pay:float -> Multi_tree.t -> direct:float array -> t

val evaluate : t -> alpha:float -> float array -> stats
val iterate : t -> unit
val run : t -> int -> unit
val iterations : t -> int
val last : t -> stats
val average : t -> stats
val current : t -> float array
