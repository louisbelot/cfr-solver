type t

val create : ?plus:bool -> Treeplex.t -> t
val play : t -> float array * float array
val update : t -> float array array -> unit
val current : t -> float array
val average : t -> float array
