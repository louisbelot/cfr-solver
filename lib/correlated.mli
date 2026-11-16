type t

val create : Treeplex.t -> t
val add : t -> float array -> unit
val record : t -> float array -> float array array -> float array -> unit
val size : t -> int
val gain : t -> int -> float
val gains : t -> float array
val gap : t -> float
val values : t -> float array
