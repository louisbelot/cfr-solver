type t
type concept = Efce | Efcce | Nfcce

val create : Treeplex.t -> t
val add : t -> float array -> unit
val record : t -> float array -> float array array -> float array -> unit
val size : t -> int
val gain : t -> int -> float
val coarse_gain : t -> int -> float
val gains : ?concept:concept -> t -> float array
val gap : ?concept:concept -> t -> float
val values : t -> float array
