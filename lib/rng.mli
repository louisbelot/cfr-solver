type t

val make : int -> t
val copy : t -> t
val split : t -> t
val next64 : t -> int64
val bits : t -> int
val int : t -> int -> int
val float : t -> float
val draw : t -> float array -> int
val pick : t -> float array -> int -> int -> int
