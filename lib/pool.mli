type t

val create : unit -> t
val default : unit -> t
val run : t -> int -> (int -> unit) -> unit
val workers : t -> int
val shutdown : t -> unit
