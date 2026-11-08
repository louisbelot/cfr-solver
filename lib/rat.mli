type t = private { num : Bigint.t; den : Bigint.t }

val zero : t
val one : t
val make : Bigint.t -> Bigint.t -> t
val of_int : int -> t
val of_ints : int -> int -> t
val of_float : float -> t
val recover : ?max_den:int -> float -> t
val to_float : t -> float
val to_string : t -> string
val sign : t -> int
val is_zero : t -> bool
val neg : t -> t
val inv : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
val pp : Format.formatter -> t -> unit
