module type S = sig
  type t

  val zero : t
  val one : t
  val of_int : int -> t
  val of_float : float -> t
  val of_string : string -> t
  val to_string : t -> string
  val to_float : t -> float
  val sign : t -> int
  val neg : t -> t
  val abs : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div_rem : t -> t -> t * t
  val div : t -> t -> t
  val rem : t -> t -> t
  val gcd : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val numbits : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Make (_ : sig
  val bits : int
end) : S

include S
