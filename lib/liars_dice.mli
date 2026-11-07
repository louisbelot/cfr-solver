type config = { dice : int; faces : int; wild : bool }

val standard : config
val name : config -> string
val bids : config -> int
val bid_to_string : config -> int -> string
val rolls : config -> (float * int list) list
val holds : config -> int list -> int list -> int -> bool

module Make (_ : sig
  val config : config
end) : Game.S

val game : config -> (module Game.S)
