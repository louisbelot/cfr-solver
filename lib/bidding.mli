type config = {
  bids : int;
  rule : Auction.rule;
  ties : Auction.ties;
  prior : ((int * int) * Rat.t) list;
  value : int -> int -> int -> Rat.t;
}

val independent : int -> int -> ((int * int) * Rat.t) list
val affiliated : int -> ((int * int) * Rat.t) list
val private_value : int -> int -> int -> Rat.t
val wallet : int -> int -> int -> Rat.t
val signals : config -> int -> int list
val key : int -> int -> string

type history

val game : config -> (module Multi.S with type history = history)
val settle : config -> int * int -> int -> int -> Rat.t * Rat.t * Rat.t
val outcome : config -> (int -> int -> Rat.t array) -> Rat.t * Rat.t * Rat.t
val pure : config -> (int -> int -> int) -> int -> int -> Rat.t array
val naive : config -> int -> int -> int

val equilibrium :
  config -> Lemke.t * Lemke.solution * (int -> int -> Rat.t array)
