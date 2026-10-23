type deck = { ranks : int; suits : int }

val standard : deck
val cards : deck -> int
val rank_name : deck -> int -> string

type abstraction = {
  name : string;
  round1 : int -> string;
  round2 : int -> int -> string;
}

val exact : deck -> abstraction

type history

val game :
  ?abstraction:abstraction ->
  deck ->
  (module Game.S with type history = history)

include Game.S with type history := history
