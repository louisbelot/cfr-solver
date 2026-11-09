type order = Descending | Ascending | Random
type payoff = Win_loss | Points
type config = { cards : int; order : order; payoff : payoff; reveal : bool }

val standard : config
val name : config -> string
val members : int -> int list

module Make (_ : sig
  val config : config
end) : Game.S

val game : config -> (module Game.S)

val profile :
  config ->
  (mine:int ->
  theirs:int ->
  deck:int ->
  upcard:int ->
  float array * float array) ->
  Strategy.t
