type meta = Exact | Nash | Uniform

type step = {
  iteration : int;
  sizes : int * int;
  value : float;
  nash_conv : float;
}

val matrix : Rat.t array array -> Rat.t array * Rat.t array * Rat.t
val mixture : Tree.t -> Game.player -> (float * Strategy.t) list -> Strategy.t
val pure : Tree.t -> Game.player -> Strategy.t

val run :
  ?meta:meta -> (module Game.S) -> iterations:int -> step list * Strategy.t
