type increment = Realized | Expected

type policy =
  | Equilibrium
  | Best_response
  | Rnr of float
  | Dbr of { pmax : float; conf : Restricted.confidence }
  | Budget of { grid : float list; increment : increment }

type config = { hands : int; refresh : int; iterations : int; prior : float }

type result = {
  realized : float;
  expected : float;
  baseline : float;
  low : float;
  exploiting : int;
}

val policy_name : policy -> string

val session :
  rng:Rng.t ->
  (module Game.S) ->
  exploiter:Game.player ->
  equilibrium:Strategy.t ->
  opponent:(int -> Strategy.t -> Strategy.t) ->
  config ->
  policy ->
  result

val adversary :
  (module Game.S) ->
  exploiter:Game.player ->
  bait:Strategy.t ->
  after:int ->
  int ->
  Strategy.t ->
  Strategy.t
