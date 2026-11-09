type confidence = Step of int | Linear of int | Curve of float

val confidence : confidence -> pmax:float -> Model.t -> string -> float

val game :
  ?p:float ->
  ?conf:(string -> float) ->
  (module Game.S) ->
  exploiter:Game.player ->
  model:Strategy.t ->
  (module Game.S)

val rnr :
  (module Game.S) ->
  exploiter:Game.player ->
  model:Strategy.t ->
  p:float ->
  (module Game.S)

val dbr :
  (module Game.S) ->
  exploiter:Game.player ->
  model:Strategy.t ->
  conf:(string -> float) ->
  (module Game.S)

val solve :
  ?algorithm:Cfr.algorithm ->
  iterations:int ->
  exploiter:Game.player ->
  (module Game.S) ->
  (module Game.S) ->
  Strategy.t

type evaluator = {
  against : Strategy.t -> Strategy.t -> float;
  worst : Strategy.t -> float;
  respond : Strategy.t -> Strategy.t;
}

val evaluator : (module Game.S) -> exploiter:Game.player -> evaluator
