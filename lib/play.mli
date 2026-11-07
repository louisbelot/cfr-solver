type move = { player : Game.player; infoset : string; action : string }
type result = { payoff : float; moves : move list }

module Make (G : Game.S) : sig
  val hand :
    rng:Rng.t ->
    seat:Game.player ->
    bot:Strategy.t ->
    ask:(string -> string list -> string) ->
    result
end
