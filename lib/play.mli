type move = { player : Game.player; infoset : string; action : string }
type result = { payoff : float; moves : move list }

type state =
  | Turn of {
      infoset : string;
      actions : string list;
      moves : move list;
      resume : string -> state;
    }
  | Over of result

module Make (G : Game.S) : sig
  val start : rng:Rng.t -> seat:Game.player -> bot:Strategy.t -> state

  val hand :
    rng:Rng.t ->
    seat:Game.player ->
    bot:Strategy.t ->
    ask:(string -> string list -> string) ->
    result
end
