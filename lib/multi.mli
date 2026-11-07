type 'h node =
  | Terminal of float array
  | Chance of (float * 'h) list
  | Decision of { player : int; infoset : string; actions : (string * 'h) list }

module type S = sig
  type history

  val name : string
  val players : int
  val root : history
  val node : history -> history node
end

module Of_game (G : Game.S) : S with type history = G.history

module Eval (G : S) : sig
  val values : Strategy.t -> float array
  val infosets : int -> (string * string array) list
end
