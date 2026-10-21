type node =
  | Terminal of float
  | Chance of { probs : float array; children : node array }
  | Decision of { infoset : int; children : node array }

type infoset = { key : string; player : Game.player; actions : string array }
type t = { root : node; infosets : infoset array }

val build : (module Game.S) -> t
