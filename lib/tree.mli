(** A game tree built once and kept in memory, with infosets numbered.

    [Game.S] describes a game lazily: every call to [node] rebuilds the children
    and the infoset key. Solvers that walk the whole tree thousands of times
    work on this instead, where a decision node holds its infoset's index and
    children are arrays. *)

type node =
  | Terminal of float
  | Chance of { probs : float array; children : node array }
  | Decision of { infoset : int; children : node array }

type infoset = { key : string; player : Game.player; actions : string array }

type t = {
  root : node;
  infosets : infoset array;
      (** Indexed by the numbers in [Decision] nodes, in depth-first order of
          first visit. *)
}

val build : (module Game.S) -> t
