(** Two-player zero-sum games with imperfect information. *)

type player = P0 | P1

val other : player -> player

val sign : player -> float
(** [1.] for [P0], [-1.] for [P1]. *)

val player_to_string : player -> string

(** What happens at a history.

    - [Terminal u]: the game is over and player 0 wins [u] (player 1 wins [-u]).
    - [Chance outcomes]: nature moves; the probabilities sum to 1.
    - [Decision]: [player] acts. [infoset] names everything [player] knows here,
      so two histories with the same key must be indistinguishable to them and
      offer the same action labels in the same order. Keys are global: the two
      players never share one. *)
type 'h node =
  | Terminal of float
  | Chance of (float * 'h) list
  | Decision of {
      player : player;
      infoset : string;
      actions : (string * 'h) list;
    }

module type S = sig
  type history

  val name : string
  val root : history
  val node : history -> history node
end
