type player = P0 | P1

let other = function P0 -> P1 | P1 -> P0
let sign = function P0 -> 1. | P1 -> -1.
let player_to_string = function P0 -> "player 0" | P1 -> "player 1"

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
