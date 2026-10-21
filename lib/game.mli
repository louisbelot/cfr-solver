type player = P0 | P1

val other : player -> player
val sign : player -> float
val player_to_string : player -> string

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
