type ship = { length : int; value : float }

type config = {
  height : int;
  width : int;
  ships : ship list;
  shots : int;
  repeat : bool;
  gamma : float;
}

val baseline : config
val name : config -> string
val placements : config -> int -> (string * int) list
val game : config -> (module Multi.S)
