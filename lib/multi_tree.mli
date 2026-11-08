type node =
  | Leaf of float array
  | Chance of float array * node array
  | Decision of int * node array

type t = {
  root : node;
  size : int;
  height : int;
  width : int;
  keys : string array;
  labels : string array array;
  owner : int array;
  offset : int array;
}

val build : (module Multi.S) -> t
val slots : t -> int
