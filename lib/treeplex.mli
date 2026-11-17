type t = private {
  tree : Multi_tree.t;
  parent : int array;
  below : int array array;
  order : int array;
  subtree : int array array;
  local : int array array;
}

val build : Multi_tree.t -> t
val infoset : t -> int -> int
val actions : t -> int -> int
val plans : t -> float array -> float array
val gradients : t -> float array -> float array array * float array
val sums : t -> float array array -> float array -> float array
val sample : t -> Rng.t -> float array -> float array
