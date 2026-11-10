type t = private { rows : int; cols : int; v : float array; g : float array }
type tape

val tape : unit -> tape
val inference : unit -> tape
val reset : tape -> unit
val param : int -> int -> float array -> t
val const : int -> int -> float array -> t
val shadow : t -> t
val zero_grad : t -> unit
val matmul : tape -> t -> t -> t
val add_bias : tape -> t -> t -> t
val relu : tape -> t -> t

val mse :
  tape -> t -> target:float array -> mask:float array -> weight:float array -> t

val softmax : mask:float array -> t -> float array

val cross_entropy :
  tape -> t -> target:float array -> mask:float array -> weight:float array -> t

val backward : tape -> t -> unit
