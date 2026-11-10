type t

val create : Rng.t -> int list -> t
val inputs : t -> int
val params : t -> Autodiff.t list
val replica : t -> t
val gather : t -> scale:float -> t -> unit
val size : t -> int
val forward : Autodiff.tape -> t -> Autodiff.t -> Autodiff.t
val predict : t -> rows:int -> float array -> float array

type adam

val adam :
  ?lr:float ->
  ?beta1:float ->
  ?beta2:float ->
  ?eps:float ->
  ?clip:float ->
  t ->
  adam

val update : adam -> t -> unit
