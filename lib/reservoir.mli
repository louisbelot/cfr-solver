type 'a t

val create : capacity:int -> Rng.t -> 'a t
val length : 'a t -> int
val seen : 'a t -> int
val add : 'a t -> 'a -> unit
val get : 'a t -> int -> 'a
val to_list : 'a t -> 'a list
