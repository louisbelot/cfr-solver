type entry = { actions : string array; probs : float array }
type t

val empty : t
val add : string -> actions:string array -> probs:float array -> t -> t
val find : t -> string -> entry option
val probs : t -> string -> n:int -> float array
val union : t -> t -> t
val bindings : t -> (string * entry) list
val cardinal : t -> int
val pp : ?keys:string list -> Format.formatter -> t -> unit
val to_json : ?keys:string list -> t -> Json.t
