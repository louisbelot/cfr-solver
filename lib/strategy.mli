(** Behavioral strategies: a distribution over actions at each infoset.

    An infoset missing from the map is played uniformly at random, so [empty] is
    the uniform random strategy for every game. *)

type entry = { actions : string array; probs : float array }
type t

val empty : t

val add : string -> actions:string array -> probs:float array -> t -> t
(** Raises [Invalid_argument] if the two arrays differ in length. *)

val find : t -> string -> entry option

val probs : t -> string -> n:int -> float array
(** Action probabilities at an infoset with [n] actions. Do not mutate the
    result. Raises [Invalid_argument] if the stored entry has a different number
    of actions. *)

val union : t -> t -> t
(** Entries of the first strategy win on shared keys. Used to put the two
    players' halves of a profile together. *)

val bindings : t -> (string * entry) list
val cardinal : t -> int

val pp : ?keys:string list -> Format.formatter -> t -> unit
(** One line per infoset, in [keys] order (default: sorted). *)

val to_json : ?keys:string list -> t -> Json.t
(** An object from infoset to an object from action to probability, in [keys]
    order (default: sorted). Keys without an entry are left out. *)
