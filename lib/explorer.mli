type request =
  | Create of {
      id : int;
      game : string;
      algorithm : string;
      sampling : string;
      seed : int;
      last : bool;
    }
  | Step of { id : int; budget : float; target : int }
  | Table of { id : int }
  | History of { id : int; key : string }
  | Deal of { id : int; seat : int; seed : int }
  | Act of { id : int; action : string }
  | Drop of { id : int }

type t

val create : now:(unit -> float) -> t
val handle : t -> request -> Json.t
