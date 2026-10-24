type size = Pot of float | Chips of float | All_in
type street = { bets : size list; raises : size list; cap : int }
type config = { pot : float; stack : float; streets : street list }
type action = Fold | Check | Call | Bet of float

type node =
  | Decision of {
      player : int;
      pot : float;
      committed : float;
      to_call : float;
      all_in : float;
      actions : action array;
      children : node array;
    }
  | Chance of node
  | Folded of { folder : int; win : float }
  | Showdown of { win : float }

val limit : ?cap:int -> pot:float -> bet:float -> int -> config
val build : config -> node
val action_to_string : action -> string
val fraction : pot:float -> committed:float -> to_call:float -> action -> float

val fold :
  decision:(node -> 'a array -> 'a) ->
  chance:('a -> 'a) ->
  leaf:(node -> 'a) ->
  node ->
  'a

val count : node -> int
