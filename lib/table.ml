type t = {
  infosets : Tree.infoset array;
  player : Game.player array;
  offset : int array;
  regret : float array;
  strategy_sum : float array;
  current : float array;
  stamp : int array;
}

let create (tree : Tree.t) =
  let n = Array.length tree.infosets in
  let offset = Array.make (n + 1) 0 in
  Array.iteri
    (fun i (info : Tree.infoset) ->
      offset.(i + 1) <- offset.(i) + Array.length info.actions)
    tree.infosets;
  let slots = offset.(n) in
  {
    infosets = tree.infosets;
    player = Array.map (fun (info : Tree.infoset) -> info.player) tree.infosets;
    offset;
    regret = Array.make slots 0.;
    strategy_sum = Array.make slots 0.;
    current = Array.make slots 0.;
    stamp = Array.make n (-1);
  }

let copy t =
  {
    t with
    regret = Array.copy t.regret;
    strategy_sum = Array.copy t.strategy_sum;
    current = Array.copy t.current;
    stamp = Array.copy t.stamp;
  }

let slots t = Array.length t.regret

let discount_range t lo hi (pos, neg, avg) =
  if pos <> 1. || neg <> 1. then
    for k = lo to hi - 1 do
      let r = t.regret.(k) in
      t.regret.(k) <- (r *. if r > 0. then pos else neg)
    done;
  if avg <> 1. then
    for k = lo to hi - 1 do
      t.strategy_sum.(k) <- t.strategy_sum.(k) *. avg
    done

let discount t factors = discount_range t 0 (slots t) factors

let match_regrets t i =
  let base = t.offset.(i) and n = t.offset.(i + 1) - t.offset.(i) in
  let total = ref 0. in
  for a = base to base + n - 1 do
    total := !total +. Float.max t.regret.(a) 0.
  done;
  for a = base to base + n - 1 do
    t.current.(a) <-
      (if !total > 0. then Float.max t.regret.(a) 0. /. !total
       else 1. /. float_of_int n)
  done

let refresh t i stamp =
  if t.stamp.(i) <> stamp then begin
    match_regrets t i;
    t.stamp.(i) <- stamp
  end

let slice t a i = Array.sub a t.offset.(i) (t.offset.(i + 1) - t.offset.(i))

let to_strategy t f a =
  let s = ref Strategy.empty in
  Array.iteri
    (fun i (info : Tree.infoset) ->
      s :=
        Strategy.add info.key ~actions:info.actions ~probs:(f (slice t a i)) !s)
    t.infosets;
  !s

let average_strategy t = to_strategy t Regret_matching.normalize t.strategy_sum
let current_strategy t = to_strategy t Regret_matching.strategy t.regret

let slices t a =
  List.init (Array.length t.infosets) (fun i ->
      (t.infosets.(i).key, slice t a i))

let regrets t = slices t t.regret
let strategy_sums t = slices t t.strategy_sum
