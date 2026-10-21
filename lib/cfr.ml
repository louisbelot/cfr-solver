type algorithm =
  | Vanilla
  | Plus
  | Linear
  | Discounted of { alpha : float; beta : float; gamma : float }

let dcfr = Discounted { alpha = 1.5; beta = 0.; gamma = 2. }

let algorithm_name = function
  | Vanilla -> "cfr"
  | Plus -> "cfr+"
  | Linear -> "lcfr"
  | Discounted { alpha; beta; gamma } ->
      Printf.sprintf "dcfr(%g,%g,%g)" alpha beta gamma

type schedule = {
  alternating : bool;
  alpha : float;
  beta : float;
  gamma : float;
}

let schedule = function
  | Vanilla ->
      { alternating = false; alpha = infinity; beta = infinity; gamma = 0. }
  | Plus ->
      { alternating = true; alpha = infinity; beta = neg_infinity; gamma = 1. }
  | Linear -> { alternating = true; alpha = 1.; beta = 1.; gamma = 1. }
  | Discounted { alpha; beta; gamma } ->
      { alternating = true; alpha; beta; gamma }

let factor e t =
  if e = infinity then 1.
  else if e = neg_infinity then 0.
  else
    let x = Float.pow (float_of_int t) e in
    x /. (x +. 1.)

module Make (G : Game.S) = struct
  type t = {
    tree : Tree.t;
    player : Game.player array;
    offset : int array;
    regret : float array;
    strategy_sum : float array;
    current : float array;
    stamp : int array;
    values : float array;
    width : int;
    schedule : schedule;
    mutable iterations : int;
    mutable passes : int;
    mutable weight : float;
  }

  let rec shape = function
    | Tree.Terminal _ -> (0, 0)
    | Chance { children; _ } ->
        Array.fold_left
          (fun (d, w) c ->
            let d', w' = shape c in
            (max d d', max w w'))
          (0, 0) children
    | Decision { children; _ } ->
        Array.fold_left
          (fun (d, w) c ->
            let d', w' = shape c in
            (max d (d' + 1), max w w'))
          (0, Array.length children)
          children

  let create ?(algorithm = Vanilla) () =
    let tree = Tree.build (module G) in
    let n = Array.length tree.infosets in
    let offset = Array.make (n + 1) 0 in
    Array.iteri
      (fun i (info : Tree.infoset) ->
        offset.(i + 1) <- offset.(i) + Array.length info.actions)
      tree.infosets;
    let slots = offset.(n) in
    let depth, width = shape tree.root in
    {
      tree;
      player =
        Array.map (fun (info : Tree.infoset) -> info.player) tree.infosets;
      offset;
      regret = Array.make slots 0.;
      strategy_sum = Array.make slots 0.;
      current = Array.make slots 0.;
      stamp = Array.make n (-1);
      values = Array.make (depth * width) 0.;
      width;
      schedule = schedule algorithm;
      iterations = 0;
      passes = 0;
      weight = 1.;
    }

  let iterations t = t.iterations

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

  let refresh t i =
    if t.stamp.(i) <> t.passes then begin
      match_regrets t i;
      t.stamp.(i) <- t.passes
    end

  let rec walk t updating node depth pi0 pi1 pc =
    match node with
    | Tree.Terminal u -> u
    | Chance { probs; children } ->
        let v = ref 0. in
        for k = 0 to Array.length children - 1 do
          let p = probs.(k) in
          v := !v +. (p *. walk t updating children.(k) depth pi0 pi1 (pc *. p))
        done;
        !v
    | Decision { infoset = i; children } ->
        refresh t i;
        let player = t.player.(i) and base = t.offset.(i) in
        let row = depth * t.width and n = Array.length children in
        let v = ref 0. in
        for a = 0 to n - 1 do
          let s = t.current.(base + a) in
          let va =
            match player with
            | P0 -> walk t updating children.(a) (depth + 1) (pi0 *. s) pi1 pc
            | P1 -> walk t updating children.(a) (depth + 1) pi0 (pi1 *. s) pc
          in
          t.values.(row + a) <- va;
          v := !v +. (s *. va)
        done;
        if updating player then begin
          let own, opp =
            match player with P0 -> (pi0, pi1) | P1 -> (pi1, pi0)
          in
          let w = Game.sign player *. opp *. pc and own = t.weight *. own in
          for a = 0 to n - 1 do
            let k = base + a in
            t.regret.(k) <- t.regret.(k) +. (w *. (t.values.(row + a) -. !v));
            t.strategy_sum.(k) <- t.strategy_sum.(k) +. (own *. t.current.(k))
          done
        end;
        !v

  let discount t updating n =
    let pos = factor t.schedule.alpha n and neg = factor t.schedule.beta n in
    if pos <> 1. || neg <> 1. then
      Array.iteri
        (fun i p ->
          if updating p then
            for k = t.offset.(i) to t.offset.(i + 1) - 1 do
              let r = t.regret.(k) in
              t.regret.(k) <- (r *. if r > 0. then pos else neg)
            done)
        t.player

  let pass t updating n =
    ignore (walk t updating t.tree.root 0 1. 1. 1.);
    t.passes <- t.passes + 1;
    discount t updating n

  let iterate t =
    let n = t.iterations + 1 in
    t.weight <- Float.pow (float_of_int n) t.schedule.gamma;
    if t.schedule.alternating then begin
      pass t (( = ) Game.P0) n;
      pass t (( = ) Game.P1) n
    end
    else pass t (fun _ -> true) n;
    t.iterations <- n

  let run t n =
    for _ = 1 to n do
      iterate t
    done

  let to_strategy t f =
    let s = ref Strategy.empty in
    Array.iteri
      (fun i (info : Tree.infoset) ->
        let slice a = Array.sub a t.offset.(i) (Array.length info.actions) in
        s := Strategy.add info.key ~actions:info.actions ~probs:(f slice) !s)
      t.tree.infosets;
    !s

  let average_strategy t =
    to_strategy t (fun slice ->
        Regret_matching.normalize (slice t.strategy_sum))

  let current_strategy t =
    to_strategy t (fun slice -> Regret_matching.strategy (slice t.regret))
end
