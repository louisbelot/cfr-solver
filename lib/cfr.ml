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
    root : Tree.node;
    table : Table.t;
    size : int;
    values : float array;
    reach : float array;
    ret : float array;
    width : int;
    schedule : schedule;
    mutable iterations : int;
    mutable passes : int;
    mutable touched : int;
    mutable weight : float;
  }

  let create ?(algorithm = Vanilla) () =
    let tree = Tree.build (module G) in
    let height, width = Tree.shape tree.root in
    {
      root = tree.root;
      table = Table.create tree;
      size = Tree.size tree.root;
      values = Array.make (height * width) 0.;
      reach = Array.make (3 * (height + 1)) 1.;
      ret = [| 0. |];
      width;
      schedule = schedule algorithm;
      iterations = 0;
      passes = 0;
      touched = 0;
      weight = 1.;
    }

  let iterations t = t.iterations
  let touched t = t.touched

  let rec walk t updating node d =
    let r = 3 * d in
    match node with
    | Tree.Terminal u -> t.ret.(0) <- u
    | Chance { probs; children } ->
        let pc = t.reach.(r + 2) and v = ref 0. in
        t.reach.(r + 3) <- t.reach.(r);
        t.reach.(r + 4) <- t.reach.(r + 1);
        for k = 0 to Array.length children - 1 do
          let p = probs.(k) in
          t.reach.(r + 5) <- pc *. p;
          walk t updating children.(k) (d + 1);
          v := !v +. (p *. t.ret.(0))
        done;
        t.ret.(0) <- !v
    | Decision { infoset = i; children } ->
        let tab = t.table in
        Table.refresh tab i t.passes;
        let player = tab.player.(i) and base = tab.offset.(i) in
        let row = d * t.width and n = Array.length children in
        let pi0 = t.reach.(r) and pi1 = t.reach.(r + 1) in
        let pc = t.reach.(r + 2) in
        let own, opp, slot =
          match player with P0 -> (pi0, pi1, r + 3) | P1 -> (pi1, pi0, r + 4)
        in
        t.reach.(r + 3) <- pi0;
        t.reach.(r + 4) <- pi1;
        t.reach.(r + 5) <- pc;
        let v = ref 0. in
        for a = 0 to n - 1 do
          let s = tab.current.(base + a) in
          t.reach.(slot) <- own *. s;
          walk t updating children.(a) (d + 1);
          t.values.(row + a) <- t.ret.(0);
          v := !v +. (s *. t.ret.(0))
        done;
        if updating player then begin
          let w = Game.sign player *. opp *. pc and own = t.weight *. own in
          for a = 0 to n - 1 do
            let k = base + a in
            tab.regret.(k) <- tab.regret.(k) +. (w *. (t.values.(row + a) -. !v));
            tab.strategy_sum.(k) <-
              tab.strategy_sum.(k) +. (own *. tab.current.(k))
          done
        end;
        t.ret.(0) <- !v

  let discount t updating n =
    let pos = factor t.schedule.alpha n and neg = factor t.schedule.beta n in
    let tab = t.table in
    if pos <> 1. || neg <> 1. then
      Array.iteri
        (fun i p ->
          if updating p then
            for k = tab.offset.(i) to tab.offset.(i + 1) - 1 do
              let r = tab.regret.(k) in
              tab.regret.(k) <- (r *. if r > 0. then pos else neg)
            done)
        tab.player

  let pass t updating n =
    walk t updating t.root 0;
    t.passes <- t.passes + 1;
    t.touched <- t.touched + t.size;
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

  let average_strategy t = Table.average_strategy t.table
  let current_strategy t = Table.current_strategy t.table
end
