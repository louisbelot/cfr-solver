type algorithm =
  | Vanilla
  | Plus
  | Linear
  | Discounted of { alpha : float; beta : float; gamma : float }
  | Predictive of { alpha : float; gamma : float }

let dcfr = Discounted { alpha = 1.5; beta = 0.; gamma = 2. }
let dcfr_plus = Discounted { alpha = 1.5; beta = neg_infinity; gamma = 4. }
let pcfr_plus = Predictive { alpha = infinity; gamma = 2. }
let pdcfr_plus = Predictive { alpha = 2.3; gamma = 5. }

let algorithm_name = function
  | Vanilla -> "cfr"
  | Plus -> "cfr+"
  | Linear -> "lcfr"
  | Discounted { alpha; beta; gamma } when beta = neg_infinity ->
      Printf.sprintf "dcfr+(%g,%g)" alpha gamma
  | Discounted { alpha; beta; gamma } ->
      Printf.sprintf "dcfr(%g,%g,%g)" alpha beta gamma
  | Predictive { alpha; gamma = 2. } when alpha = infinity -> "pcfr+"
  | Predictive { alpha; gamma } when alpha = infinity ->
      Printf.sprintf "pcfr+(%g)" gamma
  | Predictive { alpha; gamma } -> Printf.sprintf "pdcfr+(%g,%g)" alpha gamma

type schedule = {
  alternating : bool;
  predictive : bool;
  alpha : float;
  beta : float;
  gamma : float;
}

let schedule = function
  | Vanilla ->
      {
        alternating = false;
        predictive = false;
        alpha = infinity;
        beta = infinity;
        gamma = 0.;
      }
  | Plus ->
      {
        alternating = true;
        predictive = false;
        alpha = infinity;
        beta = neg_infinity;
        gamma = 1.;
      }
  | Linear ->
      {
        alternating = true;
        predictive = false;
        alpha = 1.;
        beta = 1.;
        gamma = 1.;
      }
  | Discounted { alpha; beta; gamma } ->
      { alternating = true; predictive = false; alpha; beta; gamma }
  | Predictive { alpha; gamma } ->
      {
        alternating = true;
        predictive = true;
        alpha;
        beta = neg_infinity;
        gamma;
      }

let factor e t =
  if e = infinity then 1.
  else if e = neg_infinity then 0.
  else
    let x = Float.pow (float_of_int t) e in
    x /. (x +. 1.)

let discounts algorithm t =
  let s = schedule algorithm in
  ( factor s.alpha t,
    factor s.beta t,
    Float.pow (float_of_int t /. float_of_int (t + 1)) s.gamma )

let alternating algorithm = (schedule algorithm).alternating
let predictive algorithm = (schedule algorithm).predictive
let weight algorithm t = Float.pow (float_of_int t) (schedule algorithm).gamma

module Make (G : Game.S) = struct
  type t = {
    root : Tree.node;
    table : Table.t;
    size : int;
    values : float array;
    reach : float array;
    ret : float array;
    last : float array;
    next : float array;
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
    let table = Table.create tree and s = schedule algorithm in
    let slots = if s.predictive then Table.slots table else 0 in
    {
      root = tree.root;
      table;
      size = Tree.size tree.root;
      values = Array.make (height * width) 0.;
      reach = Array.make (3 * (height + 1)) 1.;
      ret = [| 0. |];
      last = Array.make slots 0.;
      next = Array.make slots 0.;
      width;
      schedule = s;
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
        if t.schedule.predictive then
          Table.refresh_predicted tab t.last i t.passes
        else Table.refresh tab i t.passes;
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
            let k = base + a and r = w *. (t.values.(row + a) -. !v) in
            tab.regret.(k) <- tab.regret.(k) +. r;
            if t.schedule.predictive then t.next.(k) <- t.next.(k) +. r;
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

  let predict t updating =
    let tab = t.table in
    Array.iteri
      (fun i p ->
        if updating p then
          for k = tab.offset.(i) to tab.offset.(i + 1) - 1 do
            t.last.(k) <- t.next.(k);
            t.next.(k) <- 0.
          done)
      tab.player

  let pass t updating n =
    walk t updating t.root 0;
    t.passes <- t.passes + 1;
    t.touched <- t.touched + t.size;
    discount t updating n;
    if t.schedule.predictive then predict t updating

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

  let current_strategy t =
    if t.schedule.predictive then Table.predicted_strategy t.table t.last
    else Table.current_strategy t.table
end
