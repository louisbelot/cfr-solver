type sampling = External | Outcome of { epsilon : float }

let outcome = Outcome { epsilon = 0.6 }

let sampling_name = function
  | External -> "es"
  | Outcome { epsilon } -> Printf.sprintf "os(%g)" epsilon

let[@inline] uniform rng = float_of_int (Rng.bits rng) *. 0x1p-30

let pick rng probs base n =
  let x = ref (uniform rng) and a = ref 0 in
  while !a < n - 1 && !x >= probs.(base + !a) do
    x := !x -. probs.(base + !a);
    incr a
  done;
  !a

module Make (G : Game.S) = struct
  type t = {
    root : Tree.node;
    table : Table.t;
    sampling : sampling;
    weighting : Cfr.algorithm;
    interval : int;
    rng : Rng.t;
    values : float array;
    weights : float array;
    ret : float array;
    width : int;
    mutable iterations : int;
    mutable passes : int;
    mutable touched : int;
  }

  let create ?(seed = 1) ?(weighting = Cfr.Vanilla) ?(interval = 1) sampling =
    (match sampling with
    | Outcome { epsilon } when not (epsilon > 0. && epsilon <= 1.) ->
        invalid_arg "Mccfr.create: epsilon must be in (0, 1]"
    | _ -> ());
    if interval < 1 then invalid_arg "Mccfr.create: interval must be positive";
    let tree = Tree.build (module G) in
    let height, width = Tree.shape tree.root in
    {
      root = tree.root;
      table = Table.create tree;
      sampling;
      weighting;
      interval;
      rng = Rng.make seed;
      values = Array.make (height * width) 0.;
      weights = Array.make (2 * (height + 1)) 1.;
      ret = [| 0. |];
      width;
      iterations = 0;
      passes = 0;
      touched = 0;
    }

  let clone shared rng t =
    {
      t with
      table = (if shared then Table.share t.table else Table.copy t.table);
      rng;
      values = Array.copy t.values;
      weights = Array.copy t.weights;
      ret = [| 0. |];
    }

  let split ?(shared = false) t = clone shared (Rng.split t.rng) t
  let copy ?(shared = false) t = clone shared (Rng.copy t.rng) t
  let iterations t = t.iterations
  let touched t = t.touched
  let table t = t.table

  let rec external_walk t i node d =
    t.touched <- t.touched + 1;
    match node with
    | Tree.Terminal u -> t.ret.(0) <- u
    | Chance { probs; children } ->
        external_walk t i
          children.(pick t.rng probs 0 (Array.length probs))
          (d + 1)
    | Decision { infoset; children } ->
        let tab = t.table in
        Table.refresh tab infoset t.passes;
        let base = tab.offset.(infoset) and n = Array.length children in
        if tab.player.(infoset) <> i then begin
          for k = base to base + n - 1 do
            tab.strategy_sum.(k) <- tab.strategy_sum.(k) +. tab.current.(k)
          done;
          external_walk t i children.(pick t.rng tab.current base n) (d + 1)
        end
        else begin
          let row = d * t.width and v = ref 0. in
          for a = 0 to n - 1 do
            external_walk t i children.(a) (d + 1);
            t.values.(row + a) <- t.ret.(0);
            v := !v +. (tab.current.(base + a) *. t.ret.(0))
          done;
          let sign = Game.sign i in
          for a = 0 to n - 1 do
            let k = base + a in
            tab.regret.(k) <-
              tab.regret.(k) +. (sign *. (t.values.(row + a) -. !v))
          done;
          t.ret.(0) <- !v
        end

  let rec outcome_walk t i epsilon node d =
    t.touched <- t.touched + 1;
    let w = 2 * d in
    let opp = t.weights.(w) and s = t.weights.(w + 1) in
    match node with
    | Tree.Terminal u -> t.ret.(0) <- u /. s
    | Chance { probs; children } ->
        t.weights.(w + 2) <- opp;
        t.weights.(w + 3) <- s;
        outcome_walk t i epsilon
          children.(pick t.rng probs 0 (Array.length probs))
          (d + 1)
    | Decision { infoset; children } ->
        let tab = t.table in
        Table.refresh tab infoset t.passes;
        let base = tab.offset.(infoset) and n = Array.length children in
        if tab.player.(infoset) <> i then begin
          for k = base to base + n - 1 do
            tab.strategy_sum.(k) <-
              tab.strategy_sum.(k) +. (opp *. tab.current.(k) /. s)
          done;
          let a = pick t.rng tab.current base n in
          let p = tab.current.(base + a) in
          t.weights.(w + 2) <- opp *. p;
          t.weights.(w + 3) <- s *. p;
          outcome_walk t i epsilon children.(a) (d + 1);
          t.ret.(0) <- t.ret.(0) *. p
        end
        else begin
          let a =
            if uniform t.rng < epsilon then Rng.int t.rng n
            else pick t.rng tab.current base n
          in
          let p = tab.current.(base + a) in
          let q = (epsilon /. float_of_int n) +. ((1. -. epsilon) *. p) in
          t.weights.(w + 2) <- opp;
          t.weights.(w + 3) <- s *. q;
          outcome_walk t i epsilon children.(a) (d + 1);
          let x = Game.sign i *. opp *. t.ret.(0) in
          for b = 0 to n - 1 do
            let k = base + b in
            tab.regret.(k) <-
              (tab.regret.(k) +. if b = a then x *. (1. -. p) else -.x *. p)
          done;
          t.ret.(0) <- t.ret.(0) *. p
        end

  let traverse t i =
    (match t.sampling with
    | External -> external_walk t i t.root 0
    | Outcome { epsilon } -> outcome_walk t i epsilon t.root 0);
    t.passes <- t.passes + 1

  let iterate t =
    traverse t P0;
    traverse t P1;
    t.iterations <- t.iterations + 1;
    match t.weighting with
    | Vanilla -> ()
    | w ->
        if t.iterations mod t.interval = 0 then
          Table.discount t.table (Cfr.discounts w (t.iterations / t.interval))

  let run t n =
    for _ = 1 to n do
      iterate t
    done

  let average_strategy t = Table.average_strategy t.table
  let current_strategy t = Table.current_strategy t.table
  let regrets t = Table.regrets t.table
  let strategy_sums t = Table.strategy_sums t.table
end
