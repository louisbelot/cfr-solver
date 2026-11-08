type averaging = Simple | Weighted | Full

let averaging_name = function
  | Simple -> "simple"
  | Weighted -> "weighted"
  | Full -> "full"

module Make (G : Multi.S) = struct
  type t = {
    tree : Multi_tree.t;
    regret : float array;
    current : float array;
    sum : float array;
    stamp : int array;
    reach : float array;
    vals : float array;
    ret : float array;
    rng : Rng.t;
    weighting : Cfr.algorithm;
    interval : int;
    averaging : averaging;
    mutable frozen : bool;
    mutable passes : int;
    mutable iterations : int;
    mutable touched : int;
  }

  let create ?(seed = 1) ?(weighting = Cfr.Vanilla) ?(interval = 1)
      ?(averaging = Simple) () =
    if Cfr.predictive weighting then
      invalid_arg "Multi_mccfr.create: predictive weighting";
    if interval < 1 then
      invalid_arg "Multi_mccfr.create: interval must be positive";
    let tree = Multi_tree.build (module G) in
    let slots = Multi_tree.slots tree in
    {
      tree;
      regret = Array.make slots 0.;
      current = Array.make slots 0.;
      sum = Array.make slots 0.;
      stamp = Array.make (Array.length tree.keys) (-1);
      reach = Array.make G.players 1.;
      vals = Array.make ((tree.height + 1) * tree.width) 0.;
      ret = [| 0. |];
      rng = Rng.make seed;
      weighting;
      interval;
      averaging;
      frozen = false;
      passes = 0;
      iterations = 0;
      touched = 0;
    }

  let iterations t = t.iterations
  let touched t = t.touched
  let infosets t = Array.length t.tree.keys
  let tree t = t.tree

  let refresh t i =
    if (not t.frozen) && t.stamp.(i) <> t.passes then begin
      let base = t.tree.offset.(i) in
      let n = t.tree.offset.(i + 1) - base in
      let total = ref 0. in
      for k = base to base + n - 1 do
        total := !total +. Float.max t.regret.(k) 0.
      done;
      for k = base to base + n - 1 do
        t.current.(k) <-
          (if !total > 0. then Float.max t.regret.(k) 0. /. !total
           else 1. /. float_of_int n)
      done;
      t.stamp.(i) <- t.passes
    end

  let others t i p =
    let w = ref 1. in
    for j = 0 to G.players - 1 do
      if j <> i && j <> p then w := !w *. t.reach.(j)
    done;
    !w

  let rec walk t i learn node d =
    t.touched <- t.touched + 1;
    match node with
    | Multi_tree.Leaf u -> t.ret.(0) <- u.(i)
    | Chance (probs, children) ->
        walk t i learn
          children.(Rng.pick t.rng probs 0 (Array.length probs))
          (d + 1)
    | Decision (k, children) ->
        refresh t k;
        let p = t.tree.owner.(k) and base = t.tree.offset.(k) in
        let n = Array.length children in
        if p <> i then begin
          (match t.averaging with
          | Full -> ()
          | Simple ->
              for a = base to base + n - 1 do
                t.sum.(a) <- t.sum.(a) +. t.current.(a)
              done
          | Weighted ->
              let w = 1. /. others t i p in
              for a = base to base + n - 1 do
                t.sum.(a) <- t.sum.(a) +. (w *. t.current.(a))
              done);
          let a = Rng.pick t.rng t.current base n in
          let own = t.reach.(p) in
          t.reach.(p) <- own *. t.current.(base + a);
          walk t i learn children.(a) (d + 1);
          t.reach.(p) <- own
        end
        else begin
          let row = d * t.tree.width and v = ref 0. in
          for a = 0 to n - 1 do
            walk t i learn children.(a) (d + 1);
            t.vals.(row + a) <- t.ret.(0);
            v := !v +. (t.current.(base + a) *. t.ret.(0))
          done;
          if learn then
            for a = 0 to n - 1 do
              t.regret.(base + a) <-
                t.regret.(base + a) +. (t.vals.(row + a) -. !v)
            done;
          t.ret.(0) <- !v
        end

  let rec average t node =
    t.touched <- t.touched + 1;
    match node with
    | Multi_tree.Leaf _ -> ()
    | Chance (_, children) -> Array.iter (average t) children
    | Decision (k, children) ->
        refresh t k;
        let p = t.tree.owner.(k) and base = t.tree.offset.(k) in
        let own = t.reach.(p) in
        Array.iteri
          (fun a child ->
            let s = t.current.(base + a) in
            t.sum.(base + a) <- t.sum.(base + a) +. (own *. s);
            t.reach.(p) <- own *. s;
            average t child)
          children;
        t.reach.(p) <- own

  let pass t i learn =
    walk t i learn t.tree.root 0;
    t.passes <- t.passes + 1

  let full t =
    average t t.tree.root;
    t.passes <- t.passes + 1

  let traverse t i = pass t i true

  let discount t (pos, neg, avg) =
    if pos <> 1. || neg <> 1. then
      Array.iteri
        (fun k r -> t.regret.(k) <- (r *. if r > 0. then pos else neg))
        t.regret;
    if avg <> 1. then Array.iteri (fun k s -> t.sum.(k) <- s *. avg) t.sum

  let iterate t =
    for i = 0 to G.players - 1 do
      traverse t i
    done;
    if t.averaging = Full then full t;
    t.iterations <- t.iterations + 1;
    match t.weighting with
    | Vanilla -> ()
    | w ->
        if t.iterations mod t.interval = 0 then
          discount t (Cfr.discounts w (t.iterations / t.interval))

  let run t n =
    for _ = 1 to n do
      iterate t
    done

  let accumulate t i sigma =
    Array.iteri
      (fun k key ->
        let base = t.tree.offset.(k) and n = Array.length t.tree.labels.(k) in
        Array.blit (Strategy.probs sigma key ~n) 0 t.current base n)
      t.tree.keys;
    t.frozen <- true;
    if t.averaging = Full then full t else pass t i false;
    t.frozen <- false;
    Array.fill t.stamp 0 (Array.length t.stamp) (-1)

  let export t f a =
    let s = ref Strategy.empty in
    Array.iteri
      (fun k key ->
        let lo = t.tree.offset.(k) in
        let slice = Array.sub a lo (t.tree.offset.(k + 1) - lo) in
        s := Strategy.add key ~actions:t.tree.labels.(k) ~probs:(f slice) !s)
      t.tree.keys;
    !s

  let average_strategy t = export t Regret_matching.normalize t.sum
  let current_strategy t = export t Regret_matching.strategy t.regret
end
