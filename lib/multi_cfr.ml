module Make (G : Multi.S) = struct
  type t = {
    tree : Multi_tree.t;
    regret : float array;
    current : float array;
    sum : float array;
    reach : float array;
    vals : float array;
    acts : float array;
    algorithm : Cfr.algorithm;
    mutable weight : float;
    mutable iterations : int;
    mutable touched : int;
  }

  let create ?(algorithm = Cfr.Vanilla) () =
    let tree = Multi_tree.build (module G) in
    let slots = Multi_tree.slots tree in
    {
      tree;
      regret = Array.make slots 0.;
      current = Array.make slots 0.;
      sum = Array.make slots 0.;
      reach = Array.make (G.players + 1) 1.;
      vals = Array.make ((tree.height + 2) * G.players) 0.;
      acts = Array.make ((tree.height + 1) * tree.width) 0.;
      algorithm;
      weight = 1.;
      iterations = 0;
      touched = 0;
    }

  let iterations t = t.iterations
  let touched t = t.touched
  let infosets t = Array.length t.tree.keys

  let counterfactual t p =
    let w = ref 1. in
    for j = 0 to G.players do
      if j <> p then w := !w *. t.reach.(j)
    done;
    !w

  let rec walk t updating node d =
    let n = G.players in
    let o = d * n in
    match node with
    | Multi_tree.Leaf u -> Array.blit u 0 t.vals o n
    | Chance (probs, children) ->
        let pc = t.reach.(n) in
        Array.fill t.vals o n 0.;
        for k = 0 to Array.length children - 1 do
          let p = probs.(k) in
          t.reach.(n) <- pc *. p;
          walk t updating children.(k) (d + 1);
          for j = 0 to n - 1 do
            t.vals.(o + j) <- t.vals.(o + j) +. (p *. t.vals.(o + n + j))
          done
        done;
        t.reach.(n) <- pc
    | Decision (i, children) ->
        let p = t.tree.owner.(i) and base = t.tree.offset.(i) in
        let row = d * t.tree.width in
        let own = t.reach.(p) in
        Array.fill t.vals o n 0.;
        for a = 0 to Array.length children - 1 do
          let s = t.current.(base + a) in
          t.reach.(p) <- own *. s;
          walk t updating children.(a) (d + 1);
          t.acts.(row + a) <- t.vals.(o + n + p);
          for j = 0 to n - 1 do
            t.vals.(o + j) <- t.vals.(o + j) +. (s *. t.vals.(o + n + j))
          done
        done;
        t.reach.(p) <- own;
        if updating p then begin
          let cf = counterfactual t p and v = t.vals.(o + p) in
          for a = 0 to Array.length children - 1 do
            let k = base + a in
            t.regret.(k) <- t.regret.(k) +. (cf *. (t.acts.(row + a) -. v));
            t.sum.(k) <- t.sum.(k) +. (t.weight *. own *. t.current.(k))
          done
        end

  let refresh t =
    for i = 0 to Array.length t.tree.keys - 1 do
      let lo = t.tree.offset.(i) and hi = t.tree.offset.(i + 1) in
      let total = ref 0. in
      for k = lo to hi - 1 do
        total := !total +. Float.max t.regret.(k) 0.
      done;
      for k = lo to hi - 1 do
        t.current.(k) <-
          (if !total > 0. then Float.max t.regret.(k) 0. /. !total
           else 1. /. float_of_int (hi - lo))
      done
    done

  let pass t updating n =
    refresh t;
    walk t updating t.tree.root 0;
    t.touched <- t.touched + t.tree.size;
    let pos, neg, _ = Cfr.discounts t.algorithm n in
    if pos <> 1. || neg <> 1. then
      Array.iteri
        (fun i p ->
          if updating p then
            for k = t.tree.offset.(i) to t.tree.offset.(i + 1) - 1 do
              let r = t.regret.(k) in
              t.regret.(k) <- (r *. if r > 0. then pos else neg)
            done)
        t.tree.owner

  let iterate t =
    let n = t.iterations + 1 in
    t.weight <- Cfr.weight t.algorithm n;
    if Cfr.alternating t.algorithm then
      for p = 0 to G.players - 1 do
        pass t (( = ) p) n
      done
    else pass t (fun _ -> true) n;
    t.iterations <- n

  let run t n =
    for _ = 1 to n do
      iterate t
    done

  let export t f a =
    let s = ref Strategy.empty in
    Array.iteri
      (fun i key ->
        let lo = t.tree.offset.(i) in
        let slice = Array.sub a lo (t.tree.offset.(i + 1) - lo) in
        s := Strategy.add key ~actions:t.tree.labels.(i) ~probs:(f slice) !s)
      t.tree.keys;
    !s

  let average_strategy t = export t Regret_matching.normalize t.sum
  let current_strategy t = export t Regret_matching.strategy t.regret
end
