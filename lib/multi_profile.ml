open Multi_tree

let reach tree sigma =
  let n = Array.length tree.keys in
  let p = Multi_br.probs tree sigma in
  let total = Array.make n 0. and own = Array.make n 0. in
  let mine = Array.make tree.players 1. in
  let rec go node w =
    match node with
    | Leaf _ -> ()
    | Chance (q, children) ->
        Array.iteri (fun j c -> go c (w *. q.(j))) children
    | Decision (k, children) ->
        let i = tree.owner.(k) and base = tree.offset.(k) in
        let r = mine.(i) in
        total.(k) <- total.(k) +. w;
        own.(k) <- r;
        Array.iteri
          (fun a c ->
            let s = p.(base + a) in
            mine.(i) <- r *. s;
            go c (w *. s))
          children;
        mine.(i) <- r
  in
  go tree.root 1.;
  (total, own)

let distance tree a b =
  let pa = Multi_br.probs tree a and pb = Multi_br.probs tree b in
  let ra, _ = reach tree a and rb, _ = reach tree b in
  let num = Array.make tree.players 0. and den = Array.make tree.players 0. in
  Array.iteri
    (fun k _ ->
      let i = tree.owner.(k) and w = 0.5 *. (ra.(k) +. rb.(k)) in
      let tv = ref 0. in
      for s = tree.offset.(k) to tree.offset.(k + 1) - 1 do
        tv := !tv +. Float.abs (pa.(s) -. pb.(s))
      done;
      num.(i) <- num.(i) +. (w *. 0.5 *. !tv);
      den.(i) <- den.(i) +. w)
    tree.keys;
  Array.mapi (fun i x -> if den.(i) > 0. then x /. den.(i) else 0.) num

let mix tree profiles =
  let sums = Array.make (slots tree) 0. in
  List.iter
    (fun sigma ->
      let p = Multi_br.probs tree sigma and _, own = reach tree sigma in
      Array.iteri
        (fun k _ ->
          for s = tree.offset.(k) to tree.offset.(k + 1) - 1 do
            sums.(s) <- sums.(s) +. (own.(k) *. p.(s))
          done)
        tree.keys)
    profiles;
  let out = ref Strategy.empty in
  Array.iteri
    (fun k key ->
      let lo = tree.offset.(k) in
      let probs =
        Regret_matching.normalize (Array.sub sums lo (tree.offset.(k + 1) - lo))
      in
      out := Strategy.add key ~actions:tree.labels.(k) ~probs !out)
    tree.keys;
  !out
