open Treeplex

type concept = Efce | Efcce | Nfcce

type t = {
  tp : Treeplex.t;
  mutable size : int;
  sum : float array;
  dev : float array array;
  follow : float array;
  coarse : float array array;
  reached : float array;
  value : float array;
}

let create tp =
  let tree = tp.tree in
  let slots = Multi_tree.slots tree in
  let dev = Array.make slots [||] in
  let width pos = pos.(Array.length pos - 1) in
  Array.iteri
    (fun k pos ->
      for s = tree.offset.(k) to tree.offset.(k + 1) - 1 do
        dev.(s) <- Array.make (width pos) 0.
      done)
    tp.local;
  let n = Array.length tree.keys in
  {
    tp;
    size = 0;
    sum = Array.make tree.players 0.;
    dev;
    follow = Array.make slots 0.;
    coarse = Array.map (fun pos -> Array.make (width pos) 0.) tp.local;
    reached = Array.make n 0.;
    value = Array.make n 0.;
  }

let size t = t.size

let spread off sub pos d w gk =
  if w > 0. then
    Array.iteri
      (fun m j ->
        let base = pos.(m) - off.(j) in
        for s' = off.(j) to off.(j + 1) - 1 do
          d.(base + s') <- d.(base + s') +. (w *. gk.(s'))
        done)
      sub

let record t x g u =
  let tp = t.tp in
  let off = tp.tree.offset in
  t.size <- t.size + 1;
  Array.iteri (fun i v -> t.sum.(i) <- t.sum.(i) +. v) u;
  let part = sums tp g x in
  Array.iteri
    (fun k sub ->
      let gk = g.(tp.tree.owner.(k)) and pos = tp.local.(k) in
      let p = tp.parent.(k) in
      spread off sub pos t.coarse.(k) (if p < 0 then 1. else x.(p)) gk;
      for s = off.(k) to off.(k + 1) - 1 do
        t.follow.(s) <- t.follow.(s) +. part.(s);
        t.reached.(k) <- t.reached.(k) +. part.(s);
        spread off sub pos t.dev.(s) x.(s) gk
      done)
    tp.subtree

let add t flat =
  let g, u = gradients t.tp flat in
  record t (plans t.tp flat) g u

let deviation t k d =
  let tp = t.tp in
  let off = tp.tree.offset in
  let sub = tp.subtree.(k) and pos = tp.local.(k) in
  for m = Array.length sub - 1 downto 0 do
    let j = sub.(m) in
    let best = ref Float.neg_infinity in
    for s' = off.(j) to off.(j + 1) - 1 do
      let q = ref d.(pos.(m) + s' - off.(j)) in
      Array.iter (fun j' -> q := !q +. t.value.(j')) tp.below.(s');
      if !q > !best then best := !q
    done;
    t.value.(j) <- !best
  done;
  t.value.(k)

let gain t s =
  (deviation t (infoset t.tp s) t.dev.(s) -. t.follow.(s))
  /. float_of_int t.size

let coarse_gain t k =
  (deviation t k t.coarse.(k) -. t.reached.(k)) /. float_of_int t.size

let gains ?(concept = Efce) t =
  let tree = t.tp.tree in
  let g = Array.make tree.players 0. in
  let owner k = tree.owner.(k) in
  (match concept with
  | Efce ->
      for s = 0 to Multi_tree.slots tree - 1 do
        let p = owner (infoset t.tp s) in
        g.(p) <- Float.max g.(p) (gain t s)
      done
  | Efcce ->
      Array.iteri
        (fun k _ -> g.(owner k) <- Float.max g.(owner k) (coarse_gain t k))
        tree.keys
  | Nfcce ->
      let sum = Array.make tree.players 0. in
      Array.iteri
        (fun k p ->
          if p < 0 then sum.(owner k) <- sum.(owner k) +. coarse_gain t k)
        t.tp.parent;
      Array.iteri (fun i v -> g.(i) <- Float.max 0. v) sum);
  g

let gap ?concept t = Array.fold_left Float.max 0. (gains ?concept t)
let values t = Array.map (fun v -> v /. float_of_int t.size) t.sum
