open Treeplex

type t = {
  tp : Treeplex.t;
  mutable size : int;
  sum : float array;
  dev : float array array;
  follow : float array;
  value : float array;
}

let create tp =
  let tree = tp.tree in
  let slots = Multi_tree.slots tree in
  let dev = Array.make slots [||] in
  Array.iteri
    (fun k pos ->
      let w = pos.(Array.length pos - 1) in
      for s = tree.offset.(k) to tree.offset.(k + 1) - 1 do
        dev.(s) <- Array.make w 0.
      done)
    tp.local;
  {
    tp;
    size = 0;
    sum = Array.make tree.players 0.;
    dev;
    follow = Array.make slots 0.;
    value = Array.make (Array.length tree.keys) 0.;
  }

let size t = t.size

let record t x g u =
  let tp = t.tp in
  let off = tp.tree.offset in
  t.size <- t.size + 1;
  Array.iteri (fun i v -> t.sum.(i) <- t.sum.(i) +. v) u;
  let part = sums tp g x in
  Array.iteri
    (fun k sub ->
      let gk = g.(tp.tree.owner.(k)) and pos = tp.local.(k) in
      for s = off.(k) to off.(k + 1) - 1 do
        t.follow.(s) <- t.follow.(s) +. part.(s);
        let w = x.(s) and d = t.dev.(s) in
        if w > 0. then
          Array.iteri
            (fun m j ->
              let base = pos.(m) - off.(j) in
              for s' = off.(j) to off.(j + 1) - 1 do
                d.(base + s') <- d.(base + s') +. (w *. gk.(s'))
              done)
            sub
      done)
    tp.subtree

let add t flat =
  let g, u = gradients t.tp flat in
  record t (plans t.tp flat) g u

let deviation t s =
  let tp = t.tp in
  let off = tp.tree.offset in
  let k = infoset tp s in
  let sub = tp.subtree.(k) and pos = tp.local.(k) and d = t.dev.(s) in
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

let gain t s = (deviation t s -. t.follow.(s)) /. float_of_int t.size

let gains t =
  let tree = t.tp.tree in
  let g = Array.make tree.players 0. in
  for s = 0 to Multi_tree.slots tree - 1 do
    let p = tree.owner.(infoset t.tp s) in
    g.(p) <- Float.max g.(p) (gain t s)
  done;
  g

let gap t = Array.fold_left Float.max 0. (gains t)
let values t = Array.map (fun v -> v /. float_of_int t.size) t.sum
