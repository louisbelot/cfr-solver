open Treeplex

type t = {
  tp : Treeplex.t;
  plus : bool;
  regret : float array;
  flat : float array;
  reach : float array;
  value : float array;
}

let create ?(plus = false) tp =
  let slots = Multi_tree.slots tp.tree in
  {
    tp;
    plus;
    regret = Array.make slots 0.;
    flat = Array.make slots 0.;
    reach = Array.make slots 0.;
    value = Array.make (Array.length tp.tree.keys) 0.;
  }

let normalize src dst lo hi =
  let total = ref 0. in
  for s = lo to hi - 1 do
    total := !total +. Float.max 0. src.(s)
  done;
  for s = lo to hi - 1 do
    dst.(s) <-
      (if !total > 0. then Float.max 0. src.(s) /. !total
       else 1. /. float_of_int (hi - lo))
  done

let behavior t src =
  let off = t.tp.tree.offset in
  let f = Array.make (Array.length t.flat) 0. in
  for k = 0 to Array.length t.value - 1 do
    normalize src f off.(k) off.(k + 1)
  done;
  f

let current t = behavior t t.regret
let average t = behavior t t.reach

let play t =
  let off = t.tp.tree.offset in
  for k = 0 to Array.length t.value - 1 do
    normalize t.regret t.flat off.(k) off.(k + 1)
  done;
  let x = plans t.tp t.flat in
  Array.iteri (fun s w -> t.reach.(s) <- t.reach.(s) +. w) x;
  (t.flat, x)

let update t g =
  let tp = t.tp in
  let off = tp.tree.offset in
  for m = Array.length tp.order - 1 downto 0 do
    let k = tp.order.(m) in
    let gk = g.(tp.tree.owner.(k)) in
    let lo = off.(k) and hi = off.(k + 1) in
    let q =
      Array.init (hi - lo) (fun b ->
          let s = lo + b in
          Array.fold_left (fun acc j -> acc +. t.value.(j)) gk.(s) tp.below.(s))
    in
    let v = ref 0. in
    Array.iteri (fun b qb -> v := !v +. (t.flat.(lo + b) *. qb)) q;
    t.value.(k) <- !v;
    Array.iteri
      (fun b qb ->
        let r = t.regret.(lo + b) +. qb -. !v in
        t.regret.(lo + b) <- (if t.plus then Float.max 0. r else r))
      q
  done
