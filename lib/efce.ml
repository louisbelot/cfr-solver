open Treeplex

type t = {
  tp : Treeplex.t;
  plus : bool;
  rng : Rng.t option;
  mixture : Correlated.t;
  trigger : float array;
  regret : float array array;
  local : float array array;
  lambda : float array;
  flat : float array;
  seat : int array;
  mutable iterations : int;
}

let create ?(plus = false) ?rng tree =
  let tp = Treeplex.build tree in
  let slots = Multi_tree.slots tree in
  let span s =
    let pos = tp.local.(infoset tp s) in
    pos.(Array.length pos - 1)
  in
  {
    tp;
    plus;
    rng;
    mixture = Correlated.create tp;
    trigger = Array.make slots 0.;
    regret = Array.init slots (fun s -> Array.make (span s) 0.);
    local = Array.init slots (fun s -> Array.make (span s) 0.);
    lambda = Array.make slots 0.;
    flat = Array.make slots 0.;
    seat = Array.init slots (fun s -> tree.owner.(infoset tp s));
    iterations = 0;
  }

let iterations t = t.iterations
let current t = Array.copy t.flat
let mixture t = t.mixture

let matching r dst lo hi =
  let pos = ref 0. in
  for i = lo to hi - 1 do
    pos := !pos +. Float.max 0. r.(i)
  done;
  for i = lo to hi - 1 do
    dst.(i) <-
      (if !pos > 0. then Float.max 0. r.(i) /. !pos
       else 1. /. float_of_int (hi - lo))
  done

let strategies t =
  let tp = t.tp in
  let off = tp.tree.offset in
  Array.iteri
    (fun s r ->
      let k = infoset tp s in
      let sub = tp.subtree.(k) and pos = tp.local.(k) in
      Array.iteri
        (fun m j ->
          matching r t.local.(s) pos.(m) (pos.(m) + off.(j + 1) - off.(j)))
        sub)
    t.regret;
  let n = tp.tree.players in
  let total = Array.make n 0. and count = Array.make n 0 in
  Array.iteri
    (fun s p ->
      total.(p) <- total.(p) +. Float.max 0. t.trigger.(s);
      count.(p) <- count.(p) + 1)
    t.seat;
  Array.iteri
    (fun s p ->
      t.lambda.(s) <-
        (if total.(p) > 0. then Float.max 0. t.trigger.(s) /. total.(p)
         else 1. /. float_of_int count.(p)))
    t.seat

let fixed_point tp ~lambda ~local =
  let off = tp.tree.offset in
  let slots = Array.length lambda in
  let flat = Array.make slots 0. and x = Array.make slots 0. in
  let above = Array.make slots 0. and push = Array.make slots 0. in
  let yseq = Array.make slots 0. in
  Array.iter
    (fun k ->
      let p = tp.parent.(k) in
      let lo = off.(k) and n = off.(k + 1) - off.(k) in
      let xp = if p < 0 then 1. else x.(p) in
      let path = if p < 0 then 0. else above.(p) in
      let c =
        Array.init n (fun b -> if xp > 0. then push.(lo + b) /. xp else 0.)
      in
      let l = Array.fold_left ( +. ) 0. c in
      let m = ref l in
      for b = 0 to n - 1 do
        m := !m +. lambda.(lo + b)
      done;
      let r =
        if !m <= 0. then Array.make n (1. /. float_of_int n)
        else
          Markov.stationary
            (Array.init n (fun b' ->
                 let lb = lambda.(lo + b') in
                 Array.init n (fun b ->
                     (if b = b' then 1. -. ((l +. lb) /. !m) else 0.)
                     +. (((lb *. local.(lo + b').(b)) +. c.(b)) /. !m))))
      in
      for b = 0 to n - 1 do
        let s = lo + b in
        flat.(s) <- r.(b);
        x.(s) <- r.(b) *. xp;
        above.(s) <- path +. lambda.(s)
      done;
      let sub = tp.subtree.(k) and pos = tp.local.(k) in
      for b = 0 to n - 1 do
        let s = lo + b in
        let w = lambda.(s) *. x.(s) and y = local.(s) in
        if w > 0. then
          Array.iteri
            (fun i j ->
              let pj = tp.parent.(j) in
              let scale = if i = 0 then 1. else yseq.(pj) in
              for s' = off.(j) to off.(j + 1) - 1 do
                yseq.(s') <- scale *. y.(pos.(i) + s' - off.(j));
                if i > 0 then push.(s') <- push.(s') +. (w *. yseq.(s'))
              done)
            sub
      done)
    tp.order;
  (flat, x)

let update t g x =
  let tp = t.tp in
  let off = tp.tree.offset in
  let part = sums tp g x in
  let value = Array.make (Array.length tp.parent) 0. in
  let gain = Array.make (Array.length t.flat) 0. in
  Array.iteri
    (fun s r ->
      let k = infoset tp s in
      let gk = g.(tp.tree.owner.(k)) and w = x.(s) in
      let sub = tp.subtree.(k) and pos = tp.local.(k) and y = t.local.(s) in
      for m = Array.length sub - 1 downto 0 do
        let j = sub.(m) in
        let base = pos.(m) - off.(j) in
        let v = ref 0. in
        let q =
          Array.init
            (off.(j + 1) - off.(j))
            (fun b ->
              let s' = off.(j) + b in
              let q = ref (w *. gk.(s')) in
              Array.iter (fun j' -> q := !q +. value.(j')) tp.below.(s');
              v := !v +. (y.(base + s') *. !q);
              !q)
        in
        value.(j) <- !v;
        Array.iteri
          (fun b q ->
            let i = base + off.(j) + b in
            r.(i) <- r.(i) +. q -. !v;
            if t.plus && r.(i) < 0. then r.(i) <- 0.)
          q
      done;
      gain.(s) <- value.(k) -. part.(s))
    t.regret;
  let ev = Array.make tp.tree.players 0. in
  Array.iteri
    (fun s l ->
      let p = t.seat.(s) in
      ev.(p) <- ev.(p) +. (l *. gain.(s)))
    t.lambda;
  Array.iteri
    (fun s gs ->
      let p = t.seat.(s) in
      t.trigger.(s) <- t.trigger.(s) +. gs -. ev.(p);
      if t.plus && t.trigger.(s) < 0. then t.trigger.(s) <- 0.)
    gain

let iterate t =
  strategies t;
  let flat, x = fixed_point t.tp ~lambda:t.lambda ~local:t.local in
  Array.blit flat 0 t.flat 0 (Array.length flat);
  let played, plan =
    match t.rng with
    | None -> (flat, x)
    | Some rng ->
        let p = sample t.tp rng flat in
        (p, plans t.tp p)
  in
  let g, u = gradients t.tp played in
  Correlated.record t.mixture plan g u;
  update t g x;
  t.iterations <- t.iterations + 1

let run t n =
  for _ = 1 to n do
    iterate t
  done
