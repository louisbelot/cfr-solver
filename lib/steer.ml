open Multi_tree

type stats = { welfare : float; off : float; payment : float }

type t = {
  tp : Treeplex.t;
  bonus : Treeplex.t;
  fine : Treeplex.t;
  learner : Learner.t;
  alpha : int -> float;
  pay : float;
  mutable iterations : int;
  mutable sum : stats;
  mutable last : stats;
}

let flags (tree : Multi_tree.t) direct =
  let n = tree.players in
  let rec go node ok =
    match node with
    | Leaf _ ->
        let all = Array.for_all Fun.id ok in
        ( Leaf (Array.make n (if all then 1. else 0.)),
          Leaf (Array.init n (fun i -> if ok.(i) && not all then 1. else 0.)) )
    | Chance (q, children) ->
        let a, b = Array.split (Array.map (fun c -> go c ok) children) in
        (Chance (q, a), Chance (q, b))
    | Decision (k, children) ->
        let p = tree.owner.(k) in
        let a, b =
          Array.split
            (Array.mapi
               (fun j c ->
                 let ok' = Array.copy ok in
                 ok'.(p) <- ok.(p) && direct.(tree.offset.(k) + j) = 1.;
                 go c ok')
               children)
        in
        (Decision (k, a), Decision (k, b))
  in
  let a, b = go tree.root (Array.make n true) in
  (Treeplex.build { tree with root = a }, Treeplex.build { tree with root = b })

let zero = { welfare = 0.; off = 0.; payment = 0. }

let create ?(alpha = fun _ -> 0.) ?(pay = 0.) tree ~direct =
  let tp = Treeplex.build tree in
  let bonus, fine = flags tree direct in
  {
    tp;
    bonus;
    fine;
    learner = Learner.create ~plus:true tp;
    alpha;
    pay;
    iterations = 0;
    sum = zero;
    last = zero;
  }

let step t a flat =
  let n = t.tp.tree.players in
  let g, u = Treeplex.gradients t.tp flat in
  let gb, ub = Treeplex.gradients t.bonus flat in
  let gf, uf = Treeplex.gradients t.fine flat in
  let mix =
    Array.init n (fun i ->
        Array.mapi
          (fun s v -> v +. (a *. gb.(i).(s)) +. (t.pay *. gf.(i).(s)))
          g.(i))
  in
  let paid = ref 0. in
  for i = 0 to n - 1 do
    paid := !paid +. (a *. ub.(i)) +. (t.pay *. uf.(i))
  done;
  ( mix,
    {
      welfare = Array.fold_left ( +. ) 0. u;
      off = 1. -. ub.(0);
      payment = !paid;
    } )

let evaluate t ~alpha flat = snd (step t alpha flat)

let iterate t =
  let flat, _ = Learner.play t.learner in
  let mix, s = step t (t.alpha (t.iterations + 1)) flat in
  Learner.update t.learner mix;
  t.last <- s;
  t.sum <-
    {
      welfare = t.sum.welfare +. s.welfare;
      off = t.sum.off +. s.off;
      payment = t.sum.payment +. s.payment;
    };
  t.iterations <- t.iterations + 1

let run t k =
  for _ = 1 to k do
    iterate t
  done

let iterations t = t.iterations
let last t = t.last

let average t =
  let n = float_of_int (max 1 t.iterations) in
  {
    welfare = t.sum.welfare /. n;
    off = t.sum.off /. n;
    payment = t.sum.payment /. n;
  }

let current t = Learner.current t.learner
