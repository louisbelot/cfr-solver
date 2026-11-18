open Treeplex

type t = {
  tp : Treeplex.t;
  rng : Rng.t option;
  learner : Learner.t;
  mixture : Correlated.t;
  mutable iterations : int;
}

let create ?plus ?rng tree =
  let tp = Treeplex.build tree in
  {
    tp;
    rng;
    learner = Learner.create ?plus tp;
    mixture = Correlated.create tp;
    iterations = 0;
  }

let iterations t = t.iterations
let mixture t = t.mixture
let current t = Learner.current t.learner
let average t = Learner.average t.learner

let iterate t =
  let flat, x = Learner.play t.learner in
  let played, plan =
    match t.rng with
    | None -> (flat, x)
    | Some rng ->
        let p = sample t.tp rng flat in
        (p, plans t.tp p)
  in
  let g, u = gradients t.tp played in
  Correlated.record t.mixture plan g u;
  Learner.update t.learner g;
  t.iterations <- t.iterations + 1

let run t n =
  for _ = 1 to n do
    iterate t
  done
