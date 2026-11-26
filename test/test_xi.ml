open Cfr_solver

let sheriff items rounds = Sheriff.game { Sheriff.baseline with items; rounds }

let games =
  lazy
    (List.map
       (fun (name, g) ->
         let plan = Efce_lp.plan (Multi_tree.build g) in
         (name, plan, Xi.build plan))
       [
         ("chicken", Bimatrix.chicken);
         ("sheriff 1/1", sheriff 1 1);
         ("sheriff 2/1", sheriff 2 1);
         ("sheriff 3/1", sheriff 3 1);
         ("sheriff 1/2", sheriff 1 2);
         ("sheriff 2/2", sheriff 2 2);
       ])

let dot g x =
  let s = ref 0. in
  Array.iteri (fun i gi -> s := !s +. (gi *. x.(i))) g;
  !s

let random_gradient rng n =
  Array.init n (fun _ -> float_of_int (Rng.int rng 21 - 10))

let filled_once () =
  List.iter
    (fun (name, _, xi) ->
      let hits = Array.make (Xi.size xi) 0 in
      Array.iter
        (function
          | Xi.Fill { dst; _ } ->
              Array.iter (fun d -> hits.(d) <- hits.(d) + 1) dst
          | Sum { dst; _ } -> hits.(dst) <- hits.(dst) + 1)
        (Xi.ops xi);
      Array.iteri
        (fun i h ->
          let want = if i = Xi.root xi then 0 else 1 in
          if h <> want then
            Alcotest.failf "%s: entry %d filled %d times" name i h)
        hits)
    (Lazy.force games)

let feasible_iterates () =
  let rng = Rng.make 7 in
  List.iter
    (fun (name, plan, xi) ->
      let l = Xi.learner xi in
      for _ = 1 to 20 do
        let x = Xi.play l in
        let r = Efce_lp.residual plan x in
        if r > 1e-12 then Alcotest.failf "%s: residual %g" name r;
        Array.iter
          (fun v -> if v < 0. then Alcotest.failf "%s: negative" name)
          x;
        Xi.update l (random_gradient rng (Xi.size xi))
      done)
    (Lazy.force games)

let lp_max plan g =
  let rows =
    {
      Lp.coeffs = [ (Option.get (Efce_lp.index plan (-1) (-1)), Rat.one) ];
      rel = Eq;
      rhs = Rat.one;
    }
    :: List.map
         (fun (parent, children) ->
           {
             Lp.coeffs =
               (parent, Rat.neg Rat.one)
               :: Array.to_list (Array.map (fun c -> (c, Rat.one)) children);
             rel = Eq;
             rhs = Rat.zero;
           })
         (Efce_lp.consistency plan)
  in
  let objective =
    List.init (Array.length g) (fun i -> (i, Rat.of_float g.(i)))
  in
  match Lp.maximize ~vars:(Array.length g) objective rows with
  | Optimal { value; _ } -> Rat.to_float value
  | Infeasible | Unbounded -> Alcotest.fail "lp failed"

let best_matches_lp () =
  let rng = Rng.make 11 in
  List.iter
    (fun (name, plan, xi) ->
      if Xi.size xi < 400 then
        for _ = 1 to 3 do
          let g = random_gradient rng (Xi.size xi) in
          let v, x = Xi.best xi g in
          let want = lp_max plan g in
          if Float.abs (v -. want) > 1e-9 then
            Alcotest.failf "%s: %g vs lp %g" name v want;
          if Float.abs (dot g x -. v) > 1e-9 then
            Alcotest.failf "%s: argmax value" name;
          if Efce_lp.residual plan x > 1e-12 then
            Alcotest.failf "%s: argmax infeasible" name
        done)
    (Lazy.force games)

let no_regret () =
  let rng = Rng.make 3 in
  List.iter
    (fun (name, _, xi) ->
      let g = random_gradient rng (Xi.size xi) in
      let best, _ = Xi.best xi g in
      let l = Xi.learner xi in
      let total = ref 0. and weight = ref 0. in
      for t = 1 to 500 do
        let x = Xi.play l in
        total := !total +. (float_of_int t *. dot g x);
        weight := !weight +. float_of_int t;
        Xi.update l g
      done;
      let avg = !total /. !weight in
      if best -. avg > 1e-2 *. Float.max 1. (Float.abs best) then
        Alcotest.failf "%s: average %g, best %g" name avg best)
    (Lazy.force games)

let rejects_chance () =
  match Efce_lp.plan (Multi_tree.build (module (val Kuhn_n.game 2))) with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "kuhn has chance"

let tests =
  [
    Alcotest.test_case "filled once" `Quick filled_once;
    Alcotest.test_case "feasible iterates" `Quick feasible_iterates;
    Alcotest.test_case "best matches lp" `Quick best_matches_lp;
    Alcotest.test_case "no regret" `Quick no_regret;
    Alcotest.test_case "rejects chance" `Quick rejects_chance;
  ]
