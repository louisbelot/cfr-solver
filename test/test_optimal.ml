open Cfr_solver

let close ?(eps = 1e-9) what a b =
  if Float.abs (a -. b) > eps then Alcotest.failf "%s: %.12g vs %.12g" what a b

let setup items rounds =
  let tree =
    Multi_tree.build (Sheriff.game { Sheriff.baseline with items; rounds })
  in
  let plan = Efce_lp.plan tree in
  let lp = Option.get (Efce_lp.solve ~exact:true plan) in
  (tree, plan, lp, Optimal.create plan)

let three = lazy (setup 3 1)
let optimum (lp : Efce_lp.result) = lp.values.(0) +. lp.values.(1)

let lp_plan () =
  List.iter
    (fun (items, rounds) ->
      let _, _, lp, g = setup items rounds in
      close "welfare" (optimum lp) (Optimal.welfare g lp.x);
      let worst = Optimal.gain g lp.x in
      if worst > 1e-9 then Alcotest.failf "lp plan gains %g" worst)
    [ (1, 1); (2, 1); (3, 1); (1, 2) ]

let certificates () =
  let _, _, _, g = Lazy.force three in
  List.iter
    (fun mode ->
      let s = Optimal.solver ~predictive:true g mode in
      for _ = 1 to 20 do
        Optimal.run s 25;
        let b = Optimal.bounds s in
        if b.lower > b.upper +. 1e-12 then
          Alcotest.failf "lower %g above upper %g" b.lower b.upper
      done)
    [ Optimal.Threshold 4.; Threshold 6.; Penalty 8. ]

let sides () =
  let _, _, lp, g = Lazy.force three in
  let v = optimum lp in
  let verdict tau =
    (Optimal.decide ~eps:1e-4 ~predictive:true g tau).feasible
  in
  Alcotest.(check (option bool)) "below" (Some true) (verdict (v -. 0.05));
  Alcotest.(check (option bool)) "above" (Some false) (verdict (v +. 0.05))

let mass plan (x : float array) s0 s1 =
  x.(Option.get (Efce_lp.index plan s0 s1))

let bisect () =
  let tree, plan, lp, g = Lazy.force three in
  let eps = 1e-4 in
  let best, steps = Optimal.bisect ~eps ~predictive:true g in
  let v = optimum lp in
  let relaxed =
    optimum (Option.get (Efce_lp.solve ~exact:true ~slack:eps plan))
  in
  let w = Optimal.welfare g best.xi in
  if w < v -. (2. *. eps) then
    Alcotest.failf "welfare %g below %g" w (v -. (2. *. eps));
  if w > relaxed +. 1e-9 then
    Alcotest.failf "welfare %g above relaxed lp %g" w relaxed;
  if Optimal.gain g best.xi > eps then
    Alcotest.failf "gain %g" (Optimal.gain g best.xi);
  if Efce_lp.residual plan best.xi > 1e-9 then Alcotest.fail "infeasible plan";
  List.iter
    (fun (st : Optimal.step) ->
      if st.feasible = None then Alcotest.failf "undecided at %g" st.tau)
    steps;
  let med = Multi_tree.build (Mediator.game tree (mass plan best.xi)) in
  let d = Mediator.direct med in
  let profile =
    Strategy.union (Multi_br.strategy med 0 d) (Multi_br.strategy med 1 d)
  in
  let br = Array.fold_left Float.max 0. (Multi_br.gains med profile) in
  let gains = Optimal.gains g best.xi in
  let positive =
    Array.fold_left (fun acc x -> acc +. Float.max 0. x) 0. gains
  in
  if br < Optimal.gain g best.xi -. 1e-9 then
    Alcotest.failf "br %g below trigger" br;
  if br > positive +. 1e-9 then
    Alcotest.failf "br %g above trigger sum %g" br positive;
  close ~eps:1e-9 "mediated welfare" w
    (Array.fold_left ( +. ) 0. (Multi_br.values med profile))

let penalty () =
  let _, _, lp, g = Lazy.force three in
  let run k =
    let s = Optimal.solver ~predictive:true g (Optimal.Penalty k) in
    Optimal.run s 20_000;
    Optimal.bounds s
  in
  let weak = run 1. and strong = run 256. in
  if weak.welfare < optimum lp +. 1. then
    Alcotest.failf "weak penalty welfare %g" weak.welfare;
  if strong.gain > 1e-4 then Alcotest.failf "strong penalty gain %g" strong.gain;
  close ~eps:1e-3 "strong penalty welfare" (optimum lp) strong.welfare

let tests =
  [
    Alcotest.test_case "lp plan" `Quick lp_plan;
    Alcotest.test_case "certificates" `Quick certificates;
    Alcotest.test_case "sides" `Quick sides;
    Alcotest.test_case "bisect" `Quick bisect;
    Alcotest.test_case "penalty" `Quick penalty;
  ]
