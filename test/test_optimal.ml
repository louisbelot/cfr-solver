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

let assignments (tree : Multi_tree.t) infosets =
  List.fold_left
    (fun acc k ->
      List.concat_map
        (fun p ->
          List.init
            (tree.offset.(k + 1) - tree.offset.(k))
            (fun a -> (k, a) :: p))
        acc)
    [ [] ] infosets

let play (tree : Multi_tree.t) plans =
  let rec go : Multi_tree.node -> float array = function
    | Leaf u -> u
    | Decision (k, ch) -> go ch.(plans.(tree.owner.(k)).(k))
    | Chance _ -> invalid_arg "chance"
  in
  go tree.root

let pure_lp concept (tree : Multi_tree.t) =
  let tp = Treeplex.build tree in
  let n = Array.length tree.keys in
  let mine pl =
    List.filter (fun k -> tree.owner.(k) = pl) (List.init n Fun.id)
  in
  let plans pl =
    Array.of_list
      (List.map
         (fun choice ->
           let c = Array.make n 0 in
           List.iter (fun (k, a) -> c.(k) <- a) choice;
           c)
         (assignments tree (mine pl)))
  in
  let p0 = plans 0 and p1 = plans 1 in
  let cols = Array.length p1 in
  let joint =
    Array.init
      (Array.length p0 * cols)
      (fun v -> [| p0.(v / cols); p1.(v mod cols) |])
  in
  let u = Array.map (play tree) joint in
  let reaches c k =
    let rec up s =
      s < 0
      ||
      let k' = Treeplex.infoset tp s in
      c.(k') = s - tree.offset.(k') && up tp.parent.(k')
    in
    up tp.parent.(k)
  in
  let below k = Array.to_list tp.subtree.(k) in
  let triggers pl =
    match concept with
    | Efce_lp.Nfcce -> [ ((fun _ -> true), mine pl) ]
    | Efcce -> List.map (fun k -> ((fun c -> reaches c k), below k)) (mine pl)
    | Efce ->
        List.concat_map
          (fun k ->
            List.init
              (tree.offset.(k + 1) - tree.offset.(k))
              (fun a -> ((fun c -> reaches c k && c.(k) = a), below k)))
          (mine pl)
  in
  let rows = ref [] in
  for pl = 0 to 1 do
    List.iter
      (fun (cond, region) ->
        List.iter
          (fun cont ->
            let coeffs =
              List.filter_map
                (fun v ->
                  let c = joint.(v) in
                  if not (cond c.(pl)) then None
                  else
                    let d = Array.copy c in
                    d.(pl) <- Array.copy c.(pl);
                    List.iter (fun (k, a) -> d.(pl).(k) <- a) cont;
                    let loss = u.(v).(pl) -. (play tree d).(pl) in
                    if loss = 0. then None else Some (v, Rat.of_float loss))
                (List.init (Array.length joint) Fun.id)
            in
            rows := { Lp.coeffs; rel = Ge; rhs = Rat.zero } :: !rows)
          (assignments tree region))
      (triggers pl)
  done;
  let sum = List.init (Array.length joint) (fun v -> (v, Rat.one)) in
  let objective =
    List.mapi (fun v w -> (v, Rat.of_float (w.(0) +. w.(1)))) (Array.to_list u)
  in
  match
    Lp.maximize ~vars:(Array.length joint) objective
      ({ Lp.coeffs = sum; rel = Eq; rhs = Rat.one } :: !rows)
  with
  | Optimal { value; _ } -> Rat.to_float value
  | Infeasible | Unbounded -> Alcotest.fail "pure lp failed"

let matrix =
  Bimatrix.game "ce-cce"
    [|
      [| [| 4.; 1. |]; [| 0.; 1. |]; [| 2.; 4. |] |];
      [| [| 0.; 4. |]; [| 4.; 3. |]; [| 3.; 3. |] |];
      [| [| 4.; 1. |]; [| 2.; 1. |]; [| 3.; 0. |] |];
    |]

let concepts = Efce_lp.[ Efce; Efcce; Nfcce ]

let pure_plans () =
  List.iter
    (fun (name, g, want) ->
      let tree = Multi_tree.build g in
      let plan = Efce_lp.plan tree in
      List.iter2
        (fun concept w ->
          let lp =
            optimum (Option.get (Efce_lp.solve ~exact:true ~concept plan))
          in
          close ~eps:1e-9 (name ^ " pure") (pure_lp concept tree) lp;
          close ~eps:1e-6 (name ^ " value") w lp)
        concepts want)
    [
      ("3x3", matrix, [ 5.; 113. /. 17.; 113. /. 17. ]);
      ( "sheriff 3/1",
        Sheriff.game { Sheriff.baseline with items = 3; rounds = 1 },
        [ 420. /. 79.; 95. /. 12.; 540. /. 53. ] );
    ]

let coarse_learned () =
  let _, plan, _, _ = Lazy.force three in
  let eps = 1e-4 in
  List.iter
    (fun concept ->
      let g = Optimal.create ~concept plan in
      let best, _ = Optimal.bisect ~eps ~predictive:true g in
      let v = optimum (Option.get (Efce_lp.solve ~exact:true ~concept plan)) in
      let relaxed =
        optimum
          (Option.get (Efce_lp.solve ~exact:true ~slack:eps ~concept plan))
      in
      let w = Optimal.welfare g best.xi in
      if w < v -. (2. *. eps) || w > relaxed +. 1e-9 then
        Alcotest.failf "welfare %g outside [%g, %g]" w
          (v -. (2. *. eps))
          relaxed;
      if Optimal.gain g best.xi > eps then
        Alcotest.failf "gain %g" (Optimal.gain g best.xi))
    [ Efce_lp.Efcce; Nfcce ]

let tests =
  [
    Alcotest.test_case "lp plan" `Quick lp_plan;
    Alcotest.test_case "certificates" `Quick certificates;
    Alcotest.test_case "sides" `Quick sides;
    Alcotest.test_case "bisect" `Quick bisect;
    Alcotest.test_case "penalty" `Quick penalty;
    Alcotest.test_case "pure plans" `Quick pure_plans;
    Alcotest.test_case "coarse learned" `Quick coarse_learned;
  ]
