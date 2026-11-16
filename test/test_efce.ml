open Cfr_solver

let tree_of g = Multi_tree.build g
let kuhn2 = tree_of (module (val Kuhn_n.game 2))
let kuhn3 = tree_of (module (val Kuhn_n.game 3))

let close ?(eps = 1e-12) what a b =
  if Float.abs (a -. b) > eps then Alcotest.failf "%s: %.15g vs %.15g" what a b

let random_flat (tree : Multi_tree.t) rng =
  let f = Array.make (Multi_tree.slots tree) 0. in
  Array.iteri
    (fun k _ ->
      let lo = tree.offset.(k) and hi = tree.offset.(k + 1) in
      let w = Array.init (hi - lo) (fun _ -> 0.05 +. Rng.float rng) in
      let sum = Array.fold_left ( +. ) 0. w in
      Array.iteri (fun b v -> f.(lo + b) <- v /. sum) w)
    tree.keys;
  f

let stationary () =
  let check p want =
    let got = Markov.stationary p in
    Array.iteri (fun i w -> close ~eps:1e-12 "pi" w got.(i)) want
  in
  check [| [| 0.9; 0.1 |]; [| 0.5; 0.5 |] |] [| 5. /. 6.; 1. /. 6. |];
  check [| [| 0.; 1. |]; [| 1.; 0. |] |] [| 0.5; 0.5 |];
  check [| [| 0.5; 0.5 |]; [| 0.; 1. |] |] [| 0.; 1. |];
  let pi =
    Markov.stationary
      [| [| 1.; 0.; 0. |]; [| 0.; 1.; 0. |]; [| 0.3; 0.3; 0.4 |] |]
  in
  close "mass" 1. (Array.fold_left ( +. ) 0. pi);
  close "transient" 0. pi.(2)

let ancestors tp s =
  let rec go s acc =
    if s < 0 then acc
    else go tp.Treeplex.parent.(Treeplex.infoset tp s) (s :: acc)
  in
  go s []

let fixed_point tree seed () =
  let tp = Treeplex.build tree in
  let rng = Rng.make seed in
  let slots = Multi_tree.slots tree in
  let lambda =
    Array.init slots (fun _ ->
        if Rng.float rng < 0.3 then 0. else Rng.float rng)
  in
  let local =
    Array.init slots (fun s ->
        let k = Treeplex.infoset tp s in
        let sub = tp.subtree.(k) and pos = tp.local.(k) in
        let y = Array.make pos.(Array.length sub) 0. in
        Array.iteri
          (fun m j ->
            let n = Treeplex.actions tp j in
            let w =
              Array.init n (fun _ ->
                  if Rng.float rng < 0.3 then 0. else Rng.float rng)
            in
            let sum = Array.fold_left ( +. ) 0. w in
            Array.iteri
              (fun b v ->
                y.(pos.(m) + b) <-
                  (if sum > 0. then v /. sum else 1. /. float_of_int n))
              w)
          sub;
        y)
  in
  let flat, x = Efce.fixed_point tp ~lambda ~local in
  Array.iteri (fun s v -> close "plan" (Treeplex.plans tp flat).(s) v) x;
  let yseq s t =
    let k = Treeplex.infoset tp s in
    let pos = tp.local.(k) and sub = tp.subtree.(k) in
    let path =
      List.filter
        (fun a -> List.mem (Treeplex.infoset tp a) (Array.to_list sub))
        (ancestors tp t)
    in
    List.fold_left
      (fun acc a ->
        let j = Treeplex.infoset tp a in
        let m = ref 0 in
        Array.iteri (fun i j' -> if j' = j then m := i) sub;
        acc *. local.(s).(pos.(!m) + a - tree.offset.(j)))
      1. path
  in
  for t = 0 to slots - 1 do
    let p = tree.owner.(Treeplex.infoset tp t) in
    let anc = ancestors tp t in
    let mapped = ref 0. and total = ref 0. in
    for s = 0 to slots - 1 do
      let k = Treeplex.infoset tp s in
      if tree.owner.(k) = p then begin
        let l = lambda.(s) in
        total := !total +. l;
        let below = List.mem s anc in
        let inside = List.exists (fun a -> Treeplex.infoset tp a = k) anc in
        mapped :=
          !mapped
          +. (l *. if below then 0. else x.(t))
          +. if inside then l *. x.(s) *. yseq s t else 0.
      end
    done;
    if !total > 0. then close ~eps:1e-10 "phi(x) = x" x.(t) (!mapped /. !total)
  done

let pure_strategies (tree : Multi_tree.t) p =
  let mine =
    List.filter
      (fun k -> tree.owner.(k) = p)
      (List.init (Array.length tree.keys) Fun.id)
  in
  List.fold_left
    (fun acc k ->
      List.concat_map
        (fun pi ->
          List.init (Array.length tree.labels.(k)) (fun a -> (k, a) :: pi))
        acc)
    [ [] ] mine

let brute_gap () =
  let tree = kuhn2 in
  let tp = Treeplex.build tree in
  let rng = Rng.make 7 in
  let profiles = List.init 3 (fun _ -> random_flat tree rng) in
  let mix = Correlated.create tp in
  List.iter (Correlated.add mix) profiles;
  let set flat pi =
    let f = Array.copy flat in
    List.iter
      (fun (k, a) ->
        for b = 0 to Array.length tree.labels.(k) - 1 do
          f.(tree.offset.(k) + b) <- (if a = b then 1. else 0.)
        done)
      pi;
    f
  in
  let prob flat pi =
    List.fold_left (fun w (k, a) -> w *. flat.(tree.offset.(k) + a)) 1. pi
  in
  for s = 0 to Multi_tree.slots tree - 1 do
    let k = Treeplex.infoset tp s in
    let p = tree.owner.(k) and a = s - tree.offset.(k) in
    let sub = Array.to_list tp.subtree.(k) in
    let ys =
      List.map
        (List.filter (fun (j, _) -> List.mem j sub))
        (pure_strategies tree p)
      |> List.sort_uniq compare
    in
    let gain y =
      List.fold_left
        (fun acc flat ->
          List.fold_left
            (fun acc pi ->
              let dev =
                if List.assoc k pi = a then
                  List.map
                    (fun (j, b) ->
                      if List.mem j sub then (j, List.assoc j y) else (j, b))
                    pi
                else pi
              in
              let u pi = (Multi_br.payoff tree (set flat pi)).(p) in
              acc +. (prob flat pi *. (u dev -. u pi)))
            acc (pure_strategies tree p))
        0. profiles
      /. float_of_int (List.length profiles)
    in
    let best =
      List.fold_left (fun m y -> Float.max m (gain y)) Float.neg_infinity ys
    in
    close ~eps:1e-12
      (Printf.sprintf "trigger %s/%d" tree.keys.(k) a)
      best (Correlated.gain mix s)
  done

let correlated_equilibrium () =
  let tree = tree_of Bimatrix.chicken in
  let t = Efce.create tree in
  let mu = Array.make_matrix 2 2 0. in
  let n = 20000 in
  for _ = 1 to n do
    Efce.iterate t;
    let f = Efce.current t in
    for a = 0 to 1 do
      for b = 0 to 1 do
        mu.(a).(b) <- mu.(a).(b) +. (f.(a) *. f.(2 + b) /. float_of_int n)
      done
    done
  done;
  let pay =
    [| [| [| 0.; 0. |]; [| 7.; 2. |] |]; [| [| 2.; 7. |]; [| 6.; 6. |] |] |]
  in
  let u a b = pay.(a).(b) in
  let swap = ref 0. in
  for a = 0 to 1 do
    for b = 0 to 1 do
      let g1 = ref 0. and g2 = ref 0. in
      for o = 0 to 1 do
        g1 := !g1 +. (mu.(a).(o) *. ((u b o).(0) -. (u a o).(0)));
        g2 := !g2 +. (mu.(o).(a) *. ((u o b).(1) -. (u o a).(1)))
      done;
      swap := Float.max !swap (Float.max !g1 !g2)
    done
  done;
  let mix = Efce.mixture t in
  close ~eps:1e-9 "swap gain" !swap (Correlated.gap mix);
  if !swap > 0.01 then Alcotest.failf "ce gap %g" !swap

let zero_sum () =
  let t = Efce.create ~plus:true kuhn2 in
  Efce.run t 3000;
  let mix = Efce.mixture t in
  let gap = Correlated.gap mix and v = Correlated.values mix in
  if gap > 4e-3 then Alcotest.failf "gap %g" gap;
  close ~eps:5e-3 "kuhn value" (-1. /. 18.) v.(0)

let three_player () =
  let t = Efce.create kuhn3 in
  Efce.run t 200;
  let early = Correlated.gap (Efce.mixture t) in
  Efce.run t 1800;
  let late = Correlated.gap (Efce.mixture t) in
  if not (late < early /. 2.) then Alcotest.failf "gap %g -> %g" early late

let welfare ?exact ?objective g =
  match Efce_lp.solve ?exact ?objective (Efce_lp.plan (tree_of g)) with
  | Some r -> r.values
  | None -> Alcotest.fail "lp failed"

let chicken_welfare () =
  let v = welfare ~exact:true Bimatrix.chicken in
  close "welfare" 10.5 (v.(0) +. v.(1));
  close "row" 5.25 v.(0)

let zero_sum_value () =
  let g =
    Bimatrix.game "zs"
      [|
        [| [| 2.; -2. |]; [| -1.; 1. |] |]; [| [| -1.; 1. |]; [| 1.; -1. |] |];
      |]
  in
  close "value" 0.2 (welfare ~exact:true ~objective:(Player 0) g).(0);
  close "min" (-0.2) (welfare ~exact:true ~objective:(Player 1) g).(1)

let sheriff_table () =
  let check items want =
    let v =
      welfare (Sheriff.game { Sheriff.baseline with items; rounds = 1 })
    in
    close ~eps:6e-3 (Printf.sprintf "n=%d" items) want (v.(0) +. v.(1))
  in
  check 1 5.;
  check 2 10.;
  check 3 (3.89 +. 1.43);
  check 5 (2.28 +. 1.26);
  check 10 (1.76 +. 0.93)

let sheriff_game () =
  let c = { Sheriff.baseline with items = 2; rounds = 2 } in
  let tree = tree_of (Sheriff.game c) in
  let rec leaves = function
    | Multi_tree.Leaf u -> [ u ]
    | Chance (_, cs) | Decision (_, cs) ->
        List.concat_map leaves (Array.to_list cs)
  in
  let us = leaves tree.root in
  Alcotest.(check int) "leaves" (3 * 9 * 4) (List.length us);
  Alcotest.(check int)
    "infosets"
    (1 + (3 * (1 + 6)) + 3 + 18)
    (Array.length tree.keys);
  List.iter
    (fun u ->
      if
        not
          (List.mem (u.(0) +. u.(1)) [ 0.; 5.; 10. ]
          && u.(1) >= -1.
          && u.(1) <= 2.)
      then Alcotest.failf "payoff (%g, %g)" u.(0) u.(1))
    us

let tests =
  [
    Alcotest.test_case "stationary distributions" `Quick stationary;
    Alcotest.test_case "fixed point kuhn" `Quick (fixed_point kuhn2 3);
    Alcotest.test_case "fixed point kuhn3" `Quick (fixed_point kuhn3 5);
    Alcotest.test_case "gap matches brute force" `Quick brute_gap;
    Alcotest.test_case "chicken correlated" `Quick correlated_equilibrium;
    Alcotest.test_case "kuhn value" `Quick zero_sum;
    Alcotest.test_case "kuhn3 gap shrinks" `Quick three_player;
    Alcotest.test_case "chicken welfare 10.5" `Quick chicken_welfare;
    Alcotest.test_case "zero-sum value" `Quick zero_sum_value;
    Alcotest.test_case "sheriff table 1" `Quick sheriff_table;
    Alcotest.test_case "sheriff game" `Quick sheriff_game;
  ]
