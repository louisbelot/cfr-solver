open Cfr_solver

let q = Rat.of_ints
let rat = Alcotest.testable Rat.pp Rat.equal
let grid rows = Array.map (Array.map Rat.of_int) rows

let solved name a x y v =
  let x', y', v' = Meta_game.matrix a in
  Alcotest.check rat (name ^ " value") v v';
  Alcotest.(check (array rat)) (name ^ " row") x x';
  Alcotest.(check (array rat)) (name ^ " column") y y'

let matrices () =
  let third = q 1 3 in
  solved "rps"
    (grid [| [| 0; -1; 1 |]; [| 1; 0; -1 |]; [| -1; 1; 0 |] |])
    [| third; third; third |] [| third; third; third |] Rat.zero;
  solved "2x2"
    (grid [| [| 3; -1 |]; [| -2; 1 |] |])
    [| q 3 7; q 4 7 |]
    [| q 2 7; q 5 7 |]
    (q 1 7);
  solved "saddle"
    (grid [| [| 4; 2; 5 |]; [| 1; 0; 7 |] |])
    [| Rat.one; Rat.zero |]
    [| Rat.zero; Rat.one; Rat.zero |]
    (Rat.of_int 2)

let tensor sizes f =
  let n = Array.fold_left ( * ) 1 sizes in
  let decode idx =
    let s = Array.make (Array.length sizes) 0 and r = ref idx in
    for p = Array.length sizes - 1 downto 0 do
      s.(p) <- !r mod sizes.(p);
      r := !r / sizes.(p)
    done;
    s
  in
  { Meta_game.sizes; u = Array.init n (fun idx -> f (decode idx)) }

let bimatrix a b =
  tensor
    [| Array.length a; Array.length a.(0) |]
    (fun s -> [| a.(s.(0)).(s.(1)); b.(s.(0)).(s.(1)) |])

let zero_sum a = bimatrix a (Array.map (Array.map Float.neg) a)
let close eps = Alcotest.(check (float eps))

let projection () =
  Alcotest.(check (array (float 1e-12)))
    "simplex" [| 0.4; 0.6; 0. |]
    (Meta_game.project 0. [| 0.5; 0.7; -0.2 |]);
  Alcotest.(check (array (float 1e-12)))
    "floor" [| 0.8; 0.1; 0.1 |]
    (Meta_game.project 0.1 [| 1.; 0.; 0. |]);
  Alcotest.(check (array (float 1e-12)))
    "inside" [| 0.2; 0.3; 0.5 |]
    (Meta_game.project 0.1 [| 0.2; 0.3; 0.5 |])

let replicator () =
  let pennies = zero_sum [| [| 1.; -1. |]; [| -1.; 1. |] |] in
  let w = Meta_game.prd ~iterations:20_000 ~dt:1e-2 ~gamma:0. pennies in
  close 0.02 "pennies row" 0.5 w.(0).(0);
  close 0.02 "pennies column" 0.5 w.(1).(0);
  let dominated = zero_sum [| [| 2.; 3. |]; [| 1.; 0. |] |] in
  let w = Meta_game.prd ~iterations:20_000 ~dt:1e-2 ~gamma:0. dominated in
  Alcotest.(check bool) "dominated row dies" true (w.(0).(0) > 0.95);
  let w = Meta_game.prd ~iterations:5_000 ~dt:1e-2 ~gamma:0.5 dominated in
  Array.iter
    (Array.iter (fun x ->
         Alcotest.(check bool) "floor" true (x >= 0.25 -. 1e-12)))
    w

let fixation () =
  let alpha = 3. and m = 7 in
  List.iter
    (fun d ->
      let naive =
        (1. -. exp (-.alpha *. d)) /. (1. -. exp (-.float m *. alpha *. d))
      in
      close 1e-12 "naive formula" naive (Meta_game.fixation ~alpha ~m d);
      close 1e-9 "detailed balance"
        (exp (float (m - 1) *. alpha *. d))
        (Meta_game.fixation ~alpha ~m d /. Meta_game.fixation ~alpha ~m (-.d)))
    [ 0.4; -0.4; 1e-3; -2. ];
  close 1e-12 "tie" (1. /. 7.) (Meta_game.fixation ~alpha ~m 0.);
  close 1e-9 "continuous at 0" (1. /. 7.) (Meta_game.fixation ~alpha ~m 1e-12);
  Alcotest.(check bool)
    "no overflow" true
    (Meta_game.fixation ~alpha:1e4 ~m:50 (-5.) = 0.)

let stationary_by_squaring ~alpha ~m (t : Meta_game.t) =
  let n = Array.length t.u in
  let k = t.sizes and np = Array.length t.sizes in
  let stride = Array.make np 1 in
  for p = np - 2 downto 0 do
    stride.(p) <- stride.(p + 1) * k.(p + 1)
  done;
  let eta = 1. /. float (Array.fold_left (fun a x -> a + x - 1) 0 k) in
  let rho d =
    if d = 0. then 1. /. float m
    else (1. -. exp (-.alpha *. d)) /. (1. -. exp (-.float m *. alpha *. d))
  in
  let c = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    for p = 0 to np - 1 do
      let sp = i / stride.(p) mod k.(p) in
      for alt = 0 to k.(p) - 1 do
        if alt <> sp then begin
          let j = i + ((alt - sp) * stride.(p)) in
          c.(i).(j) <- eta *. rho (t.u.(j).(p) -. t.u.(i).(p))
        end
      done
    done;
    c.(i).(i) <- 1. -. Array.fold_left ( +. ) 0. c.(i)
  done;
  let mul a b =
    Array.init n (fun i ->
        Array.init n (fun j ->
            let s = ref 0. in
            for l = 0 to n - 1 do
              s := !s +. (a.(i).(l) *. b.(l).(j))
            done;
            !s))
  in
  let p = ref c in
  for _ = 1 to 40 do
    p :=
      Array.map
        (fun row ->
          let t = Array.fold_left ( +. ) 0. row in
          Array.map (fun x -> x /. t) row)
        (mul !p !p)
  done;
  !p.(0)

let alpharank_chain () =
  let rng = Rng.make 21 in
  let random =
    tensor [| 2; 3; 2 |] (fun _ -> Array.init 3 (fun _ -> Rng.float rng))
  in
  let pd =
    bimatrix [| [| 3.; 0. |]; [| 5.; 1. |] |] [| [| 3.; 5. |]; [| 0.; 1. |] |]
  in
  List.iter
    (fun (name, t, alpha, m) ->
      Alcotest.(check (array (float 1e-9)))
        name
        (stationary_by_squaring ~alpha ~m t)
        (Meta_game.alpharank ~alpha ~m t))
    [
      ("random three-player", random, 1.5, 4); ("prisoner's dilemma", pd, 0.5, 3);
    ];
  let pi = Meta_game.alpharank ~alpha:50. ~m:50 pd in
  Alcotest.(check bool) "defect, defect" true (pi.(3) > 1. -. 1e-9);
  let rps =
    zero_sum [| [| 0.; -1.; 1. |]; [| 1.; 0.; -1. |]; [| -1.; 1.; 0. |] |]
  in
  let w = Meta_game.marginals rps (Meta_game.alpharank ~alpha:10. ~m:20 rps) in
  Array.iter (Array.iter (close 1e-9 "rps marginal" (1. /. 3.))) w

let tests =
  [
    Alcotest.test_case "matrix games" `Quick matrices;
    Alcotest.test_case "simplex projection" `Quick projection;
    Alcotest.test_case "replicator dynamics" `Quick replicator;
    Alcotest.test_case "fixation probability" `Quick fixation;
    Alcotest.test_case "alpha-rank chain" `Quick alpharank_chain;
  ]
