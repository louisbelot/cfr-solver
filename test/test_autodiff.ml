open Cfr_solver

let random rng n = Array.init n (fun _ -> (2. *. Rng.float rng) -. 1.)

let graph rng =
  let x = Autodiff.const 3 4 (random rng 12)
  and w1 = Autodiff.param 4 5 (random rng 20)
  and b1 = Autodiff.param 1 5 (random rng 5)
  and w2 = Autodiff.param 5 3 (random rng 15)
  and b2 = Autodiff.param 1 3 (random rng 3) in
  let forward tape =
    let h =
      Autodiff.relu tape (Autodiff.add_bias tape (Autodiff.matmul tape x w1) b1)
    in
    Autodiff.add_bias tape (Autodiff.matmul tape h w2) b2
  in
  (forward, [ w1; b1; w2; b2 ])

let mask = [| 1.; 1.; 0.; 1.; 1.; 1.; 1.; 0.; 1. |]
let weight = [| 1.; 0.5; 2. |]

let mse rng =
  let target = random rng 9 in
  fun tape out -> Autodiff.mse tape out ~target ~mask ~weight

let xent _ =
  let target = [| 0.25; 0.75; 0.; 0.2; 0.3; 0.5; 1.; 0.; 0. |] in
  fun tape out -> Autodiff.cross_entropy tape out ~target ~mask ~weight

let gradcheck make () =
  let rng = Rng.make 3 in
  let forward, params = graph rng in
  let loss = make rng in
  let value () =
    let tape = Autodiff.inference () in
    (loss tape (forward tape) : Autodiff.t).v.(0)
  in
  let tape = Autodiff.tape () in
  Autodiff.backward tape (loss tape (forward tape));
  List.iter
    (fun (p : Autodiff.t) ->
      Array.iteri
        (fun i g ->
          let x = p.v.(i) and h = 1e-6 in
          p.v.(i) <- x +. h;
          let up = value () in
          p.v.(i) <- x -. h;
          let down = value () in
          p.v.(i) <- x;
          let numeric = (up -. down) /. (2. *. h) in
          if Float.abs (numeric -. g) > 1e-6 *. Float.max 1. (Float.abs g) then
            Alcotest.failf "gradient %g, finite difference %g" g numeric)
        p.g)
    params

let masked_entries_ignored () =
  let x = Autodiff.param 1 3 [| 0.3; 5.; -1. |] in
  let tape = Autodiff.tape () in
  let l =
    Autodiff.mse tape x ~target:[| 0.; 0.; 0. |] ~mask:[| 1.; 0.; 1. |]
      ~weight:[| 1. |]
  in
  Alcotest.(check (float 1e-12)) "loss" (0.09 +. 1.) l.v.(0);
  Autodiff.backward tape l;
  Alcotest.(check (array (float 1e-12))) "grad" [| 0.6; 0.; -2. |] x.g

let softmax () =
  let x = Autodiff.const 2 3 [| 1.; 2.; 300.; 0.; 0.; 0. |] in
  let p = Autodiff.softmax ~mask:[| 1.; 1.; 0.; 1.; 1.; 1. |] x in
  let e = exp 1. in
  Alcotest.(check (array (float 1e-12)))
    "masked rows"
    [| 1. /. (1. +. e); e /. (1. +. e); 0.; 1. /. 3.; 1. /. 3.; 1. /. 3. |]
    p

let inference_records_nothing () =
  let w = Autodiff.param 2 2 [| 1.; 2.; 3.; 4. |] in
  let tape = Autodiff.inference () in
  let y = Autodiff.matmul tape (Autodiff.const 1 2 [| 1.; 1. |]) w in
  Alcotest.(check (array (float 0.))) "value" [| 4.; 6. |] y.v;
  Alcotest.(check int) "no grad buffer" 0 (Array.length y.g)

let tests =
  [
    Alcotest.test_case "mse gradient" `Quick (gradcheck mse);
    Alcotest.test_case "cross-entropy gradient" `Quick (gradcheck xent);
    Alcotest.test_case "masked entries ignored" `Quick masked_entries_ignored;
    Alcotest.test_case "masked softmax" `Quick softmax;
    Alcotest.test_case "inference tape" `Quick inference_records_nothing;
  ]
