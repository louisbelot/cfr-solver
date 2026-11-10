open Cfr_solver

let size () =
  Alcotest.(check int)
    "weights and biases" 26
    (Mlp.size (Mlp.create (Rng.make 1) [ 3; 4; 2 ]))

let first_adam_step () =
  let net = Mlp.create (Rng.make 2) [ 2; 3; 1 ] in
  let before =
    List.map (fun (p : Autodiff.t) -> Array.copy p.v) (Mlp.params net)
  in
  let tape = Autodiff.tape () in
  let out = Mlp.forward tape net (Autodiff.const 1 2 [| 0.5; -1. |]) in
  Autodiff.backward tape
    (Autodiff.mse tape out ~target:[| 10. |] ~mask:[| 1. |] ~weight:[| 1. |]);
  let grads =
    List.map (fun (p : Autodiff.t) -> Array.copy p.g) (Mlp.params net)
  in
  Mlp.update (Mlp.adam ~lr:0.1 net) net;
  List.iteri
    (fun k (p : Autodiff.t) ->
      Array.iteri
        (fun i v ->
          let g = (List.nth grads k).(i) and v0 = (List.nth before k).(i) in
          let expected =
            if g = 0. then v0 else v0 -. (0.1 *. Float.of_int (compare g 0.))
          in
          Alcotest.(check (float 1e-6)) "lr times sign" expected v;
          Alcotest.(check (float 0.)) "grad cleared" 0. p.g.(i))
        p.v)
    (Mlp.params net)

let fits_xor () =
  let net = Mlp.create (Rng.make 5) [ 2; 16; 1 ] in
  let opt = Mlp.adam ~lr:0.01 ~clip:1. net in
  let x = Autodiff.const 4 2 [| 0.; 0.; 0.; 1.; 1.; 0.; 1.; 1. |]
  and target = [| 0.; 1.; 1.; 0. |]
  and mask = Array.make 4 1.
  and weight = Array.make 4 1. in
  for _ = 1 to 2000 do
    let tape = Autodiff.tape () in
    Autodiff.backward tape
      (Autodiff.mse tape (Mlp.forward tape net x) ~target ~mask ~weight);
    Mlp.update opt net
  done;
  Array.iteri
    (fun i y -> Alcotest.(check (float 0.05)) "xor" target.(i) y)
    (Mlp.predict net ~rows:4 x.v)

let tests =
  [
    Alcotest.test_case "parameter count" `Quick size;
    Alcotest.test_case "first adam step" `Quick first_adam_step;
    Alcotest.test_case "fits xor" `Quick fits_xor;
  ]
