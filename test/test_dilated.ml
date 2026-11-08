open Cfr_solver
module D = Dilated.Make (Kuhn)
module B = Best_response.Make (Kuhn)
module E = Eval.Make (Kuhn)

let offset = [| 0; 2; 4; 7; 9 |]
let parent = [| -1; 0; 0; 1 |]
let order = [| 1; 2; 3; 0 |]

let objective eta g b =
  let x = Array.make (Array.length b) 0. and v = ref 0. in
  List.iter
    (fun i ->
      let p = parent.(i) in
      let reach = if p >= 0 then x.(p) else 1. in
      for k = offset.(i) to offset.(i + 1) - 1 do
        x.(k) <- reach *. b.(k);
        v := !v +. (x.(k) *. g.(k));
        if b.(k) > 0. then v := !v -. (reach *. b.(k) *. Float.log b.(k) /. eta)
      done)
    [ 0; 1; 2; 3 ];
  !v

let behavior weights =
  let b = Array.copy weights in
  for i = 0 to 3 do
    let lo = offset.(i) and hi = offset.(i + 1) in
    let s = ref 0. in
    for k = lo to hi - 1 do
      s := !s +. b.(k)
    done;
    for k = lo to hi - 1 do
      b.(k) <- b.(k) /. !s
    done
  done;
  b

let response_maximizes () =
  let gen =
    QCheck2.Gen.(
      triple (float_range 0.1 3.)
        (array_size (return 9) (float_range (-5.) 5.))
        (array_size (return 9) (float_range 0.01 1.)))
  in
  QCheck2.Test.check_exn
  @@ QCheck2.Test.make ~count:300 ~name:"argmax" gen (fun (eta, g, w) ->
      let b = Dilated.response ~offset ~parent ~order ~eta g in
      let best = objective eta g b in
      let other = behavior w in
      let mix = Array.map2 (fun x y -> (0.99 *. x) +. (0.01 *. y)) b other in
      best >= objective eta g other -. 1e-12
      && best >= objective eta g mix -. 1e-12)

let response_at_zero () =
  let b = Dilated.response ~offset ~parent ~order ~eta:1. (Array.make 9 0.) in
  Alcotest.(check (array (float 1e-15)))
    "entropy below"
    [| 0.75; 0.25; 0.5; 0.5; 1. /. 3.; 1. /. 3.; 1. /. 3.; 0.5; 0.5 |]
    b

let solve ?optimistic eta n =
  let t = D.create ?optimistic ~eta () in
  D.run t n;
  t

let first_iteration () =
  let s = D.average_strategy (solve 1. 1) in
  List.iter
    (fun (key, pass) ->
      Alcotest.(check (array (float 1e-15)))
        key
        [| pass; 1. -. pass |]
        (Strategy.probs s key ~n:2))
    [ ("J", 2. /. 3.); ("Jp", 0.5); ("Jb", 0.5); ("Kpb", 0.5) ]

let average_converges () =
  let t = solve 1. 2000 in
  let e = B.exploitability (D.average_strategy t) in
  if e > 2e-3 then Alcotest.failf "average exploitability %g" e;
  let v = E.expected_value (D.average_strategy t) in
  if Float.abs (v +. (1. /. 18.)) > 1e-3 then Alcotest.failf "value %g" v

let optimism_fixes_last_iterate () =
  let last optimistic =
    B.exploitability (D.current_strategy (solve ~optimistic 1. 5000))
  in
  let o = last true and plain = last false in
  if o > 1e-4 then Alcotest.failf "optimistic last iterate %g" o;
  if plain < 100. *. o then
    Alcotest.failf "plain %g against optimistic %g" plain o

let bad_eta () =
  Alcotest.check_raises "eta"
    (Invalid_argument "Dilated.create: eta must be positive") (fun () ->
      ignore (D.create ~eta:0. ()))

let tests =
  [
    Alcotest.test_case "response maximizes" `Quick response_maximizes;
    Alcotest.test_case "response at zero" `Quick response_at_zero;
    Alcotest.test_case "first iteration" `Quick first_iteration;
    Alcotest.test_case "average converges" `Quick average_converges;
    Alcotest.test_case "optimism and the last iterate" `Quick
      optimism_fixes_last_iterate;
    Alcotest.test_case "bad eta" `Quick bad_eta;
  ]
