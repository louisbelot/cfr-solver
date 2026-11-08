open Cfr_solver
module L = (val Leduc_n.game ~ranks:3 3)

let tree = lazy (Multi_tree.build (module L))
let profile seed = Test_multi_br.random_profile (module L) seed

let distance () =
  let tree = Lazy.force tree in
  let a = profile 1 and b = profile 2 in
  Array.iter
    (fun d -> Alcotest.(check (float 0.)) "self" 0. d)
    (Multi_profile.distance tree a a);
  let ab = Multi_profile.distance tree a b
  and ba = Multi_profile.distance tree b a in
  Array.iteri
    (fun i d ->
      Alcotest.(check (float 1e-15)) "symmetric" d ba.(i);
      if d <= 0. || d >= 1. then Alcotest.failf "player %d: %g" i d)
    ab

let swap player a b =
  let tree = Lazy.force tree and s = ref a in
  Array.iteri
    (fun k key ->
      if tree.owner.(k) = player then
        match Strategy.find b key with
        | Some e -> s := Strategy.add key ~actions:e.actions ~probs:e.probs !s
        | None -> ())
    tree.keys;
  !s

let mix_is_realization_equivalent () =
  let tree = Lazy.force tree in
  let a = profile 4 in
  let b = swap 1 a (profile 5) in
  let m = Multi_profile.mix tree [ a; b ] in
  let ua = Multi_br.values tree a
  and ub = Multi_br.values tree b
  and um = Multi_br.values tree m in
  Array.iteri
    (fun i u ->
      Alcotest.(check (float 1e-12)) "mean value" ((ua.(i) +. ub.(i)) /. 2.) u)
    um

let mix_of_one () =
  let tree = Lazy.force tree in
  let a = profile 6 in
  let m = Multi_profile.mix tree [ a; a ] in
  Array.iter
    (fun d -> Alcotest.(check (float 1e-12)) "same" 0. d)
    (Multi_profile.distance tree a m)

let tests =
  [
    Alcotest.test_case "distance" `Quick distance;
    Alcotest.test_case "mix keeps values" `Quick mix_is_realization_equivalent;
    Alcotest.test_case "mix of one" `Quick mix_of_one;
  ]
