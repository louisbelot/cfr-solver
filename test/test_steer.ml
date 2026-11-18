open Cfr_solver

let close ?(eps = 1e-9) what a b =
  if Float.abs (a -. b) > eps then Alcotest.failf "%s: %.12g vs %.12g" what a b

let mediated ?(exact = true) g =
  let tree = Multi_tree.build g in
  let plan = Efce_lp.plan tree in
  let r = Option.get (Efce_lp.solve ~exact plan) in
  (r, Multi_tree.build (Mediator.game tree (Efce_lp.mass plan r)))

let profile (tree : Multi_tree.t) flat =
  Strategy.union (Multi_br.strategy tree 0 flat) (Multi_br.strategy tree 1 flat)

let check_direct ?(eps = 1e-9) what ((r : Efce_lp.result), med) =
  let d = profile med (Mediator.direct med) in
  let v = Multi_br.values med d in
  close ~eps (what ^ " row") r.values.(0) v.(0);
  close ~eps (what ^ " col") r.values.(1) v.(1);
  Array.iteri
    (fun i gain ->
      if gain > eps then Alcotest.failf "%s: player %d gains %g" what i gain)
    (Multi_br.gains med d)

let chicken () =
  let ((r, _) as m) = mediated Bimatrix.chicken in
  check_direct "chicken" m;
  close "welfare" 10.5 (r.values.(0) +. r.values.(1))

let sheriff () =
  List.iter
    (fun (items, rounds) ->
      check_direct
        (Printf.sprintf "sheriff %d/%d" items rounds)
        (mediated (Sheriff.game { Sheriff.baseline with items; rounds })))
    [ (1, 1); (2, 1); (3, 1); (1, 2) ]

let not_equilibrium () =
  let tree = Multi_tree.build Bimatrix.chicken in
  let swerve = [ -1; 1; 3 ] in
  let mass s0 s1 =
    if List.mem s0 swerve && List.mem s1 swerve then 1. else 0.
  in
  let med = Multi_tree.build (Mediator.game tree mass) in
  let d = profile med (Mediator.direct med) in
  let v = Multi_br.values med d and g = Multi_br.gains med d in
  close "value" 6. v.(0);
  close "row gain" 1. g.(0);
  close "col gain" 1. g.(1)

let recommended () =
  Alcotest.(check (option int))
    "follow" (Some 2)
    (Mediator.recommended "?0p#1.0.2");
  Alcotest.(check (option int)) "off" None (Mediator.recommended "?0p#1#off")

let three =
  lazy (mediated (Sheriff.game { Sheriff.baseline with items = 3; rounds = 1 }))

let at_direct () =
  let r, med = Lazy.force three in
  let d = Mediator.direct med in
  let t = Steer.create ~pay:4. med ~direct:d in
  let s = Steer.evaluate t ~alpha:0.25 d in
  close "welfare" (r.values.(0) +. r.values.(1)) s.welfare;
  close "off" 0. s.off;
  close "payment" 0.5 s.payment

let converges () =
  let r, med = Lazy.force three in
  let d = Mediator.direct med in
  let t = Steer.create ~alpha:(fun _ -> 0.1) ~pay:4. med ~direct:d in
  Steer.run t 2000;
  let s = Steer.last t in
  if s.off > 1e-6 then Alcotest.failf "off-direct mass %g" s.off;
  close ~eps:1e-6 "welfare" (r.values.(0) +. r.values.(1)) s.welfare;
  close ~eps:1e-6 "payment" 0.2 s.payment

let unsteered () =
  let r, med = Lazy.force three in
  let t = Steer.create med ~direct:(Mediator.direct med) in
  Steer.run t 2000;
  let s = Steer.average t in
  if s.off < 0.99 then Alcotest.failf "off-direct mass %g" s.off;
  if s.welfare > (r.values.(0) +. r.values.(1)) /. 3. then
    Alcotest.failf "welfare %g" s.welfare

let tests =
  [
    Alcotest.test_case "chicken mediator" `Quick chicken;
    Alcotest.test_case "sheriff mediator" `Quick sheriff;
    Alcotest.test_case "swerve is not a ce" `Quick not_equilibrium;
    Alcotest.test_case "recommendation keys" `Quick recommended;
    Alcotest.test_case "payments at direct" `Quick at_direct;
    Alcotest.test_case "steering converges" `Quick converges;
    Alcotest.test_case "no payments, no obedience" `Quick unsteered;
  ]
