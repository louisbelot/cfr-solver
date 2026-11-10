open Cfr_solver

let injective (module G : Game.S) (f : Features.t) () =
  let tree = Tree.build (module G) in
  let seen = Hashtbl.create 64 in
  Array.iter
    (fun (info : Tree.infoset) ->
      let x = f.encode info.key in
      Alcotest.(check int) "width" f.dim (Array.length x);
      Array.iter
        (fun v -> if v <> 0. && v <> 1. then Alcotest.fail "not binary")
        x;
      (match Hashtbl.find_opt seen x with
      | Some k -> Alcotest.failf "%s and %s collide" k info.key
      | None -> ());
      Hashtbl.add seen x info.key)
    tree.infosets

let leduc_layout () =
  let f = Features.leduc Leduc.standard in
  let x = f.encode "KQ:crc/r" in
  let on = List.filter (fun i -> x.(i) = 1.) (List.init f.dim Fun.id) in
  Alcotest.(check (list int)) "hot entries" [ 2; 4; 6; 9; 10; 15 ] on

let rejects () =
  Alcotest.check_raises "bad card"
    (Invalid_argument "Features: unexpected infoset X:c") (fun () ->
      ignore ((Features.leduc Leduc.standard).encode "X:c"))

let leduc5 = { Leduc.ranks = 5; suits = 2 }

let tests =
  [
    Alcotest.test_case "kuhn injective" `Quick
      (injective (module Kuhn) Features.kuhn);
    Alcotest.test_case "leduc injective" `Quick
      (injective (module Leduc) (Features.leduc Leduc.standard));
    Alcotest.test_case "leduc5 injective" `Quick
      (injective (Leduc.game leduc5 :> (module Game.S)) (Features.leduc leduc5));
    Alcotest.test_case "leduc layout" `Quick leduc_layout;
    Alcotest.test_case "rejects unknown" `Quick rejects;
  ]
