open Cfr_solver

let same_game (module G : Game.S) () =
  let module Check = Game_check.Make (G) in
  let module E = Eval.Make (G) in
  let c = Check.check () and tree = Tree.build (module G) in
  Alcotest.(check int) "histories" c.histories (Tree.size tree.root);
  Alcotest.(check int) "infosets" c.infosets (Array.length tree.infosets);
  List.iter
    (fun p ->
      let mine =
        Array.to_list tree.infosets
        |> List.filter (fun (i : Tree.infoset) -> i.player = p)
        |> List.map (fun (i : Tree.infoset) -> (i.key, i.actions))
      in
      Alcotest.(check (list (pair string (array string))))
        (Game.player_to_string p) (E.infosets p) mine)
    [ Game.P0; P1 ]

let shape () =
  let tree = Tree.build (module Kuhn) in
  Alcotest.(check (pair int int)) "kuhn" (5, 6) (Tree.shape tree.root);
  let leduc = Tree.build (module Leduc) in
  Alcotest.(check (pair int int)) "leduc" (12, 3) (Tree.shape leduc.root)

let tests =
  [
    Alcotest.test_case "shape" `Quick shape;
    Alcotest.test_case "kuhn" `Quick (same_game (module Kuhn));
    Alcotest.test_case "leduc" `Quick (same_game (module Leduc));
  ]
