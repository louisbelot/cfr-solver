open Cfr_solver

let rec count = function
  | Tree.Terminal _ -> 1
  | Chance { children; _ } | Decision { children; _ } ->
      Array.fold_left (fun n c -> n + count c) 1 children

let same_game (module G : Game.S) () =
  let module Check = Game_check.Make (G) in
  let module E = Eval.Make (G) in
  let c = Check.check () and tree = Tree.build (module G) in
  Alcotest.(check int) "histories" c.histories (count tree.root);
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

let tests =
  [
    Alcotest.test_case "kuhn" `Quick (same_game (module Kuhn));
    Alcotest.test_case "leduc" `Quick (same_game (module Leduc));
  ]
