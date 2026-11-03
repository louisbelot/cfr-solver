open Cfr_solver

let turn = Bet_tree.limit ~cap:2 ~pot:4. ~bet:1. 2
let board = Hand.parse "Ah Kd 7c 2h"
let r0 = Range.of_string "AA, KK, AK, 77:0.5, A7s, QJs, 22, T9s:0.25"
let r1 = Range.of_string "AK, KQ, 77, A2s, JTs, 33:0.75"

let blueprint iterations =
  let t = Subgame.create ~algorithm:Cfr.dcfr ~iso:false turn ~board r0 r1 in
  Subgame.run t iterations;
  (t, Subgame.average t)

let river_of (r : Subgame.root) =
  Subgame.create ~algorithm:Cfr.dcfr
    (Subgame.river_config turn r)
    ~board:r.board r.reach.(0) r.reach.(1)

let close tol name x y =
  if not (Float.abs (x -. y) <= tol) then
    Alcotest.failf "%s: expected %.12g, got %.12g" name x y

let roots () =
  let t, prof = blueprint 5 in
  let roots = Subgame.roots t prof in
  Alcotest.(check int) "five lines, 48 rivers" 240 (List.length roots);
  let mass = Hashtbl.create 256 in
  List.iter
    (fun (l : Subgame.line) -> Hashtbl.replace mass l.path l.mass)
    (Subgame.lines ~chance:true t prof);
  List.iter
    (fun (r : Subgame.root) ->
      if Array.length r.board <> 5 then Alcotest.fail "board";
      close 1e-12 r.path (Hashtbl.find mass r.path) (Range.total r.reach.(0)))
    roots;
  List.iter
    (fun (path, pot) ->
      let r = List.find (fun (r : Subgame.root) -> r.path = path) roots in
      close 0. path pot r.pot)
    [ ("xx/3s", 4.); ("b1c/Qd", 6.); ("xb1c/3s", 6.); ("b1b2c/9c", 8.) ]

let identity () =
  let t, prof = blueprint 7 in
  List.iter
    (fun (r : Subgame.root) ->
      let river = river_of r in
      let sub = Subgame.restrict t prof r.path river in
      for p = 0 to 1 do
        if Subgame.graft t prof r.path river sub p <> prof then
          Alcotest.failf "%s player %d" r.path p
      done)
    (List.filteri (fun i _ -> i mod 17 = 0) (Subgame.roots t prof))

let cbv_sums_to_best_response () =
  let t, prof = blueprint 4 in
  let r = List.nth (Subgame.roots t prof) 30 in
  let river = river_of r in
  let sub = Subgame.restrict t prof r.path river in
  let a = Subgame.range river 0 and b = Subgame.range river 1 in
  let pairs = ref 0. in
  Array.iteri
    (fun h (c, d) ->
      Array.iteri
        (fun o (e, f) ->
          if c <> e && c <> f && d <> e && d <> f then
            pairs := !pairs +. (a.weights.(h) *. b.weights.(o)))
        b.hands)
    a.hands;
  for i = 0 to 1 do
    let w = (Subgame.range river i).weights in
    let total = ref 0. in
    Array.iteri
      (fun h x -> total := !total +. (w.(h) *. x))
      (Subgame.cbv river i sub);
    close 1e-12 "best response"
      (Subgame.best_response river i sub)
      (!total /. !pairs)
  done

let low_alternatives_change_nothing () =
  let t, prof = blueprint 3 in
  let r = List.nth (Subgame.roots t prof) 100 in
  let plain = river_of r and gadget = river_of r in
  let n = Range.size (Subgame.range gadget 1) in
  Subgame.gadget gadget 1 (Array.make n (-1e9));
  Subgame.run plain 50;
  Subgame.run gadget 50;
  if Subgame.average plain <> Subgame.average gadget then
    Alcotest.fail "averages differ";
  Array.iter (fun e -> close 0. "enters" 1. e) (Subgame.entry gadget)

let gadget_bounds_cbv () =
  let t, prof = blueprint 3 in
  List.iter
    (fun i ->
      let r = List.nth (Subgame.roots t prof) i in
      for p = 0 to 1 do
        let river = river_of r in
        let alt =
          Subgame.cbv river (1 - p) (Subgame.restrict t prof r.path river)
        in
        Subgame.gadget river (1 - p) alt;
        Subgame.run river 2000;
        let after = Subgame.cbv river (1 - p) (Subgame.average river) in
        let spread =
          Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. alt
        in
        Array.iteri
          (fun h x ->
            if x > alt.(h) +. (1e-6 *. spread) then
              Alcotest.failf "%s player %d hand %d: %g above %g" r.path p h x
                alt.(h))
          after
      done)
    [ 0; 61; 130; 200 ]

let rejects () =
  let t =
    Subgame.create ~algorithm:Cfr.dcfr turn ~board:(Hand.parse "Ah Kh 7h 2h") r0
      r0
  in
  match Subgame.roots t (Subgame.average t) with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "folded tree accepted"

let tests =
  [
    Alcotest.test_case "river roots" `Quick roots;
    Alcotest.test_case "restrict then graft" `Quick identity;
    Alcotest.test_case "cbv" `Quick cbv_sums_to_best_response;
    Alcotest.test_case "low alternatives change nothing" `Quick
      low_alternatives_change_nothing;
    Alcotest.test_case "gadget bounds the opponent" `Slow gadget_bounds_cbv;
    Alcotest.test_case "folded trees" `Quick rejects;
  ]
