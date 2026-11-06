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

let disjoint (a, b) (c, d) = a <> c && a <> d && b <> c && b <> d

let mass (opp : Range.t) hand =
  let m = ref 0. in
  Array.iteri
    (fun j c -> if disjoint hand c then m := !m +. opp.weights.(j))
    opp.hands;
  !m

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
  Subgame.gadget gadget Resolve 1 (Array.make n (-1e9));
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
        Subgame.gadget river Resolve (1 - p) alt;
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

let worst xs = Array.fold_left Float.min infinity xs

let maxmargin_beats_resolve () =
  let t, prof = blueprint 3 in
  List.iter
    (fun i ->
      let r = List.nth (Subgame.roots t prof) i in
      for p = 0 to 1 do
        let margin kind =
          let river = Subgame.prepare (Safe kind) turn t prof r p in
          Subgame.run river 3000;
          worst (Subgame.margins river (Subgame.average river))
        in
        let mm = margin Maxmargin and rs = margin Resolve in
        if mm < -1e-3 then Alcotest.failf "%s player %d: margin %g" r.path p mm;
        if mm < rs -. 1e-3 then
          Alcotest.failf "%s player %d: maxmargin %g, resolve %g" r.path p mm rs
      done)
    [ 0; 61; 130; 200 ]

let gifts_from_folds () =
  let t, prof = blueprint 5 in
  let roots = Subgame.roots t prof in
  let gifts = Array.init 2 (fun i -> Subgame.gifts t prof i) in
  for i = 0 to 1 do
    Alcotest.(check int)
      "one per root" (List.length roots)
      (List.length gifts.(i));
    List.iter
      (fun (path, g) ->
        Array.iter (fun x -> if not (x >= 0.) then Alcotest.fail "negative") g;
        if String.sub path 0 3 = "xx/" then Array.iter (close 0. path 0.) g)
      gifts.(i)
  done;
  let hands = (Subgame.range t 1).hands and r0 = Subgame.range t 0 in
  let s = Subgame.strategy t prof in
  let probs c = Option.get (Strategy.find s (Subgame.key c "")) in
  let b1 =
    Option.get (Array.find_index (( = ) "b1") (probs r0.hands.(0)).actions)
  in
  let bet =
    Array.mapi (fun j c -> r0.weights.(j) *. (probs c).probs.(b1)) r0.hands
  in
  let called =
    List.filter (fun (r : Subgame.root) -> String.sub r.path 0 4 = "b1c/") roots
  in
  Alcotest.(check int) "48 rivers" 48 (List.length called);
  let n = Array.length hands in
  let direct = Array.map (mass { r0 with weights = bet }) hands in
  let m = Array.make n 0.
  and call = Array.make n 0.
  and used = Array.make n 0. in
  let ids = Hashtbl.create n in
  Array.iteri (fun h c -> Hashtbl.replace ids c h) hands;
  List.iter
    (fun (r : Subgame.root) ->
      let alt solving =
        Subgame.alternatives (Subgame.prepare solving turn t prof r 0)
      in
      let plain = alt (Safe Resolve) and reach = alt (Reach Resolve) in
      let river = Subgame.prepare (Safe Resolve) turn t prof r 0 in
      Array.iteri
        (fun x c ->
          let h = Hashtbl.find ids c in
          m.(h) <- m.(h) +. (mass r.reach.(0) c /. 44.);
          call.(h) <- call.(h) +. (plain.(x) /. 44.);
          used.(h) <- used.(h) +. ((reach.(x) -. plain.(x)) /. 44.))
        (Subgame.range river 1).hands)
    called;
  let g = List.assoc (List.hd called).path gifts.(1) in
  let spread = Array.fold_left (fun a x -> Float.max a (Float.abs x)) 0. call in
  let positive = ref 0 in
  for h = 0 to n - 1 do
    close 1e-12 "split" direct.(h) m.(h);
    let want = Float.max 0. ((-2. *. m.(h)) -. call.(h)) in
    if want > 0. then incr positive;
    close (1e-9 *. spread) "gift" want (g.(h) *. m.(h));
    close (1e-9 *. spread) "spent" want used.(h)
  done;
  if !positive = 0 then Alcotest.fail "no gifts on the call line"

let per_hand_bound () =
  let t, prof = blueprint 30 in
  let before = Array.init 2 (fun i -> Subgame.cbv t i prof) in
  List.iter
    (fun solving ->
      let after = Subgame.resolve solving ~iterations:2000 turn t prof in
      for i = 0 to 1 do
        let spread =
          Array.fold_left (fun a x -> Float.max a (Float.abs x)) 0. before.(i)
        in
        Array.iteri
          (fun h x ->
            if x > before.(i).(h) +. (1e-4 *. spread) then
              Alcotest.failf "%s player %d hand %d: %g above %g"
                (Subgame.solving_name solving)
                i h x
                before.(i).(h))
          (Subgame.cbv t i after)
      done)
    Subgame.[ Safe Resolve; Safe Maxmargin; Reach Resolve; Reach Maxmargin ]

let decision t (prof : Subgame.profile) player path =
  let s = Subgame.strategy t prof and hands = (Subgame.range t player).hands in
  let fits rows =
    Array.length rows.(0) = Array.length hands
    && Array.for_all2
         (fun h c ->
           match Strategy.find s (Subgame.key c path) with
           | Some e ->
               Array.length e.probs = Array.length rows
               && Array.for_all2 (fun p row -> row.(h) = p) e.probs rows
           | None -> false)
         (Array.init (Array.length hands) Fun.id)
         hands
  in
  Option.get (Array.find_index fits prof)

let unreached_hands () =
  let t, prof = blueprint 30 in
  let d = decision t prof 1 "b1" in
  let hands = (Subgame.range t 1).hands in
  let strong = Range.of_string ~board "77, AK" in
  Array.iteri
    (fun h c ->
      if Array.mem c strong.hands then
        Array.iteri
          (fun a row -> row.(h) <- (if a = 0 then 1. else 0.))
          prof.(d))
    hands;
  let before = Subgame.cbv t 1 prof in
  let after =
    Subgame.cbv t 1
      (Subgame.resolve (Safe Resolve) ~iterations:2000 turn t prof)
  in
  let spread =
    Array.fold_left (fun a x -> Float.max a (Float.abs x)) 0. before
  in
  Array.iteri
    (fun h x ->
      if x > before.(h) +. (1e-4 *. spread) then
        Alcotest.failf "%s: %g above %g"
          (Range.combo_to_string hands.(h))
          x before.(h))
    after

let safe_never_worse () =
  List.iter
    (fun iterations ->
      let t, prof = blueprint iterations in
      let before = Subgame.exploitability t prof in
      List.iter
        (fun solving ->
          let after =
            Subgame.exploitability t
              (Subgame.resolve solving ~iterations:200 turn t prof)
          in
          if not (after < before) then
            Alcotest.failf "%s, %d iterations: %g to %g"
              (Subgame.solving_name solving)
              iterations before after)
        Subgame.[ Safe Resolve; Safe Maxmargin; Reach Resolve; Reach Maxmargin ])
    [ 1; 10; 100 ]

let unsafe_is_a_profile () =
  let t, prof = blueprint 10 in
  let out = Subgame.resolve Unsafe ~iterations:50 turn t prof in
  Array.iter
    (fun rows ->
      Array.iteri
        (fun h _ ->
          close 1e-9 "sums to one" 1.
            (Array.fold_left (fun acc row -> acc +. row.(h)) 0. rows))
        rows.(0))
    out

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
    Alcotest.test_case "maxmargin margins" `Slow maxmargin_beats_resolve;
    Alcotest.test_case "gifts" `Quick gifts_from_folds;
    Alcotest.test_case "per-hand bound at the turn root" `Slow per_hand_bound;
    Alcotest.test_case "hands the blueprint folds" `Slow unreached_hands;
    Alcotest.test_case "safe re-solving never hurts" `Slow safe_never_worse;
    Alcotest.test_case "unsafe re-solving" `Quick unsafe_is_a_profile;
    Alcotest.test_case "folded trees" `Quick rejects;
  ]
