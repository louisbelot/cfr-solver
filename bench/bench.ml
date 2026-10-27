open Cfr_solver

let report game name ~iterations ~elapsed ~words ~nodes =
  let n = float_of_int iterations in
  Printf.printf "%-6s %-14s %10.0f it/s %8.1f Mnodes/s %8.1f words/it\n" game
    name (n /. elapsed)
    (float_of_int nodes /. elapsed /. 1e6)
    (words /. n)

let time f =
  let words = Gc.minor_words () and start = Unix.gettimeofday () in
  f ();
  (Unix.gettimeofday () -. start, Gc.minor_words () -. words)

let full (module G : Game.S) algorithm iterations =
  let module C = Cfr.Make (G) in
  let t = C.create ~algorithm () in
  C.run t 10;
  let before = C.touched t in
  let elapsed, words = time (fun () -> C.run t iterations) in
  report G.name
    (Cfr.algorithm_name algorithm)
    ~iterations ~elapsed ~words
    ~nodes:(C.touched t - before)

let sampled (module G : Game.S) sampling iterations =
  let module M = Mccfr.Make (G) in
  let t = M.create sampling in
  M.run t 10;
  let before = M.touched t in
  let elapsed, words = time (fun () -> M.run t iterations) in
  report G.name
    (Mccfr.sampling_name sampling)
    ~iterations ~elapsed ~words
    ~nodes:(M.touched t - before)

let parallel (module G : Game.S) mode domains iterations =
  let module P = Parallel.Make (G) in
  let t = P.create ~mode ~domains External in
  P.run t 1000;
  let before = P.touched t and start = Unix.gettimeofday () in
  P.run t iterations;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%-6s %-14s %10.0f it/s %8.1f Mnodes/s\n" G.name
    (Printf.sprintf "es %s x%d" (Parallel.mode_name mode) domains)
    (float_of_int iterations /. elapsed)
    (float_of_int (P.touched t - before) /. elapsed /. 1e6)

let chunked (module G : Game.S) domains calls iterations =
  let module P = Parallel.Make (G) in
  let t = P.create ~domains External in
  P.run t 1000;
  let start = Unix.gettimeofday () in
  for _ = 1 to calls do
    P.run t (iterations / calls)
  done;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%-6s %-14s %10.0f it/s %8.2f ms/call\n" G.name
    (Printf.sprintf "es x%d /%d" domains calls)
    (float_of_int iterations /. elapsed)
    (elapsed /. float_of_int calls *. 1e3)

let seven_cards () =
  let counts = Array.make 9 0 and seen = Bytes.make (9 lsl 17) '\000' in
  let h = Array.make 7 0 in
  let start = Unix.gettimeofday () in
  let rec go depth from =
    if depth = 7 then begin
      let v = Hand.eval h in
      let k = v lsr 20 in
      counts.(k) <- counts.(k) + 1;
      let byte = Char.code (Bytes.get seen (v lsr 3)) in
      Bytes.set seen (v lsr 3) (Char.chr (byte lor (1 lsl (v land 7))))
    end
    else
      for c = from to 45 + depth do
        h.(depth) <- c;
        go (depth + 1) (c + 1)
      done
  in
  go 0 0;
  let elapsed = Unix.gettimeofday () -. start in
  let total = Array.fold_left ( + ) 0 counts in
  let distinct = ref 0 in
  Bytes.iter (fun b -> distinct := !distinct + Hand.popcount (Char.code b)) seen;
  Printf.printf "hand   7-card eval    %10.1f M/s, %d hands, %d distinct\n"
    (float_of_int total /. elapsed /. 1e6)
    total !distinct;
  let expected =
    [|
      23294460;
      58627800;
      31433400;
      6461620;
      6180020;
      4047644;
      3473184;
      224848;
      41584;
    |]
  in
  if counts <> expected || !distinct <> 4824 then begin
    prerr_endline "7-card category counts are wrong";
    exit 1
  end

let subgame ?(iso = true) ?(single = false) name cfg board iterations =
  let board = Hand.parse board in
  let t =
    Subgame.create ~algorithm:Cfr.dcfr ~iso ~single cfg ~board (Range.full ())
      (Range.full ())
  in
  Subgame.run t 2;
  let start = Unix.gettimeofday () in
  Subgame.run t iterations;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%-6s %-14s %10.1f it/s %8.2f ms/it %8.1f MB\n" name
    (Printf.sprintf "dcfr%s%s"
       (if single then " f32" else "")
       (if iso then "" else " no-iso"))
    (float_of_int iterations /. elapsed)
    (elapsed /. float_of_int iterations *. 1e3)
    (float_of_int (Subgame.memory t) /. 1e6)

let pruning () =
  let board = Hand.parse "Qs Jh 8d 5c" in
  let r0 = Range.of_string ~board "TT+, AQ+, KQs, QJs, JTs, T9s, 98s"
  and r1 = Range.of_string ~board "88+, AJ+, KQ, QJs, JTs, A5s" in
  List.iter
    (fun prune ->
      let t =
        Subgame.create ~algorithm:Cfr.dcfr ~prune
          (Bet_tree.limit ~pot:4. ~bet:1. 2)
          ~board r0 r1
      in
      Subgame.run t 100;
      let skipped = Subgame.skipped t in
      let start = Unix.gettimeofday () in
      Subgame.run t 100;
      let elapsed = Unix.gettimeofday () -. start in
      Printf.printf "%-6s %-14s %10.1f it/s %8.2f ms/it %5d skips/it\n" "turn"
        (if prune then "dcfr prune" else "dcfr")
        (100. /. elapsed) (elapsed *. 10.)
        ((Subgame.skipped t - skipped) / 100))
    [ false; true ]

let () =
  seven_cards ();
  subgame "river" (Bet_tree.limit ~pot:6. ~bet:1. 1) "Qs Jh 8d 5c 2s" 2000;
  let turn = Bet_tree.limit ~pot:4. ~bet:1. 2 in
  subgame "turn" turn "Qs Jh 8d 5c" 5;
  subgame ~single:true "turn" turn "Qs Jh 8d 5c" 5;
  subgame ~iso:false "turn" turn "Qs Js 8s 5s" 5;
  subgame "turn" turn "Qs Js 8s 5s" 5;
  subgame ~single:true "turn" turn "Qs Js 8s 5s" 5;
  pruning ();
  List.iter
    (fun (game, iterations, samples) ->
      List.iter
        (fun alg -> full game alg iterations)
        Cfr.[ Vanilla; Plus; Linear; dcfr ];
      List.iter (fun s -> sampled game s samples) Mccfr.[ External; outcome ])
    [
      ((module Kuhn : Game.S), 200_000, 2_000_000);
      ((module Leduc), 20_000, 2_000_000);
    ];
  List.iter
    (fun d ->
      if d <= Domain.recommended_domain_count () then
        List.iter
          (fun mode -> parallel (module Leduc) mode d 4_000_000)
          Parallel.[ Shared; Private ])
    [ 2; 4; 8 ];
  List.iter
    (fun calls -> chunked (module Leduc) 8 calls 4_000_000)
    [ 1; 40; 400 ]
