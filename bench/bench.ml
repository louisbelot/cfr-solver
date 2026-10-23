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

let () =
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
    [ 2; 4; 8 ]
