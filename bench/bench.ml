(* Iterations per second and minor-heap words allocated per iteration, for
   every game and CFR variant. Run with: dune exec ./bench/bench.exe *)

open Cfr_solver

let measure (module G : Game.S) algorithm iterations =
  let module C = Cfr.Make (G) in
  let t = C.create ~algorithm () in
  C.run t 10;
  let words = Gc.minor_words () and start = Unix.gettimeofday () in
  C.run t iterations;
  let elapsed = Unix.gettimeofday () -. start in
  let words = Gc.minor_words () -. words in
  Printf.printf "%-6s %-14s %10.0f it/s %10.1f words/it\n" G.name
    (Cfr.algorithm_name algorithm)
    (float_of_int iterations /. elapsed)
    (words /. float_of_int iterations)

let () =
  List.iter
    (fun (game, iterations) ->
      List.iter
        (fun alg -> measure game alg iterations)
        Cfr.[ Vanilla; Plus; Linear; dcfr ])
    [ ((module Kuhn : Game.S), 200_000); ((module Leduc), 20_000) ]
