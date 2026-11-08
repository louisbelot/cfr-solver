open Cfr_solver
open Report
open Cmdliner
module S = Sequence_form

let liars : (string * (module Game.S)) list =
  List.concat_map
    (fun (dice, faces) ->
      List.map
        (fun wild ->
          let c = { Liars_dice.dice; faces; wild } in
          (Liars_dice.name c, (module (val Liars_dice.game c) : Game.S)))
        [ false; true ])
    [ (1, 2); (1, 3); (1, 4); (2, 2) ]

let games : (string * (module Game.S)) list =
  [
    ("kuhn", (module Kuhn : Game.S));
    ("leduc2", (module (val Leduc.game { Leduc.ranks = 2; suits = 2 }) : Game.S));
    ("leduc", (module Leduc : Game.S));
  ]
  @ liars

let algorithms =
  [
    ("cfr", Cfr.Vanilla);
    ("cfr+", Cfr.Plus);
    ("dcfr", Cfr.dcfr);
    ("pcfr+", Cfr.pcfr_plus);
    ("pdcfr+", Cfr.pdcfr_plus);
  ]

let timed f =
  let start = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. start)

let solve game cold iterations csv svg =
  let ((module G : Game.S) as g) = List.assoc game games in
  let module B = Best_response.Make (G) in
  let t, build = timed (fun () -> S.build g) in
  let sol, lp = timed (fun () -> S.solve ~warm:(not cold) t) in
  let x, y = sol.plans in
  let gap = Rat.add (S.best_response t P0 y) (S.best_response t P1 x) in
  Format.printf
    "%s: %d + %d sequences, %d + %d infosets, %d payoff entries (%.2fs)@."
    G.name (S.sequences t P0) (S.sequences t P1) (S.infosets t P0)
    (S.infosets t P1) (S.entries t) build;
  Format.printf "exact LP: %d + %d pivots, %.2fs%s@." (fst sol.pivots)
    (snd sol.pivots) lp
    (if cold then ", cold start" else "");
  Format.printf "value      %s@.           = %.12f@." (Rat.to_string sol.value)
    (Rat.to_float sol.value);
  Format.printf "nash conv  %s@.@." (Rat.to_string gap);
  if iterations > 0 then (
    let runs =
      List.map
        (fun (name, algorithm) ->
          let module C = Cfr.Make (G) in
          let c = C.create ~algorithm () in
          let rows =
            List.map
              (fun k ->
                C.run c (k - C.iterations c);
                let s = C.average_strategy c in
                let v = S.value t (S.plan t P0 s) (S.plan t P1 s) in
                ( k,
                  B.exploitability s,
                  Rat.to_float (S.exploitability t s),
                  Rat.to_float (Rat.sub v sol.value) ))
              (List.sort_uniq compare
                 (checkpoints iterations @ log_checkpoints 4 iterations))
          in
          (name, rows))
        algorithms
    in
    Format.printf
      "exploitability of the average strategy, float best response / exact@.";
    Format.printf "%9s" "iteration";
    List.iter (fun (name, _) -> Format.printf "%22s" name) runs;
    Format.printf "@.";
    List.iter
      (fun k ->
        Format.printf "%9d" k;
        List.iter
          (fun (_, rows) ->
            let _, f, e, _ = List.find (fun (k', _, _, _) -> k' = k) rows in
            Format.printf "%22s" (sci f ^ " / " ^ sci e))
          runs;
        Format.printf "@.")
      (checkpoints iterations);
    try
      Option.iter
        (fun path ->
          runs
          |> List.concat_map (fun (name, rows) ->
              List.map
                (fun (k, f, e, v) ->
                  Printf.sprintf "%s,%d,%s,%s,%s\n" name k (short_float f)
                    (short_float e) (short_float v))
                rows)
          |> String.concat ""
          |> ( ^ ) "algorithm,iteration,float,exact,value_error\n"
          |> write_file path)
        csv;
      Option.iter
        (fun path ->
          Plot.loglog
            ~title:
              (Printf.sprintf "%s: exact exploitability of the average strategy"
                 (String.capitalize_ascii G.name))
            ~xlabel:"iterations" ~ylabel:"exploitability"
            (List.map
               (fun (name, rows) ->
                 {
                   Plot.label = name;
                   dashed = false;
                   points = List.map (fun (k, _, e, _) -> (k, e)) rows;
                 })
               runs
            @ List.map
                (fun (name, rows) ->
                  {
                    Plot.label = name ^ ", float";
                    dashed = true;
                    points = List.map (fun (k, f, _, _) -> (k, f)) rows;
                  })
                (List.filter (fun (name, _) -> name = "pdcfr+") runs))
          |> write_file path)
        svg;
      Ok ()
    with Sys_error msg -> Error (`Msg msg))
  else Ok ()

let cmd =
  let names = List.map fst games in
  let game =
    Arg.(
      value
      & opt (enum (List.map (fun n -> (n, n)) names)) "kuhn"
      & info [ "g"; "game" ] ~docv:"GAME"
          ~doc:(Printf.sprintf "Game to solve: %s." (String.concat ", " names)))
  and cold =
    Arg.(
      value & flag
      & info [ "cold" ]
          ~doc:"Skip the floating-point warm start of the simplex.")
  and iterations =
    Arg.(
      value & opt int 0
      & info [ "n"; "iterations" ] ~docv:"N"
          ~doc:
            "Also run CFR, CFR+, DCFR and PCFR+ for $(docv) iterations and \
             measure their exact exploitability.")
  and csv =
    Arg.(
      value
      & opt (some string) None
      & info [ "csv" ] ~docv:"FILE"
          ~doc:"Write the exploitability curves to $(docv).")
  and svg =
    Arg.(
      value
      & opt (some string) None
      & info [ "svg" ] ~docv:"FILE"
          ~doc:"Plot the exact exploitability curves to $(docv).")
  in
  Cmd.v
    (Cmd.info "exact"
       ~doc:
         "Solve the sequence-form linear program in exact rational arithmetic \
          and print the game value as a fraction.")
    Term.(term_result (const solve $ game $ cold $ iterations $ csv $ svg))

let cmds = [ cmd ]
