open Cfr_solver
open Report
open Cmdliner

let games : (string * (module Multi.S)) list =
  [
    ("kuhn3", (module (val Kuhn_n.game 3)));
    ("kuhn4", (module (val Kuhn_n.game 4)));
    ("leduc3", (module (val Leduc_n.game 3)));
    ("leduc3-6", (module (val Leduc_n.game ~ranks:3 3)));
    ("leduc4", (module (val Leduc_n.game 4)));
  ]

let algorithms =
  [ ("cfr", Cfr.Vanilla); ("cfr+", Plus); ("lcfr", Linear); ("dcfr", Cfr.dcfr) ]

let floats u = String.concat "," (Array.to_list (Array.map short_float u))

let solve game chosen iterations csv svg =
  let (module G : Multi.S) = List.assoc game games in
  let module C = Multi_cfr.Make (G) in
  let runs =
    List.map
      (fun (name, algorithm) ->
        let t = C.create ~algorithm () and cpu = ref 0. in
        let rows =
          List.map
            (fun k ->
              let start = Sys.time () in
              C.run t (k - C.iterations t);
              cpu := !cpu +. (Sys.time () -. start);
              let s = C.average_strategy t in
              (k, Multi_br.gains (C.tree t) s, Multi_br.values (C.tree t) s))
            (checkpoints iterations)
        in
        (name, rows, !cpu, C.infosets t))
      chosen
  in
  let total g = Array.fold_left ( +. ) 0. g in
  (match runs with
  | (_, _, _, infosets) :: _ ->
      Format.printf "%s: %d players, %d infosets@.@." G.name G.players infosets
  | [] -> ());
  Format.printf "nash conv of the average strategy@.%10s" "iteration";
  List.iter (fun (name, _, _, _) -> Format.printf "%11s" name) runs;
  Format.printf "@.";
  List.iteri
    (fun i k ->
      Format.printf "%10d" k;
      List.iter
        (fun (_, rows, _, _) ->
          let _, g, _ = List.nth rows i in
          Format.printf "%11s" (sci (total g)))
        runs;
      Format.printf "@.")
    (checkpoints iterations);
  Format.printf "@.%-6s %10s  %-30s %s@." "" "ms/it" "values" "gains";
  List.iter
    (fun (name, rows, cpu, _) ->
      let _, g, u = List.nth rows (List.length rows - 1) in
      Format.printf "%-6s %10.3f  %-30s %s@." name
        (1000. *. cpu /. float_of_int iterations)
        (String.concat " "
           (Array.to_list (Array.map (Printf.sprintf "%+.6f") u)))
        (String.concat " " (Array.to_list (Array.map sci g))))
    runs;
  try
    Option.iter
      (fun path ->
        let players f = String.concat "," (List.init G.players f) in
        runs
        |> List.concat_map (fun (name, rows, _, _) ->
            List.map
              (fun (k, g, u) ->
                Printf.sprintf "%s,%d,%s,%s,%s\n" name k
                  (short_float (total g))
                  (floats u) (floats g))
              rows)
        |> String.concat ""
        |> ( ^ )
             (Printf.sprintf "algorithm,iteration,nash_conv,%s,%s\n"
                (players (Printf.sprintf "u%d"))
                (players (Printf.sprintf "gain%d")))
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        Plot.loglog
          ~title:(Printf.sprintf "%s: nash conv of the average strategy" G.name)
          ~xlabel:"iterations" ~ylabel:"nash conv"
          (List.map
             (fun (name, rows, _, _) ->
               {
                 Plot.label = name;
                 dashed = false;
                 points = List.map (fun (k, g, _) -> (k, total g)) rows;
               })
             runs)
        |> write_file path)
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

type sampler =
  | Tree of Cfr.algorithm
  | Sampled of Cfr.algorithm * Multi_mccfr.averaging

let samplers =
  [
    ("dcfr", Tree Cfr.dcfr);
    ("es", Sampled (Cfr.Vanilla, Multi_mccfr.Simple));
    ("es-weighted", Sampled (Cfr.Vanilla, Weighted));
    ("es-full", Sampled (Cfr.Vanilla, Full));
    ("es-linear", Sampled (Cfr.Linear, Simple));
  ]

let sample game chosen budget start seeds csv svg =
  let (module G : Multi.S) = List.assoc game games in
  let module C = Multi_cfr.Make (G) in
  let module M = Multi_mccfr.Make (G) in
  let marks = List.filter (fun k -> k >= start) (log_checkpoints 4 budget) in
  let trace advance touched tree average =
    let cpu = ref 0. in
    let rows =
      List.map
        (fun mark ->
          let t0 = Sys.time () in
          while touched () < mark do
            advance ()
          done;
          cpu := !cpu +. (Sys.time () -. t0);
          let s = average () in
          (mark, Multi_br.nash_conv tree s, Multi_br.values tree s))
        marks
    in
    (rows, !cpu)
  in
  let run seed = function
    | Tree algorithm ->
        let t = C.create ~algorithm () in
        trace
          (fun () -> C.iterate t)
          (fun () -> C.touched t)
          (C.tree t)
          (fun () -> C.average_strategy t)
    | Sampled (weighting, averaging) ->
        let t = M.create ~seed ~weighting ~averaging () in
        trace
          (fun () -> M.iterate t)
          (fun () -> M.touched t)
          (M.tree t)
          (fun () -> M.average_strategy t)
  in
  let runs =
    List.map
      (fun (name, sampler) ->
        let k = match sampler with Tree _ -> 1 | Sampled _ -> seeds in
        let traces = List.init k (fun i -> run (i + 1) sampler) in
        let mean f =
          List.fold_left (fun acc x -> acc +. f x) 0. traces /. float_of_int k
        in
        let rows =
          List.mapi
            (fun i mark ->
              ( mark,
                mean (fun (rows, _) ->
                    let _, nc, _ = List.nth rows i in
                    nc),
                List.map
                  (fun (rows, _) ->
                    let _, _, u = List.nth rows i in
                    u)
                  traces ))
            marks
        in
        (name, rows, mean snd))
      chosen
  in
  Format.printf "%s: %d players, mean of %d seed%s for the sampled runs@.@."
    G.name G.players seeds
    (if seeds = 1 then "" else "s");
  Format.printf "nash conv of the average strategy@.%14s" "nodes touched";
  List.iter (fun (name, _, _) -> Format.printf "%13s" name) runs;
  Format.printf "@.";
  List.iteri
    (fun i mark ->
      Format.printf "%14s" (sci (float_of_int mark));
      List.iter
        (fun (_, rows, _) ->
          let _, nc, _ = List.nth rows i in
          Format.printf "%13s" (sci nc))
        runs;
      Format.printf "@.")
    marks;
  Format.printf "@.%-12s %10s  %s@." "" "cpu s" "values (first seed)";
  List.iter
    (fun (name, rows, cpu) ->
      let _, _, us = List.nth rows (List.length rows - 1) in
      Format.printf "%-12s %10.2f  %s@." name cpu
        (String.concat " "
           (Array.to_list (Array.map (Printf.sprintf "%+.6f") (List.hd us)))))
    runs;
  try
    Option.iter
      (fun path ->
        runs
        |> List.concat_map (fun (name, rows, _) ->
            List.map
              (fun (mark, nc, us) ->
                Printf.sprintf "%s,%d,%s,%s\n" name mark (short_float nc)
                  (floats (List.hd us)))
              rows)
        |> String.concat ""
        |> ( ^ )
             (Printf.sprintf "run,touched,nash_conv,%s\n"
                (String.concat "," (List.init G.players (Printf.sprintf "u%d"))))
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        Plot.loglog
          ~title:
            (Printf.sprintf "%s: external sampling against full-tree DCFR"
               G.name)
          ~xlabel:"nodes touched" ~ylabel:"nash conv"
          (List.map
             (fun (name, rows, _) ->
               {
                 Plot.label = name;
                 dashed =
                   (match List.assoc name samplers with
                   | Tree _ -> true
                   | Sampled _ -> false);
                 points = List.map (fun (mark, nc, _) -> (mark, nc)) rows;
               })
             runs)
        |> write_file path)
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

let select game iterations budget seeds =
  let (module G : Multi.S) = List.assoc game games in
  let module C = Multi_cfr.Make (G) in
  let module M = Multi_mccfr.Make (G) in
  let full algorithm =
    let t = C.create ~algorithm () in
    C.run t iterations;
    (C.tree t, C.average_strategy t)
  in
  let tree, plus = full Cfr.Plus in
  let _, dcfr = full Cfr.dcfr in
  let sampled =
    List.init seeds (fun i ->
        let t = M.create ~seed:(i + 1) ~averaging:Weighted () in
        while M.touched t < budget do
          M.iterate t
        done;
        (Printf.sprintf "es%d" (i + 1), M.average_strategy t))
  in
  let mixed = Multi_profile.mix tree (List.map snd sampled) in
  let ends =
    [ ("cfr+", plus); ("dcfr", dcfr) ] @ sampled @ [ ("es-mix", mixed) ]
  in
  let vector f u = String.concat " " (Array.to_list (Array.map f u)) in
  Format.printf
    "%s: cfr+ and dcfr after %d iterations, %d external-sampling seeds \
     after      %s nodes each@.@."
    G.name iterations seeds
    (sci (float_of_int budget));
  Format.printf "%-8s %10s  %s@." "" "nash conv" "values";
  List.iter
    (fun (name, s) ->
      Format.printf "%-8s %10s  %s@." name
        (sci (Multi_br.nash_conv tree s))
        (vector (Printf.sprintf "%+.4f") (Multi_br.values tree s)))
    ends;
  Format.printf "@.reach-weighted distance, players %s@.%-8s"
    (String.concat "/" (List.init G.players string_of_int))
    "";
  List.iter (fun (name, _) -> Format.printf "%18s" name) ends;
  Format.printf "@.";
  List.iter
    (fun (name, a) ->
      Format.printf "%-8s" name;
      List.iter
        (fun (_, b) ->
          Format.printf "%18s"
            (vector (Printf.sprintf "%.3f") (Multi_profile.distance tree a b)))
        ends;
      Format.printf "@.")
    ends;
  Format.printf "@.nash conv of the mix of the first k seeds@.";
  List.iteri
    (fun k _ ->
      let first = List.filteri (fun i _ -> i <= k) sampled in
      let m = Multi_profile.mix tree (List.map snd first) in
      Format.printf "%3d %10s  %s@." (k + 1)
        (sci (Multi_br.nash_conv tree m))
        (vector (Printf.sprintf "%+.4f") (Multi_br.values tree m)))
    sampled

let cmds =
  let game =
    Arg.(
      value
      & opt (enum (List.map (fun (n, _) -> (n, n)) games)) "kuhn3"
      & info [ "g"; "game" ] ~docv:"GAME"
          ~doc:
            "kuhn3, kuhn4, leduc3 (8 cards), leduc3-6 (6 cards) or leduc4 \
             (10              cards).")
  in
  let chosen =
    Arg.(
      value
      & opt
          (list (enum (List.map (fun (n, a) -> (n, (n, a))) algorithms)))
          algorithms
      & info [ "a"; "algorithms" ] ~docv:"LIST"
          ~doc:"Any of cfr, cfr+, lcfr, dcfr.")
  in
  let iterations =
    Arg.(
      value & opt positive 1000
      & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Iterations.")
  in
  let file names doc =
    Arg.(value & opt (some string) None & info names ~docv:"FILE" ~doc)
  in
  let sampled =
    Arg.(
      value
      & opt
          (list (enum (List.map (fun (n, s) -> (n, (n, s))) samplers)))
          samplers
      & info [ "r"; "runs" ] ~docv:"LIST"
          ~doc:"Any of dcfr, es, es-weighted, es-full, es-linear.")
  in
  let budget =
    Arg.(
      value & opt positive 10_000_000
      & info [ "b"; "budget" ] ~docv:"N" ~doc:"Nodes touched per run.")
  in
  let start =
    Arg.(
      value & opt positive 10_000
      & info [ "start" ] ~docv:"N" ~doc:"First checkpoint, in nodes touched.")
  in
  let seeds =
    Arg.(
      value & opt positive 1
      & info [ "seeds" ] ~docv:"K" ~doc:"Seeds averaged for each sampled run.")
  in
  [
    Cmd.v
      (Cmd.info "multi-select"
         ~doc:
           "Solve one game with cfr+, dcfr and several \
            external-sampling             seeds, then compare the end points: \
            nash conv, values,             reach-weighted strategy distance \
            and the mix of the seeds.")
      Term.(
        const select $ game $ iterations $ budget
        $ Arg.(
            value & opt positive 5
            & info [ "seeds" ] ~docv:"K" ~doc:"External-sampling seeds."));
    Cmd.v
      (Cmd.info "multi-es"
         ~doc:
           "Compare external-sampling MCCFR, under three ways of \
            averaging             the strategy, with full-tree DCFR at equal \
            nodes touched.")
      Term.(
        term_result
          (const sample $ game $ sampled $ budget $ start $ seeds
          $ file [ "csv" ] "Write nash conv per checkpoint to $(docv)."
          $ file [ "svg" ] "Plot nash conv against nodes touched to $(docv)."));
    Cmd.v
      (Cmd.info "multi"
         ~doc:
           "Run full-tree CFR variants on a game with more than two players \
            and track each player's gain from deviating.")
      Term.(
        term_result
          (const solve $ game $ chosen $ iterations
          $ file [ "csv" ] "Write values and gains per checkpoint to $(docv)."
          $ file [ "svg" ] "Plot nash conv against iterations to $(docv)."));
  ]
