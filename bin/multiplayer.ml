open Cfr_solver
open Report
open Cmdliner

let games : (string * (module Multi.S)) list =
  [
    ("kuhn3", (module (val Kuhn_n.game 3)));
    ("kuhn4", (module (val Kuhn_n.game 4)));
    ("leduc3", (module (val Leduc_n.game 3)));
    ("leduc3-6", (module (val Leduc_n.game ~ranks:3 3)));
  ]

let algorithms =
  [ ("cfr", Cfr.Vanilla); ("cfr+", Plus); ("lcfr", Linear); ("dcfr", Cfr.dcfr) ]

let floats u = String.concat "," (Array.to_list (Array.map short_float u))

let solve game chosen iterations csv svg =
  let (module G : Multi.S) = List.assoc game games in
  let module C = Multi_cfr.Make (G) in
  let module B = Multi_br.Make (G) in
  let module E = Multi.Eval (G) in
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
              (k, B.gains s, E.values s))
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

let cmds =
  let game =
    Arg.(
      value
      & opt (enum (List.map (fun (n, _) -> (n, n)) games)) "kuhn3"
      & info [ "g"; "game" ] ~docv:"GAME"
          ~doc:"kuhn3, kuhn4, leduc3 (8 cards) or leduc3-6 (6 cards).")
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
  [
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
