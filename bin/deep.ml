open Cfr_solver
open Report
open Cmdliner

let games : (string * (module Game.S) * Features.t) list =
  [
    ("kuhn", (module Kuhn), Features.kuhn);
    ("leduc", (module Leduc), Features.leduc Leduc.standard);
    ( "leduc5",
      (module (val Leduc.game { ranks = 5; suits = 2 }) : Game.S),
      Features.leduc { ranks = 5; suits = 2 } );
  ]

type row = {
  iteration : int;
  traversals : int;
  current : float;
  sd : float;
  exact : float;
  net : float;
  tabular : float;
  seconds : float;
}

let run game iterations traversals hidden steps batch lr capacity exact
    average_steps domains seed csv svg =
  let _, (module G : Game.S), features =
    List.find (fun (n, _, _) -> n = game) games
  in
  let module B = Best_response.Make (G) in
  let module M = Mccfr.Make (G) in
  let tree = Tree.build (module G) in
  let learner =
    if exact then Deep_cfr.Exact
    else Deep_cfr.Network { hidden; steps; batch; lr }
  in
  let config =
    { Deep_cfr.traversals; capacity; learner; average_steps; domains; seed }
  in
  let d = Deep_cfr.create ~config features tree in
  let twin =
    Deep_cfr.create ~config:{ config with learner = Exact } features tree
  in
  let tab = M.create ~seed ~weighting:Cfr.Linear Mccfr.External in
  let elapsed = ref 0. in
  Format.printf "%9s %11s %11s %11s %11s %11s %11s %8s@." "iteration"
    "traversals" "current" "sd-cfr" "net" "exact" "tabular" "seconds";
  let rows =
    List.map
      (fun k ->
        let start = Unix.gettimeofday () in
        Deep_cfr.run d (k - Deep_cfr.iterations d);
        elapsed := !elapsed +. (Unix.gettimeofday () -. start);
        let seconds = !elapsed in
        Deep_cfr.run twin (k - Deep_cfr.iterations twin);
        M.run tab ((k * traversals) - M.iterations tab);
        let net = B.exploitability (Deep_cfr.average_network d) in
        let row =
          {
            iteration = k;
            traversals = k * traversals;
            current = B.exploitability (Deep_cfr.current d);
            sd = B.exploitability (Deep_cfr.average_exact d);
            exact = B.exploitability (Deep_cfr.average_exact twin);
            net;
            tabular = B.exploitability (M.average_strategy tab);
            seconds;
          }
        in
        Format.printf "%9d %11d %11s %11s %11s %11s %11s %8.1f@." row.iteration
          row.traversals (sci row.current) (sci row.sd) (sci row.net)
          (sci row.exact) (sci row.tabular) row.seconds;
        row)
      (checkpoints iterations)
  in
  Format.printf "@.%s, %s, %d traversals per player per iteration@." G.name
    (Deep_cfr.learner_name learner)
    traversals;
  Format.printf "%d samples, %d nodes touched@." (Deep_cfr.samples d)
    (Deep_cfr.touched d);
  try
    Option.iter
      (fun path ->
        rows
        |> List.map (fun r ->
            Printf.sprintf "%d,%d,%s,%s,%s,%s,%s,%.2f\n" r.iteration
              r.traversals (short_float r.current) (short_float r.sd)
              (short_float r.net) (short_float r.exact) (short_float r.tabular)
              r.seconds)
        |> String.concat ""
        |> ( ^ )
             "iteration,traversals,current,sd_cfr,strategy_net,exact_memories,tabular_es,seconds\n"
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        let series label dashed pick =
          {
            Plot.label;
            dashed;
            points = List.map (fun r -> (r.traversals, pick r)) rows;
          }
        in
        Plot.loglog
          ~title:
            (Printf.sprintf "%s: Deep CFR (%s)"
               (String.capitalize_ascii G.name)
               (Deep_cfr.learner_name learner))
          ~xlabel:"traversals per player" ~ylabel:"exploitability"
          [
            series "strategy net" false (fun r -> r.net);
            series "sd-cfr" false (fun r -> r.sd);
            series "exact memories" false (fun r -> r.exact);
            series "current" true (fun r -> r.current);
            series "tabular es" true (fun r -> r.tabular);
          ]
        |> write_file path)
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

let cmd =
  let names = List.map (fun (n, _, _) -> n) games in
  let game =
    Arg.(
      value
      & opt (enum (List.map (fun n -> (n, n)) names)) "leduc"
      & info [ "g"; "game" ] ~docv:"GAME"
          ~doc:(Printf.sprintf "Game: %s." (String.concat ", " names)))
  and iterations =
    Arg.(
      value & opt positive 100
      & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Deep CFR iterations.")
  and traversals =
    Arg.(
      value & opt positive 1000
      & info [ "k"; "traversals" ] ~docv:"K"
          ~doc:"Traversals per player per iteration.")
  and hidden =
    Arg.(
      value
      & opt (list int) [ 64; 64 ]
      & info [ "hidden" ] ~docv:"SIZES" ~doc:"Hidden layer widths.")
  and steps =
    Arg.(
      value & opt positive 2000
      & info [ "steps" ] ~docv:"N" ~doc:"SGD steps per advantage network.")
  and batch =
    Arg.(
      value & opt positive 256 & info [ "batch" ] ~docv:"N" ~doc:"Batch size.")
  and lr =
    Arg.(
      value & opt float 3e-3
      & info [ "lr" ] ~docv:"RATE" ~doc:"Adam learning rate.")
  and capacity =
    Arg.(
      value & opt positive 1_000_000
      & info [ "capacity" ] ~docv:"N" ~doc:"Reservoir size of each memory.")
  and exact =
    Arg.(
      value & flag
      & info [ "exact" ]
          ~doc:
            "Replace the networks by exact weighted means over the same \
             memories.")
  and average_steps =
    Arg.(
      value & opt positive 4000
      & info [ "average-steps" ] ~docv:"N"
          ~doc:"SGD steps for the strategy network.")
  and domains =
    Arg.(
      value & opt positive 1
      & info [ "domains" ] ~docv:"N" ~doc:"Domains sharing each training batch.")
  and seed =
    Arg.(value & opt int 1 & info [ "seed" ] ~docv:"SEED" ~doc:"Random seed.")
  and csv =
    Arg.(
      value
      & opt (some string) None
      & info [ "csv" ] ~docv:"FILE" ~doc:"Write the curves to $(docv).")
  and svg =
    Arg.(
      value
      & opt (some string) None
      & info [ "svg" ] ~docv:"FILE" ~doc:"Plot the curves to $(docv).")
  in
  Cmd.v
    (Cmd.info "deep"
       ~doc:
         "Deep CFR (Brown et al. 2019) with its strategy network, the exact \
          SD-CFR average of the advantage networks (Steinberger 2019), and \
          tabular external sampling on the same traversal budget.")
    Term.(
      term_result
        (const run $ game $ iterations $ traversals $ hidden $ steps $ batch
       $ lr $ capacity $ exact $ average_steps $ domains $ seed $ csv $ svg))

let cmds = [ cmd ]
