open Cfr_solver
open Report
open Cmdliner

let games : (string * (module Game.S) * Lbr.rollout list) list =
  let poker = Lbr.[ Own; Uniform; Prefer "c" ]
  and other = Lbr.[ Own; Uniform ] in
  [
    ("kuhn", (module Kuhn), other);
    ("leduc", (module Leduc), poker);
    ( "liars",
      Liars_dice.game Liars_dice.standard,
      Lbr.[ Own; Uniform; Prefer "liar" ] );
    ("goofspiel", Goofspiel.game { Goofspiel.standard with cards = 5 }, other);
    ( "goofspiel-random",
      Goofspiel.game { Goofspiel.standard with order = Random },
      other );
  ]

let timed f =
  let start = Unix.gettimeofday () in
  let r = f () in
  (r, 1000. *. (Unix.gettimeofday () -. start))

let run game iterations csv svg =
  let _, (module G : Game.S), rollouts =
    List.find (fun (n, _, _) -> n = game) games
  in
  let module M = Multi.Of_game (G) in
  let module C = Cfr.Make (G) in
  let tree = Multi_tree.build (module M) in
  let c = C.create ~algorithm:Cfr.Plus () in
  let rows =
    List.map
      (fun k ->
        C.run c (k - C.iterations c);
        let s = C.average_strategy c in
        let exact, t_br = timed (fun () -> Multi_br.nash_conv tree s) in
        let local =
          List.map
            (fun rollout -> timed (fun () -> Lbr.nash_conv ~rollout tree s))
            rollouts
        in
        (k, (exact, t_br), local))
      (checkpoints iterations)
  in
  Format.printf
    "%s: NashConv of the CFR+ average, exact and by local best response@.@."
    G.name;
  Format.printf "%9s %11s" "iteration" "exact";
  List.iter
    (fun r -> Format.printf " %14s" ("lbr " ^ Lbr.rollout_name r))
    rollouts;
  Format.printf "@.";
  List.iter
    (fun (k, (e, _), local) ->
      Format.printf "%9d %11s" k (sci e);
      List.iter (fun (v, _) -> Format.printf " %14s" (sci v)) local;
      Format.printf "@.")
    rows;
  let n = float (List.length rows) in
  let mean f = List.fold_left (fun acc r -> acc +. f r) 0. rows /. n in
  Format.printf "@.ms per call: exact %.1f" (mean (fun (_, (_, t), _) -> t));
  List.iteri
    (fun i r ->
      Format.printf ", %s %.1f" (Lbr.rollout_name r)
        (mean (fun (_, _, l) -> snd (List.nth l i))))
    rollouts;
  Format.printf "@.";
  try
    Option.iter
      (fun path ->
        rows
        |> List.concat_map (fun (k, (e, _), local) ->
            Printf.sprintf "exact,%d,%s\n" k (short_float e)
            :: List.map2
                 (fun r (v, _) ->
                   Printf.sprintf "%s,%d,%s\n" (Lbr.rollout_name r) k
                     (short_float v))
                 rollouts local)
        |> String.concat ""
        |> ( ^ ) "series,iteration,nash_conv\n"
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        let series label dashed pick =
          {
            Plot.label;
            dashed;
            points =
              List.filter_map
                (fun ((k, _, _) as r) ->
                  let v = pick r in
                  if v > 0. then Some (k, v) else None)
                rows;
          }
        in
        Plot.loglog
          ~title:
            (Printf.sprintf "%s: exact and local best response"
               (String.capitalize_ascii G.name))
          ~xlabel:"CFR+ iterations" ~ylabel:"NashConv"
          [
            series "exact" false (fun (_, (e, _), _) -> e);
            series "lbr own" true (fun (_, _, l) -> fst (List.hd l));
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
      value & opt positive 1000
      & info [ "n"; "iterations" ] ~docv:"N" ~doc:"CFR+ iterations.")
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
    (Cmd.info "lbr"
       ~doc:
         "Compare local best response (Lisy and Bowling 2017) with the exact \
          best response along a CFR+ run.")
    Term.(term_result (const run $ game $ iterations $ csv $ svg))

let cmds = [ cmd ]
