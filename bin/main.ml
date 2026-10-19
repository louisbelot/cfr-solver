open Cfr_solver

let games : (string * (module Game.S)) list = [ ("kuhn", (module Kuhn)) ]

(* 1, 2, 5, 10, 20, 50, ... below n, then n: evenly spaced on a log axis. *)
let checkpoints n =
  let rec go p acc =
    if p >= n then acc
    else go (p * 10) (List.filter (fun k -> k < n) [ 5 * p; 2 * p; p ] @ acc)
  in
  List.rev (n :: go 1 [])

let write_file path contents =
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc contents)

let solve game iterations json trace =
  let (module G : Game.S) = List.assoc game games in
  let module C = Cfr.Make (G) in
  let module B = Best_response.Make (G) in
  let module E = Eval.Make (G) in
  let t = C.create () in
  let cpu = ref 0. in
  let run_to k =
    let start = Sys.time () in
    C.run t (k - C.iterations t);
    cpu := !cpu +. (Sys.time () -. start)
  in
  let rows =
    match trace with
    | None ->
        run_to iterations;
        []
    | Some _ ->
        List.map
          (fun k ->
            run_to k;
            (k, B.exploitability (C.average_strategy t)))
          (checkpoints iterations)
  in
  let sigma = C.average_strategy t in
  let value = E.expected_value sigma in
  let expl = B.exploitability sigma in
  let players = [ Game.P0; P1 ] in
  let keys p = List.map fst (E.infosets p) in
  Format.printf "%s: %d iterations of CFR in %.2f s@." G.name iterations !cpu;
  Format.printf "game value      %.6f (player 0)@." value;
  Format.printf "exploitability  %.3e@." expl;
  List.iter
    (fun p ->
      Format.printf "@.%s@.%a@?" (Game.player_to_string p)
        (Strategy.pp ~keys:(keys p))
        sigma)
    players;
  try
    Option.iter
      (fun path ->
        Json.Object
          [
            ("game", String G.name);
            ("algorithm", String "cfr");
            ("iterations", Int iterations);
            ("game_value", Float value);
            ("exploitability", Float expl);
            ( "players",
              List
                (List.map
                   (fun p -> Strategy.to_json ~keys:(keys p) sigma)
                   players) );
          ]
        |> Json.to_string
        |> fun s -> write_file path (s ^ "\n"))
      json;
    Option.iter
      (fun path ->
        rows
        |> List.map (fun (k, e) -> Printf.sprintf "%d,%.6g\n" k e)
        |> String.concat ""
        |> ( ^ ) "iteration,exploitability\n"
        |> write_file path)
      trace;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

open Cmdliner

let game =
  let names = List.map fst games in
  Arg.(
    value
    & opt (enum (List.map (fun n -> (n, n)) names)) "kuhn"
    & info [ "g"; "game" ] ~docv:"GAME"
        ~doc:(Printf.sprintf "Game to solve: %s." (String.concat ", " names)))

let iterations =
  let positive =
    let parse s =
      match int_of_string_opt s with
      | Some n when n > 0 -> Ok n
      | _ -> Error (`Msg "expected a positive integer")
    in
    Arg.conv (parse, Format.pp_print_int)
  in
  Arg.(
    value & opt positive 10_000
    & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Number of CFR iterations.")

let json =
  Arg.(
    value
    & opt (some string) None
    & info [ "json" ] ~docv:"FILE" ~doc:"Write the average strategy to $(docv).")

let trace =
  Arg.(
    value
    & opt (some string) None
    & info [ "trace" ] ~docv:"FILE"
        ~doc:
          "Write exploitability at 1, 2, 5, 10, 20, ... iterations to $(docv) \
           as CSV.")

let solve_cmd =
  Cmd.v
    (Cmd.info "solve" ~doc:"Run CFR and print the average strategy.")
    Term.(term_result (const solve $ game $ iterations $ json $ trace))

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "cfr-solver" ~version:"0.1.0"
             ~doc:
               "Equilibrium strategies by counterfactual regret minimization.")
          [ solve_cmd ]))
