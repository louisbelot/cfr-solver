open Cfr_solver
open Cmdliner
module S = Sequence_form

let config cards hidden width rounds = { Market.cards; hidden; width; rounds }

let equilibrium c iterations =
  let g = Market.game c in
  let (module G) = g in
  if iterations = 0 then
    let sol = S.solve (S.build g) in
    (Rat.to_float sol.value, 0., sol.strategy)
  else
    let module C = Cfr.Make (G) in
    let module B = Best_response.Make (G) in
    let t = C.create ~algorithm:Cfr.dcfr () in
    C.run t iterations;
    let avg = C.average_strategy t in
    ((B.value P0 avg -. B.value P1 avg) /. 2., B.nash_conv avg, avg)

let against c strat =
  let (module G) = Market.game c in
  let module B = Best_response.Make (G) in
  -.B.value P1 strat

let quotes c strat =
  let lo, hi = Market.mids c in
  for own = 1 to c.Market.cards do
    let probs =
      Strategy.probs strat (Printf.sprintf "%d:" own) ~n:(hi - lo + 1)
    in
    let mix =
      List.init (hi - lo + 1) Fun.id
      |> List.filter (fun k -> probs.(k) >= 0.005)
      |> List.map (fun k -> Printf.sprintf "%d:%.2f" (lo + k) probs.(k))
    in
    Format.printf "  card %2d  fair %5.2f  quotes %s@." own (Market.fair c own)
      (String.concat " " mix)
  done

let solve cards hidden width rounds iterations =
  let c = config cards hidden width rounds in
  let start = Unix.gettimeofday () in
  let v, nc, strat = equilibrium c iterations in
  let secs = Unix.gettimeofday () -. start in
  Format.printf "%s, %s in %.2fs@." (Market.name c)
    (if iterations = 0 then "exact LP"
     else Printf.sprintf "%d iterations of dcfr" iterations)
    secs;
  Format.printf "maker value     %+.6f per hand%s@." v
    (if iterations = 0 then "" else Printf.sprintf " (nash conv %.2e)" nc);
  if rounds = 1 then begin
    Format.printf "revealing quote %+.6f@." (against c (Market.revealing c));
    Format.printf "uninformed      %+.6f@." (against c (Market.uninformed c))
  end;
  Format.printf "@.maker's first quote:@.";
  quotes c strat

let width_sweep cards hidden widths iterations csv svg =
  let rows = Buffer.create 1024 in
  Buffer.add_string rows "width,equilibrium,revealing,uninformed\n";
  Format.printf "%5s %12s %12s %12s@." "width" "equilibrium" "revealing"
    "uninformed";
  let pts =
    List.map
      (fun w ->
        let c = config cards hidden w 1 in
        let eq, _, _ = equilibrium c iterations in
        let r = against c (Market.revealing c)
        and u = against c (Market.uninformed c) in
        Format.printf "%5d %12.4f %12.4f %12.4f@." w eq r u;
        Printf.bprintf rows "%d,%.6f,%.6f,%.6f\n" w eq r u;
        (float_of_int w, -.eq, -.r, -.u))
      (List.init (widths + 1) Fun.id)
  in
  Option.iter
    (fun f ->
      Out_channel.with_open_text f (fun oc -> Buffer.output_buffer oc rows))
    csv;
  let curve color name pick =
    { Plot.name; color; dash = false; xy = List.map pick pts }
  in
  Option.iter
    (fun f ->
      Out_channel.with_open_text f (fun oc ->
          output_string oc
            (Plot.lines
               ~title:
                 (Printf.sprintf
                    "Market on the sum of %d cards from 1..%d, each player \
                     sees one"
                    (2 + hidden) cards)
               ~xlabel:"quoted width" ~ylabel:"maker's expected loss per hand"
               ~note:
                 (if iterations = 0 then "equilibrium from the exact LP"
                  else
                    Printf.sprintf "equilibrium from %d iterations of DCFR"
                      iterations)
               [
                 curve 1 "uninformed" (fun (w, _, _, u) -> (w, u));
                 curve 0 "revealing" (fun (w, _, r, _) -> (w, r));
                 curve 2 "equilibrium" (fun (w, e, _, _) -> (w, e));
               ])))
    svg

let cards =
  Arg.(
    value & opt int 6
    & info [ "cards" ] ~docv:"K" ~doc:"Deck of cards 1 to $(docv).")

let hidden =
  Arg.(
    value & opt int 0
    & info [ "hidden" ] ~docv:"H"
        ~doc:"Extra cards nobody sees that also count in the sum.")

let iterations =
  Arg.(
    value & opt int 0
    & info [ "n"; "iterations" ] ~docv:"N"
        ~doc:"DCFR iterations; 0 solves the sequence-form LP exactly.")

let file name doc =
  Arg.(value & opt (some string) None & info [ name ] ~docv:"FILE" ~doc)

let solve_cmd =
  let width =
    Arg.(value & opt int 2 & info [ "width" ] ~docv:"W" ~doc:"Ask minus bid.")
  and rounds =
    Arg.(
      value & opt int 1
      & info [ "rounds" ] ~docv:"R"
          ~doc:"1, or 2 to have the taker quote back after the first trade.")
  in
  Cmd.v
    (Cmd.info "market"
       ~doc:
         "Solve the interview market-making game: each player holds a card, \
          one quotes a fixed-width market on the sum, the other buys, sells or \
          passes.")
    Term.(const solve $ cards $ hidden $ width $ rounds $ iterations)

let width_cmd =
  let widths =
    Arg.(
      value & opt int 8
      & info [ "widths" ] ~docv:"W" ~doc:"Sweep widths 0 to $(docv).")
  in
  Cmd.v
    (Cmd.info "market-width"
       ~doc:"The maker's loss against quoted width, for three ways of quoting.")
    Term.(
      const width_sweep $ cards $ hidden $ widths $ iterations
      $ file "csv" "Write the sweep to $(docv)."
      $ file "svg" "Write the chart to $(docv).")

let cmds = [ solve_cmd; width_cmd ]
