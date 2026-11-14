open Cfr_solver
open Cmdliner
module F = Figgie
module B = Figgie_bots

let parse_lineup s =
  let names = String.split_on_char ',' s |> List.map String.trim in
  let bots = List.filter_map B.find names in
  if List.length bots <> List.length names then
    Error
      (`Msg
         (Printf.sprintf "unknown bot in %S; known: %s" s
            (String.concat ", " (List.map (fun (b : F.bot) -> b.name) B.all))))
  else if List.length bots < 4 || List.length bots > 5 then
    Error (`Msg "figgie takes 4 or 5 players")
  else Ok (Array.of_list bots)

let tournament lineup games ticks seed =
  Result.map
    (fun bots ->
      let start = Unix.gettimeofday () in
      let r = B.tournament (Rng.make seed) ~games ~ticks bots in
      Format.printf "%d games of %d ticks, seats shuffled, %.2fs@." games ticks
        (Unix.gettimeofday () -. start);
      Format.printf "%-16s %10s %8s@." "bot" "mean pnl" "stderr";
      Array.iteri
        (fun i (s : B.stats) ->
          Format.printf "%-16s %+10.2f %8.2f@." bots.(i).F.name s.mean s.stderr)
        r)
    (parse_lineup lineup)

let parse_hand s =
  let h = Array.make 4 0 in
  let rec go i =
    if i >= String.length s then Ok h
    else
      match
        Array.find_index
          (fun x -> F.letter x = Char.uppercase_ascii s.[i])
          F.suits
      with
      | None -> Error (`Msg (Printf.sprintf "bad suit %c in %S" s.[i] s))
      | Some k ->
          let j = ref (i + 1) in
          while !j < String.length s && s.[!j] >= '0' && s.[!j] <= '9' do
            incr j
          done;
          if !j = i + 1 then
            Error (`Msg (Printf.sprintf "no count after %c" s.[i]))
          else begin
            h.(k) <- h.(k) + int_of_string (String.sub s (i + 1) (!j - i - 1));
            go !j
          end
  in
  go 0

let hand s players =
  Result.bind (parse_hand s) (fun h ->
      let size = Array.fold_left ( + ) 0 h in
      if players < 4 || players > 5 then
        Error (`Msg "figgie takes 4 or 5 players")
      else if size <> 40 / players then
        Error
          (`Msg
             (Printf.sprintf "a %d-player hand has %d cards" players
                (40 / players)))
      else if Array.exists (fun x -> x > 12) h then
        Error (`Msg "at most 12 of a suit")
      else
        let post = F.posterior h in
        let g = F.goal_probs post in
        let others = Array.make (players - 1) (40 / players) in
        Format.printf "expected payout %.2f (ante %d)@.@."
          (F.expected_payout post h others)
          (F.ante players);
        Format.printf "%-6s %6s %6s %8s %8s@." "suit" "held" "goal" "buy" "sell";
        Array.iteri
          (fun i s ->
            Format.printf "%-6s %6d %6.3f %8.2f %8s@."
              (String.make 1 (F.letter s))
              h.(i) g.(i)
              (F.buy_value post h others s)
              (if h.(i) > 0 then
                 Printf.sprintf "%.2f" (F.sell_value post h others s)
               else "-"))
          F.suits;
        Ok ())

let mean_stderr xs =
  let n = float_of_int (Array.length xs) in
  let m = Array.fold_left ( +. ) 0. xs /. n in
  let v =
    Array.fold_left (fun a x -> a +. ((x -. m) ** 2.)) 0. xs /. (n -. 1.)
  in
  (m, sqrt (v /. n))

let uniform _ n = Array.make n (1. /. float_of_int n)

let solve_figgie players rounds iterations br playouts games ticks seed top =
  if players < 4 || players > 5 then Error (`Msg "figgie takes 4 or 5 players")
  else if rounds < 1 then Error (`Msg "rounds must be positive")
  else
    let module A = Figgie_abs.Make (struct
      let players = players
      let rounds = rounds
    end) in
    let module Z = Lazy_mccfr.Make (A) in
    let z = Z.create ~seed () in
    let start = Unix.gettimeofday () in
    Format.printf "%s: external-sampling MCCFR, %d iterations@.@." A.name
      iterations;
    Format.printf "%10s %9s %12s %9s@." "iteration" "infosets" "nodes" "seconds";
    List.iter
      (fun k ->
        Z.run z (k - Z.iterations z);
        Format.printf "%10d %9d %12d %9.1f@." k (Z.infosets z) (Z.touched z)
          (Unix.gettimeofday () -. start))
      (Report.checkpoints iterations);
    let blueprint = Z.average z in
    let score learner field k =
      (Z.playout
         (Rng.make ((1_000_000 * seed) + k))
         (fun p -> if p = 0 then learner else field)).(0)
    in
    Format.printf
      "@.seat 0 against %d copies of a fixed profile, %d paired deals@."
      (players - 1) playouts;
    Format.printf "%-10s %16s %16s %16s@." "profile" "itself" "best response"
      "gain";
    List.iter
      (fun (name, field) ->
        let r =
          Z.create ~seed:(seed + 1)
            ~fixed:(fun p -> if p = 0 then None else Some field)
            ()
        in
        Z.run r br;
        let u0 = Array.init playouts (score field field)
        and u1 = Array.init playouts (score (Z.average r) field) in
        let m0, s0 = mean_stderr u0
        and m1, s1 = mean_stderr u1
        and g, sg = mean_stderr (Array.map2 ( -. ) u1 u0) in
        Format.printf "%-10s %+9.2f +- %4.2f %+9.2f +- %4.2f %+9.2f +- %4.2f@."
          name m0 s0 m1 s1 g sg)
      [ ("uniform", uniform); ("mccfr", blueprint) ];
    let field =
      [| B.fundamentalist (); B.counter (); B.bottom_feeder; B.noise |]
    in
    let fill =
      Array.init (players - 1) (fun i -> field.(i mod Array.length field))
    in
    Format.printf "@.one seat in a field of %s, %d games of %d ticks@."
      (String.concat ", "
         (Array.to_list (Array.map (fun (b : F.bot) -> b.name) fill)))
      games ticks;
    Format.printf "%-16s %10s %8s@." "bot" "mean pnl" "stderr";
    List.iter
      (fun (b : F.bot) ->
        let r =
          B.tournament (Rng.make seed) ~games ~ticks (Array.append [| b |] fill)
        in
        Format.printf "%-16s %+10.2f %8.2f@." b.name r.(0).mean r.(0).stderr)
      [
        Figgie_abs.bot ~name:"mccfr" ~rounds blueprint;
        Figgie_abs.bot ~name:"abstract-uniform" ~rounds uniform;
        B.fundamentalist ();
        B.counter ();
        B.passive;
      ];
    let keys =
      Strategy.bindings (Z.average_strategy z)
      |> List.map (fun (key, e) -> (Z.mass z key, key, e))
      |> List.sort (fun (a, _, _) (b, _, _) -> compare b a)
      |> List.filteri (fun i _ -> i < top)
    in
    Format.printf
      "@.most visited infosets (round, confidence, held, bid, offer, junk)@.";
    List.iter
      (fun (_, key, (e : Strategy.entry)) ->
        Format.printf "%s  %s@." key
          (String.concat " "
             (Array.to_list
                (Array.mapi
                   (fun a l -> Printf.sprintf "%s %.2f" l e.probs.(a))
                   e.actions))))
      keys;
    Ok ()

let figgie_cmd =
  let lineup =
    Arg.(
      value
      & opt string "fundamentalist,counter,bottom-feeder,noise"
      & info [ "lineup" ] ~docv:"BOTS"
          ~doc:
            "Comma-separated bots, 4 or 5: passive, noise, \
             fundamentalist,              counter, bottom-feeder. \
             fundamentalist:E and counter:E quote E              dollars from \
             their value (default 2).")
  and games = Arg.(value & opt int 2000 & info [ "games" ] ~docv:"N")
  and ticks =
    Arg.(
      value & opt int 200
      & info [ "ticks" ] ~docv:"T"
          ~doc:"Each player acts once per tick, in a random order.")
  and seed = Arg.(value & opt int 1 & info [ "seed" ] ~docv:"S") in
  Cmd.v
    (Cmd.info "figgie"
       ~doc:"Play Figgie games between bots and report each bot's P&L.")
    Term.(term_result (const tournament $ lineup $ games $ ticks $ seed))

let hand_cmd =
  let h =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"HAND" ~doc:"Counts per suit, e.g. S5C2H2D1.")
  and players = Arg.(value & opt int 4 & info [ "players" ] ~docv:"P") in
  Cmd.v
    (Cmd.info "figgie-hand"
       ~doc:
         "Exact posterior over the goal suit for a dealt hand, and what one \
          more or one less card of each suit is worth.")
    Term.(term_result (const hand $ h $ players))

let edge_sweep edges games ticks seed svg =
  let field = [ B.fundamentalist (); B.bottom_feeder; B.noise ] in
  Format.printf "%5s %18s %18s %18s %18s@." "edge" "counter" "fundamentalist"
    "bottom-feeder" "noise";
  let rows =
    List.init (edges + 1) (fun e ->
        let bots =
          Array.of_list (B.counter ~edge:(float_of_int e) () :: field)
        in
        let r = B.tournament (Rng.make seed) ~games ~ticks bots in
        Format.printf "%5d" e;
        Array.iter
          (fun (s : B.stats) ->
            Format.printf " %+10.2f +- %5.2f" s.mean s.stderr)
          r;
        Format.printf "@.";
        (float_of_int e, Array.map (fun (s : B.stats) -> s.mean) r))
  in
  Option.iter
    (fun f ->
      let curve i color name =
        {
          Plot.name;
          color;
          dash = false;
          xy = List.map (fun (e, m) -> (e, m.(i))) rows;
        }
      in
      Out_channel.with_open_text f (fun oc ->
          output_string oc
            (Plot.lines
               ~title:"Figgie: the counter's quote edge against a fixed field"
               ~xlabel:"counter's edge (dollars from its value)"
               ~ylabel:"mean profit per game (dollars)"
               ~note:
                 (Printf.sprintf "%d games per point, %d ticks, seats shuffled"
                    games ticks)
               [
                 curve 0 0 "counter";
                 curve 1 1 "fundamentalist";
                 curve 2 2 "bottom-feeder";
               ])))
    svg

let edge_cmd =
  let edges =
    Arg.(
      value & opt int 8
      & info [ "edges" ] ~docv:"E" ~doc:"Sweep edges 0 to $(docv).")
  and games = Arg.(value & opt int 2000 & info [ "games" ] ~docv:"N")
  and ticks = Arg.(value & opt int 200 & info [ "ticks" ] ~docv:"T")
  and seed = Arg.(value & opt int 1 & info [ "seed" ] ~docv:"S")
  and svg =
    Arg.(value & opt (some string) None & info [ "svg" ] ~docv:"FILE")
  in
  Cmd.v
    (Cmd.info "figgie-edge"
       ~doc:
         "How far from its value the card-counting bot should quote, against a \
          fundamentalist, a bottom-feeder and a noise trader.")
    Term.(const edge_sweep $ edges $ games $ ticks $ seed $ svg)

let solve_cmd =
  let opt_int names v doc = Arg.(value & opt int v & info names ~doc) in
  Cmd.v
    (Cmd.info "figgie-cfr"
       ~doc:
         "Train external-sampling MCCFR on abstracted Figgie, measure how much \
          a best response gains against it, and play it against the bots.")
    Term.(
      term_result
        (const solve_figgie
        $ opt_int [ "players" ] 4 "Players, 4 or 5."
        $ opt_int [ "rounds" ] 3
            "Rounds in the training game; every player acts once per round."
        $ opt_int [ "n"; "iterations" ] 2000 "MCCFR iterations."
        $ opt_int [ "br" ] 2000 "Iterations of the best-response learner."
        $ opt_int [ "playouts" ] 20000 "Playouts per best-response score."
        $ opt_int [ "games" ] 2000 "Games per tournament."
        $ opt_int [ "ticks" ] 3 "Ticks per tournament game."
        $ opt_int [ "seed" ] 1 "Seed."
        $ opt_int [ "top" ] 12 "Infosets to print."))

let cmds = [ figgie_cmd; hand_cmd; edge_cmd; solve_cmd ]
