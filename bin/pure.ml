open Cfr_solver
open Cmdliner

let orders =
  [
    ("random", Goofspiel.Random);
    ("descending", Descending);
    ("ascending", Ascending);
  ]

let timed f =
  let start = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. start)

let upcards n = function
  | Goofspiel.Random -> List.init n (fun i -> i + 1)
  | Descending -> [ n ]
  | Ascending -> [ 1 ]

let table n columns =
  Format.printf "%-6s" "bid";
  List.iter
    (fun (k, _) -> Format.printf "%8s" (Printf.sprintf "up %d" k))
    columns;
  Format.printf "@.";
  for b = 1 to n do
    Format.printf "%-6d" b;
    List.iter (fun (_, x) -> Format.printf "%8.4f" x.(b - 1)) columns;
    Format.printf "@."
  done

let digits b = String.length (Bigint.to_string (Bigint.abs b))

let win_loss n order =
  let c = { Goofspiel.cards = n; order; payoff = Points; reveal = true } in
  let s, secs = timed (fun () -> Gops.strategy c) in
  Format.printf "@.points-optimal profile in the open game (%.2fs to export)@."
    secs;
  List.iter
    (fun payoff ->
      let (module G : Game.S) = Goofspiel.game { c with payoff } in
      let module B = Best_response.Make (G) in
      Format.printf "  %-22s best response gains %.6f + %.6f@." G.name
        (B.value P0 s) (B.value P1 s))
    [ Goofspiel.Points; Win_loss ]

let first_move create turn size n order =
  let all = (1 lsl n) - 1 in
  timed (fun () ->
      let t = create order in
      let cols =
        List.map
          (fun k ->
            let x, _, v = turn t all all all k in
            (k, x, v))
          (upcards n order)
      in
      (cols, size t))

let solve n order exact loss =
  let (cols, size), secs =
    first_move Gops.Float.create Gops.Float.turn Gops.Float.size n order
  in
  let value =
    List.fold_left (fun acc (_, _, v) -> acc +. v) 0. cols
    /. float (List.length cols)
  in
  Format.printf "GOPS %d, %s upcards: %d subgames stored, %.2fs, value %.3g@.@."
    n
    (fst (List.find (fun (_, o) -> o = order) orders))
    size secs value;
  table n (List.map (fun (k, x, _) -> (k, x)) cols);
  if exact then begin
    let (ecols, _), secs =
      first_move Gops.Exact.create Gops.Exact.turn Gops.Exact.size n order
    in
    let gap =
      List.fold_left2
        (fun m (_, q, _) (_, f, _) ->
          Array.fold_left Float.max m
            (Array.mapi (fun i q -> Float.abs (Rat.to_float q -. f.(i))) q))
        0. ecols cols
    in
    Format.printf "@.exact: %.2fs, largest gap to the float table %.2e@." secs
      gap;
    let k, x, _ = List.hd ecols in
    Array.iteri
      (fun i (q : Rat.t) ->
        Format.printf "  upcard %d, bid %d: %.10f, %d / %d digits@." k (i + 1)
          (Rat.to_float q) (digits q.num) (digits q.den))
      x
  end;
  if loss then win_loss n order

let gops n order exact loss =
  if n > 13 then Error (`Msg "at most 13 cards")
  else if loss && n > 5 then Error (`Msg "--win-loss needs at most 5 cards")
  else Ok (solve n order exact loss)

let cmd =
  let n =
    Arg.(
      value & opt Report.positive 5
      & info [ "n"; "cards" ] ~docv:"N" ~doc:"Cards per suit (at most 13).")
  and order =
    Arg.(
      value
      & opt (enum orders) Goofspiel.Random
      & info [ "order" ] ~docv:"ORDER"
          ~doc:"Upcard order: random, descending or ascending.")
  and exact =
    Arg.(
      value & flag
      & info [ "exact" ] ~doc:"Also solve every subgame in exact rationals.")
  and loss =
    Arg.(
      value & flag
      & info [ "win-loss" ]
          ~doc:
            "Export the points-optimal profile to the open extensive-form game \
             and measure how exploitable it is under win/loss scoring.")
  in
  Cmd.v
    (Cmd.info "gops"
       ~doc:
         "Solve the game of pure strategy by Ross's recursion over matrix \
          games (Rhoads and Bartholdi 2012) and print the first-move strategy.")
    Term.(term_result (const gops $ n $ order $ exact $ loss))

let cmds = [ cmd ]
