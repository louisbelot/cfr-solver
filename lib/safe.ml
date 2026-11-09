type increment = Realized | Expected

type policy =
  | Equilibrium
  | Best_response
  | Rnr of float
  | Dbr of { pmax : float; conf : Restricted.confidence }
  | Budget of { grid : float list; increment : increment }

type config = { hands : int; refresh : int; iterations : int; prior : float }

type result = {
  realized : float;
  expected : float;
  baseline : float;
  low : float;
  exploiting : int;
}

let policy_name = function
  | Equilibrium -> "equilibrium"
  | Best_response -> "best response"
  | Rnr p -> Printf.sprintf "rnr p=%g" p
  | Dbr { pmax; conf = Step k } -> Printf.sprintf "dbr %d-step %g" k pmax
  | Dbr { pmax; conf = Linear k } -> Printf.sprintf "dbr 0-%d linear %g" k pmax
  | Dbr { pmax; conf = Curve s } -> Printf.sprintf "dbr %g-curve %g" s pmax
  | Budget { increment = Realized; _ } -> "budget realized"
  | Budget { increment = Expected; _ } -> "budget expected"

let session ~rng (module G : Game.S) ~exploiter ~equilibrium ~opponent cfg
    policy =
  let module P = Play.Make (G) in
  let g = (module G : Game.S) in
  let tree = Tree.build g and ev = Restricted.evaluator g ~exploiter in
  let opp = Game.other exploiter in
  let baseline = ev.worst equilibrium in
  let solve model = function
    | `Br -> ev.respond model
    | `Rnr p ->
        Restricted.solve ~iterations:cfg.iterations ~exploiter g
          (Restricted.rnr g ~exploiter ~model ~p)
    | `Dbr conf ->
        Restricted.solve ~iterations:cfg.iterations ~exploiter g
          (Restricted.dbr g ~exploiter ~model ~conf)
  in
  let frontier = ref [] and fixed = ref equilibrium in
  let refresh counts =
    let model = Model.strategy ~prior:cfg.prior tree opp counts in
    match policy with
    | Equilibrium -> ()
    | Best_response -> fixed := solve model `Br
    | Rnr p -> fixed := solve model (`Rnr p)
    | Dbr { pmax; conf } ->
        fixed := solve model (`Dbr (Restricted.confidence conf ~pmax counts))
    | Budget { grid; _ } ->
        frontier :=
          List.map
            (fun p ->
              let s = solve model (`Rnr p) in
              (baseline -. ev.worst s, s))
            (List.sort (fun a b -> compare b a) grid)
  in
  let choose k =
    match policy with
    | Budget _ -> (
        match List.find_opt (fun (loss, _) -> loss <= k) !frontier with
        | Some (_, s) -> s
        | None -> equilibrium)
    | _ -> !fixed
  in
  let memo = ref (Strategy.empty, Strategy.empty, 0.) in
  let value sigma tau =
    let s, t, v = !memo in
    if s == sigma && t == tau then v
    else
      let v = ev.against sigma tau in
      memo := (sigma, tau, v);
      v
  in
  let counts = ref Model.empty and k = ref 0. and margin = ref 0. in
  let realized = ref 0. and expected = ref 0. in
  let low = ref 0. and exploiting = ref 0 in
  for t = 0 to cfg.hands - 1 do
    if t > 0 && t mod cfg.refresh = 0 then refresh !counts;
    let sigma = choose (Float.max 0. !k) in
    let tau = opponent t sigma in
    if sigma != equilibrium then incr exploiting;
    let ask infoset labels =
      let probs = Strategy.probs sigma infoset ~n:(List.length labels) in
      List.nth labels (Rng.draw rng probs)
    in
    let r = P.hand ~rng ~seat:exploiter ~bot:tau ~ask in
    let u = value sigma tau in
    counts := Model.observe opp r.moves !counts;
    realized := !realized +. r.payoff;
    expected := !expected +. u;
    margin := !margin +. u -. baseline;
    low := Float.min !low !margin;
    let d =
      match policy with
      | Budget { increment = Realized; _ } -> r.payoff
      | _ -> u
    in
    k := !k +. d -. baseline
  done;
  {
    realized = !realized;
    expected = !expected;
    baseline;
    low = !low;
    exploiting = !exploiting;
  }

let adversary g ~exploiter ~bait ~after =
  let ev = Restricted.evaluator g ~exploiter:(Game.other exploiter) in
  let last = ref (Strategy.empty, Strategy.empty) in
  fun t sigma ->
    if t < after then bait
    else
      let s, r = !last in
      if s == sigma then r
      else
        let r = ev.respond sigma in
        last := (sigma, r);
        r
