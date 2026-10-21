type algorithm =
  | Vanilla
  | Plus
  | Linear
  | Discounted of { alpha : float; beta : float; gamma : float }

let dcfr = Discounted { alpha = 1.5; beta = 0.; gamma = 2. }

let algorithm_name = function
  | Vanilla -> "cfr"
  | Plus -> "cfr+"
  | Linear -> "lcfr"
  | Discounted { alpha; beta; gamma } ->
      Printf.sprintf "dcfr(%g,%g,%g)" alpha beta gamma

type schedule = {
  alternating : bool;
  alpha : float;
  beta : float;
  gamma : float;
}

let schedule = function
  | Vanilla ->
      { alternating = false; alpha = infinity; beta = infinity; gamma = 0. }
  | Plus ->
      { alternating = true; alpha = infinity; beta = neg_infinity; gamma = 1. }
  | Linear -> { alternating = true; alpha = 1.; beta = 1.; gamma = 1. }
  | Discounted { alpha; beta; gamma } ->
      { alternating = true; alpha; beta; gamma }

(* t^e / (t^e + 1), with the infinite exponents meaning "keep" and "reset". A
   regret r_s added on iteration s and discounted with e = 1 ends iteration T
   as r_s * s / (T + 1), which is how Linear CFR gets its weights. *)
let factor e t =
  if e = infinity then 1.
  else if e = neg_infinity then 0.
  else
    let x = Float.pow (float_of_int t) e in
    x /. (x +. 1.)

module Make (G : Game.S) = struct
  type info = {
    player : Game.player;
    actions : string array;
    regret : float array;
    strategy_sum : float array;
    current : float array;
    mutable stamp : int;
  }

  type t = {
    table : (string, info) Hashtbl.t;
    schedule : schedule;
    mutable iterations : int;
    mutable passes : int;
    mutable weight : float;
  }

  let create ?(algorithm = Vanilla) () =
    {
      table = Hashtbl.create 64;
      schedule = schedule algorithm;
      iterations = 0;
      passes = 0;
      weight = 1.;
    }

  let iterations t = t.iterations

  let info t player key actions =
    match Hashtbl.find_opt t.table key with
    | Some i -> i
    | None ->
        let n = List.length actions in
        let i =
          {
            player;
            actions = Array.of_list (List.map fst actions);
            regret = Array.make n 0.;
            strategy_sum = Array.make n 0.;
            current = Array.make n 0.;
            stamp = -1;
          }
        in
        Hashtbl.add t.table key i;
        i

  (* An infoset is visited once per history in it, and its regrets change on
     every visit. Computing sigma^t once per pass and caching it keeps every
     visit on the same strategy, as the update rule assumes. Recomputing it on
     every visit, as some tutorial implementations do, lets later histories in
     an iteration see regret added by earlier ones. With alternating updates
     the cache is per pass, so player 1's pass sees player 0's new strategy. *)
  let current t i =
    if i.stamp <> t.passes then begin
      Regret_matching.strategy_into i.regret i.current;
      i.stamp <- t.passes
    end;
    i.current

  (* Player 0's expected payoff below [h] under the current profile. [pi0] and
     [pi1] are the players' own reach probabilities and [pc] is chance's.
     Regrets and the strategy sum change only at infosets of players for which
     [updating] holds. *)
  let rec walk t updating h pi0 pi1 pc =
    match G.node h with
    | Game.Terminal u -> u
    | Chance outcomes ->
        List.fold_left
          (fun acc (p, h') ->
            acc +. (p *. walk t updating h' pi0 pi1 (pc *. p)))
          0. outcomes
    | Decision { player; infoset; actions } ->
        let i = info t player infoset actions in
        let sigma = current t i in
        let values = Array.make (Array.length sigma) 0. in
        let v = ref 0. in
        List.iteri
          (fun a (_, h') ->
            let va =
              match player with
              | P0 -> walk t updating h' (pi0 *. sigma.(a)) pi1 pc
              | P1 -> walk t updating h' pi0 (pi1 *. sigma.(a)) pc
            in
            values.(a) <- va;
            v := !v +. (sigma.(a) *. va))
          actions;
        if updating player then begin
          let own, opp =
            match player with P0 -> (pi0, pi1) | P1 -> (pi1, pi0)
          in
          (* Counterfactual weight: the probability that everyone except
             [player] plays to reach [h]. *)
          let w = Game.sign player *. opp *. pc in
          let own = t.weight *. own in
          Array.iteri
            (fun a va ->
              i.regret.(a) <- i.regret.(a) +. (w *. (va -. !v));
              i.strategy_sum.(a) <- i.strategy_sum.(a) +. (own *. sigma.(a)))
            values
        end;
        !v

  let discount t updating n =
    let pos = factor t.schedule.alpha n and neg = factor t.schedule.beta n in
    if pos <> 1. || neg <> 1. then
      Hashtbl.iter
        (fun _ i ->
          if updating i.player then
            Array.iteri
              (fun a r -> i.regret.(a) <- (r *. if r > 0. then pos else neg))
              i.regret)
        t.table

  let pass t updating n =
    ignore (walk t updating G.root 1. 1. 1.);
    t.passes <- t.passes + 1;
    discount t updating n

  let iterate t =
    let n = t.iterations + 1 in
    t.weight <- Float.pow (float_of_int n) t.schedule.gamma;
    if t.schedule.alternating then begin
      pass t (( = ) Game.P0) n;
      pass t (( = ) Game.P1) n
    end
    else pass t (fun _ -> true) n;
    t.iterations <- n

  let run t n =
    for _ = 1 to n do
      iterate t
    done

  let to_strategy t f =
    Hashtbl.fold
      (fun key i s -> Strategy.add key ~actions:i.actions ~probs:(f i) s)
      t.table Strategy.empty

  let average_strategy t =
    to_strategy t (fun i -> Regret_matching.normalize i.strategy_sum)

  let current_strategy t =
    to_strategy t (fun i -> Regret_matching.strategy i.regret)
end
