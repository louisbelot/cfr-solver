module Make (G : Game.S) = struct
  type info = {
    actions : string array;
    regret : float array;
    strategy_sum : float array;
    current : float array;
    mutable stamp : int;
  }

  type t = { table : (string, info) Hashtbl.t; mutable iterations : int }

  let create () = { table = Hashtbl.create 64; iterations = 0 }
  let iterations t = t.iterations

  let info t key actions =
    match Hashtbl.find_opt t.table key with
    | Some i -> i
    | None ->
        let n = List.length actions in
        let i =
          {
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
     every visit. Computing sigma^t once per iteration and caching it keeps
     every visit on the same strategy, as the update rule assumes. Recomputing
     it on every visit, as some tutorial implementations do, lets later
     histories in an iteration see regret added by earlier ones. *)
  let current t i =
    if i.stamp <> t.iterations then begin
      Regret_matching.strategy_into i.regret i.current;
      i.stamp <- t.iterations
    end;
    i.current

  (* Player 0's expected payoff below [h] under the current profile. [pi0] and
     [pi1] are the players' own reach probabilities and [pc] is chance's. *)
  let rec walk t h pi0 pi1 pc =
    match G.node h with
    | Game.Terminal u -> u
    | Chance outcomes ->
        List.fold_left
          (fun acc (p, h') -> acc +. (p *. walk t h' pi0 pi1 (pc *. p)))
          0. outcomes
    | Decision { player; infoset; actions } ->
        let i = info t infoset actions in
        let sigma = current t i in
        let values = Array.make (Array.length sigma) 0. in
        let v = ref 0. in
        List.iteri
          (fun a (_, h') ->
            let va =
              match player with
              | P0 -> walk t h' (pi0 *. sigma.(a)) pi1 pc
              | P1 -> walk t h' pi0 (pi1 *. sigma.(a)) pc
            in
            values.(a) <- va;
            v := !v +. (sigma.(a) *. va))
          actions;
        let own, opp =
          match player with P0 -> (pi0, pi1) | P1 -> (pi1, pi0)
        in
        (* Counterfactual weight: the probability that everyone except
           [player] plays to reach [h]. *)
        let w = Game.sign player *. opp *. pc in
        Array.iteri
          (fun a va ->
            i.regret.(a) <- i.regret.(a) +. (w *. (va -. !v));
            i.strategy_sum.(a) <- i.strategy_sum.(a) +. (own *. sigma.(a)))
          values;
        !v

  let iterate t =
    ignore (walk t G.root 1. 1. 1.);
    t.iterations <- t.iterations + 1

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
