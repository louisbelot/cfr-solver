open Cfr_solver

type algorithm = {
  alternating : bool;
  predictive : bool;
  pos : int -> float;
  neg : int -> float;
  avg : int -> float;
}

let ratio e t =
  if e = infinity then 1.
  else if e = neg_infinity then 0.
  else
    let x = float_of_int t ** e in
    x /. (x +. 1.)

let of_algorithm = function
  | Cfr.Vanilla ->
      {
        alternating = false;
        predictive = false;
        pos = Fun.const 1.;
        neg = Fun.const 1.;
        avg = Fun.const 1.;
      }
  | Plus ->
      {
        alternating = true;
        predictive = false;
        pos = Fun.const 1.;
        neg = Fun.const 0.;
        avg = float_of_int;
      }
  | Linear ->
      {
        alternating = true;
        predictive = false;
        pos = ratio 1.;
        neg = ratio 1.;
        avg = float_of_int;
      }
  | Discounted { alpha; beta; gamma } ->
      {
        alternating = true;
        predictive = false;
        pos = ratio alpha;
        neg = ratio beta;
        avg = (fun t -> float_of_int t ** gamma);
      }
  | Predictive { alpha; gamma } ->
      {
        alternating = true;
        predictive = true;
        pos = ratio alpha;
        neg = Fun.const 0.;
        avg = (fun t -> float_of_int t ** gamma);
      }

module Make (G : Game.S) = struct
  let table tbl key n =
    match Hashtbl.find_opt tbl key with
    | Some x -> x
    | None ->
        let x = Array.make n 0. in
        Hashtbl.add tbl key x;
        x

  let profile regrets =
    Hashtbl.fold
      (fun key r s ->
        Strategy.add key
          ~actions:(Array.make (Array.length r) "")
          ~probs:(Regret_matching.strategy r)
          s)
      regrets Strategy.empty

  let rec value sigma h =
    match G.node h with
    | Game.Terminal u -> u
    | Chance outcomes ->
        List.fold_left
          (fun acc (p, h') -> acc +. (p *. value sigma h'))
          0. outcomes
    | Decision { infoset; actions; _ } ->
        let probs = Strategy.probs sigma infoset ~n:(List.length actions) in
        List.mapi (fun a (_, h') -> probs.(a) *. value sigma h') actions
        |> List.fold_left ( +. ) 0.

  let histories sigma p =
    let found = ref [] in
    let rec go h own others =
      match G.node h with
      | Game.Terminal _ -> ()
      | Chance outcomes ->
          List.iter (fun (q, h') -> go h' own (others *. q)) outcomes
      | Decision { player; infoset; actions } ->
          let probs = Strategy.probs sigma infoset ~n:(List.length actions) in
          if player = p then
            found := (infoset, List.map snd actions, own, others) :: !found;
          List.iteri
            (fun a (_, h') ->
              if player = p then go h' (own *. probs.(a)) others
              else go h' own (others *. probs.(a)))
            actions
    in
    go G.root 1. 1.;
    !found

  let update ~regrets ~pred ~sums alg sigma t p =
    let cfv = Hashtbl.create 16 and reach = Hashtbl.create 16 in
    List.iter
      (fun (key, children, own, others) ->
        let n = List.length children in
        let acc = table cfv key n in
        List.iteri
          (fun a ch ->
            acc.(a) <- acc.(a) +. (others *. Game.sign p *. value sigma ch))
          children;
        Hashtbl.replace reach key own)
      (histories sigma p);
    Hashtbl.iter
      (fun key q ->
        let n = Array.length q in
        let probs = Strategy.probs sigma key ~n in
        let ev = ref 0. in
        Array.iteri (fun a qa -> ev := !ev +. (probs.(a) *. qa)) q;
        let r = table regrets key n and m = table pred key n in
        Array.iteri
          (fun a qa ->
            let inst = qa -. !ev in
            if alg.predictive then begin
              let x = Float.max 0. ((r.(a) *. alg.pos (t - 1)) +. inst) in
              r.(a) <- x;
              m.(a) <- Float.max 0. ((x *. alg.pos t) +. inst)
            end
            else
              let x = r.(a) +. inst in
              r.(a) <- (x *. if x > 0. then alg.pos t else alg.neg t))
          q;
        let s = table sums key n in
        let own = Hashtbl.find reach key in
        Array.iteri
          (fun a pa -> s.(a) <- s.(a) +. (alg.avg t *. own *. pa))
          probs)
      cfv

  let solve algorithm iterations =
    let regrets = Hashtbl.create 16 and sums = Hashtbl.create 16 in
    let pred = Hashtbl.create 16 in
    let update = update ~regrets ~pred ~sums in
    let alg = of_algorithm algorithm in
    let current () = profile (if alg.predictive then pred else regrets) in
    for t = 1 to iterations do
      if alg.alternating then begin
        update alg (current ()) t P0;
        update alg (current ()) t P1
      end
      else begin
        let sigma = current () in
        update alg sigma t P0;
        update alg sigma t P1
      end
    done;
    (sums, if alg.predictive then pred else regrets)

  let run algorithm iterations =
    Hashtbl.fold
      (fun key s acc -> (key, Regret_matching.normalize s) :: acc)
      (fst (solve algorithm iterations))
      []

  let current algorithm iterations =
    Hashtbl.fold
      (fun key r acc -> (key, Regret_matching.strategy r) :: acc)
      (snd (solve algorithm iterations))
      []
end
