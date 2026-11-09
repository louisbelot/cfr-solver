type confidence = Step of int | Linear of int | Curve of float

let confidence c ~pmax model key =
  if not (pmax >= 0. && pmax <= 1.) then
    invalid_arg "Restricted.confidence: pmax outside [0, 1]";
  let n = Model.visits model key in
  if n = 0 then 0.
  else
    match c with
    | Step k -> if n >= k then pmax else 0.
    | Linear k -> pmax *. Float.min 1. (float_of_int n /. float_of_int k)
    | Curve s -> pmax *. float_of_int n /. (s +. float_of_int n)

let positive outcomes = List.filter (fun (q, _) -> q > 0.) outcomes

let game ?(p = 0.) ?(conf = fun _ -> 0.) (module G : Game.S) ~exploiter ~model =
  if not (p >= 0. && p <= 1.) then
    invalid_arg "Restricted.game: p outside [0, 1]";
  let module R = struct
    type history =
      | Root
      | Pinned of G.history
      | Open of G.history
      | Forced of G.history
      | Chosen of G.history

    let name = G.name ^ "-restricted"
    let root = if p = 0. then Open G.root else Root

    let forced infoset actions wrap =
      let probs = Strategy.probs model infoset ~n:(List.length actions) in
      Game.Chance
        (positive (List.mapi (fun a (_, h) -> (probs.(a), wrap h)) actions))

    let lift wrap = function
      | Game.Terminal u -> Game.Terminal u
      | Chance outcomes ->
          Chance (List.map (fun (q, h) -> (q, wrap h)) outcomes)
      | Decision d ->
          Decision
            { d with actions = List.map (fun (a, h) -> (a, wrap h)) d.actions }

    let node = function
      | Root ->
          Game.Chance (positive [ (p, Pinned G.root); (1. -. p, Open G.root) ])
      | Pinned h -> (
          match G.node h with
          | Decision { player; infoset; actions } when player <> exploiter ->
              forced infoset actions (fun h -> Pinned h)
          | n -> lift (fun h -> Pinned h) n)
      | Open h -> (
          match G.node h with
          | Decision { player; infoset; actions } as n when player <> exploiter
            ->
              let c = conf infoset in
              if c <= 0. then lift (fun h -> Open h) n
              else if c >= 1. then forced infoset actions (fun h -> Open h)
              else Chance [ (c, Forced h); (1. -. c, Chosen h) ]
          | n -> lift (fun h -> Open h) n)
      | Forced h -> (
          match G.node h with
          | Decision { infoset; actions; _ } ->
              forced infoset actions (fun h -> Open h)
          | _ -> assert false)
      | Chosen h -> lift (fun h -> Open h) (G.node h)
  end in
  (module R : Game.S)

let rnr g ~exploiter ~model ~p = game ~p g ~exploiter ~model
let dbr g ~exploiter ~model ~conf = game ~conf g ~exploiter ~model

let solve ?(algorithm = Cfr.Plus) ~iterations ~exploiter original restricted =
  let module C = Cfr.Make ((val restricted : Game.S)) in
  let t = C.create ~algorithm () in
  C.run t iterations;
  Bots.part (Tree.build original) exploiter (C.average_strategy t)

type evaluator = {
  against : Strategy.t -> Strategy.t -> float;
  worst : Strategy.t -> float;
  respond : Strategy.t -> Strategy.t;
}

let evaluator (module G : Game.S) ~exploiter =
  let tree = Multi_tree.build (module Multi.Of_game (G)) in
  let own = Tree.build (module G) in
  let e = match exploiter with Game.P0 -> 0 | P1 -> 1 in
  {
    against =
      (fun sigma opponent ->
        let sigma = Bots.part own exploiter sigma in
        (Multi_br.values tree (Strategy.union sigma opponent)).(e));
    worst = (fun sigma -> -.fst (Multi_br.best tree (1 - e) sigma));
    respond = (fun opponent -> snd (Multi_br.best tree e opponent));
  }
