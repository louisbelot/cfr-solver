type learner =
  | Network of { hidden : int list; steps : int; batch : int; lr : float }
  | Exact

type config = {
  traversals : int;
  capacity : int;
  learner : learner;
  average_steps : int;
  domains : int;
  seed : int;
}

let default =
  {
    traversals = 1000;
    capacity = 1_000_000;
    learner =
      Network { hidden = [ 64; 64 ]; steps = 2000; batch = 256; lr = 3e-3 };
    average_steps = 4000;
    domains = 1;
    seed = 1;
  }

let learner_name = function
  | Exact -> "exact"
  | Network { hidden; _ } ->
      "mlp " ^ String.concat "x" (List.map string_of_int hidden)

type sample = { infoset : int; t : int; target : float array }

type t = {
  config : config;
  tree : Tree.t;
  x : float array array;
  dim : int;
  width : int;
  own : int array array;
  rng : Rng.t;
  advantages : sample Reservoir.t array;
  strategies : sample Reservoir.t array;
  policy : float array array;
  sums : float array array;
  mutable iteration : int;
  mutable touched : int;
}

let index = function Game.P0 -> 0 | P1 -> 1
let arity (s : t) i = Array.length s.tree.infosets.(i).actions
let uniform n = Array.make n (1. /. float_of_int n)

let create ?(config = default) (features : Features.t) (tree : Tree.t) =
  if config.traversals < 1 then invalid_arg "Deep_cfr.create: traversals";
  if config.domains < 1 then invalid_arg "Deep_cfr.create: domains";
  let rng = Rng.make config.seed in
  let memory () =
    Array.init 2 (fun _ ->
        Reservoir.create ~capacity:config.capacity (Rng.split rng))
  in
  let own p =
    Array.to_list tree.infosets
    |> List.mapi (fun i (info : Tree.infoset) -> (i, info.player))
    |> List.filter_map (fun (i, q) -> if q = p then Some i else None)
    |> Array.of_list
  in
  {
    config;
    tree;
    x =
      Array.map
        (fun (info : Tree.infoset) -> features.encode info.key)
        tree.infosets;
    dim = features.dim;
    width =
      Array.fold_left
        (fun w (info : Tree.infoset) -> max w (Array.length info.actions))
        1 tree.infosets;
    own = [| own Game.P0; own P1 |];
    rng;
    advantages = memory ();
    strategies = memory ();
    policy =
      Array.map
        (fun (info : Tree.infoset) -> uniform (Array.length info.actions))
        tree.infosets;
    sums =
      Array.map
        (fun (info : Tree.infoset) -> Array.make (Array.length info.actions) 0.)
        tree.infosets;
    iteration = 0;
    touched = 0;
  }

let regret_match adv =
  let pos = Array.map (fun r -> Float.max r 0.) adv in
  let total = Array.fold_left ( +. ) 0. pos in
  if total > 0. then Array.map (fun r -> r /. total) pos
  else
    let best = ref 0 in
    Array.iteri (fun a r -> if r > adv.(!best) then best := a) adv;
    Array.init (Array.length adv) (fun a -> if a = !best then 1. else 0.)

let rec traverse s p t node =
  s.touched <- s.touched + 1;
  match node with
  | Tree.Terminal u -> Game.sign p *. u
  | Chance { probs; children } -> traverse s p t children.(Rng.draw s.rng probs)
  | Decision { infoset; children } ->
      let sigma = s.policy.(infoset) in
      let q = s.tree.infosets.(infoset).player in
      if q = p then begin
        let v = Array.map (traverse s p t) children in
        let ev = ref 0. in
        Array.iteri (fun a x -> ev := !ev +. (sigma.(a) *. x)) v;
        Reservoir.add
          s.advantages.(index p)
          { infoset; t; target = Array.map (fun x -> x -. !ev) v };
        !ev
      end
      else begin
        Reservoir.add
          s.strategies.(index q)
          { infoset; t; target = Array.copy sigma };
        traverse s p t children.(Rng.draw s.rng sigma)
      end

let accumulate s p t =
  let seen = Array.make (Array.length s.policy) false in
  let rec go reach = function
    | Tree.Terminal _ -> ()
    | Chance { children; _ } -> Array.iter (go reach) children
    | Decision { infoset; children } ->
        if s.tree.infosets.(infoset).player = p then begin
          let sigma = s.policy.(infoset) in
          if not seen.(infoset) then begin
            seen.(infoset) <- true;
            Array.iteri
              (fun a x ->
                s.sums.(infoset).(a) <-
                  s.sums.(infoset).(a) +. (float_of_int t *. reach *. x))
              sigma
          end;
          Array.iteri
            (fun a c -> if sigma.(a) > 0. then go (reach *. sigma.(a)) c)
            children
        end
        else Array.iter (go reach) children
  in
  go 1. s.tree.root

let exact_means s memory own =
  let num =
    Array.map (fun sigma -> Array.make (Array.length sigma) 0.) s.policy
  in
  let den = Array.make (Array.length s.policy) 0. in
  List.iter
    (fun { infoset; t; target } ->
      let w = float_of_int t in
      den.(infoset) <- den.(infoset) +. w;
      Array.iteri
        (fun a x -> num.(infoset).(a) <- num.(infoset).(a) +. (w *. x))
        target)
    (Reservoir.to_list memory);
  Array.map
    (fun i ->
      if den.(i) > 0. then Some (Array.map (fun x -> x /. den.(i)) num.(i))
      else None)
    own

type worker = {
  tape : Autodiff.tape;
  rng : Rng.t;
  replica : Mlp.t;
  rows : int;
  xs : float array;
  ys : float array;
  mask : float array;
  weight : float array;
}

let workers (s : t) net batch =
  let d = min s.config.domains batch in
  Array.init d (fun j ->
      let rows = (batch * (j + 1) / d) - (batch * j / d) in
      {
        tape = Autodiff.tape ();
        rng = Rng.split s.rng;
        replica = Mlp.replica net;
        rows;
        xs = Array.make (rows * s.dim) 0.;
        ys = Array.make (rows * s.width) 0.;
        mask = Array.make (rows * s.width) 0.;
        weight = Array.make rows 0.;
      })

let fill (s : t) memory k =
  let w = s.width in
  for b = 0 to k.rows - 1 do
    let { infoset; t; target } =
      Reservoir.get memory (Rng.int k.rng (Reservoir.length memory))
    in
    Array.blit s.x.(infoset) 0 k.xs (b * s.dim) s.dim;
    for a = 0 to w - 1 do
      let legal = a < Array.length target in
      k.ys.((b * w) + a) <- (if legal then target.(a) else 0.);
      k.mask.((b * w) + a) <- (if legal then 1. else 0.)
    done;
    k.weight.(b) <- float_of_int t
  done

let sum a =
  let total = ref 0. in
  for i = 0 to Array.length a - 1 do
    total := !total +. a.(i)
  done;
  !total

let train (s : t) memory own ~loss ~steps ~hidden ~batch ~lr =
  let net = Mlp.create s.rng ((s.dim :: hidden) @ [ s.width ]) in
  let opt = Mlp.adam ~lr ~clip:1. net in
  let ks = workers s net batch in
  let barrier = Spin_barrier.create (Array.length ks) in
  Pool.run (Pool.default ()) (Array.length ks) (fun j ->
      let k = ks.(j) in
      for _ = 1 to steps do
        fill s memory k;
        Autodiff.reset k.tape;
        let out =
          Mlp.forward k.tape k.replica (Autodiff.const k.rows s.dim k.xs)
        in
        Autodiff.backward k.tape
          (loss k.tape out ~target:k.ys ~mask:k.mask ~weight:k.weight);
        Spin_barrier.await barrier;
        if j = 0 then begin
          let total = Array.fold_left (fun t k -> t +. sum k.weight) 0. ks in
          Array.iter
            (fun k -> Mlp.gather net ~scale:(sum k.weight /. total) k.replica)
            ks;
          Mlp.update opt net
        end;
        Spin_barrier.await barrier
      done);
  let rows = Array.length own in
  let input = Array.concat (Array.to_list (Array.map (fun i -> s.x.(i)) own)) in
  Mlp.predict net ~rows input

let advantages s p =
  let own = s.own.(index p) and memory = s.advantages.(index p) in
  if Reservoir.length memory = 0 then Array.map (fun _ -> None) own
  else
    match s.config.learner with
    | Exact -> exact_means s memory own
    | Network { hidden; steps; batch; lr } ->
        let out =
          train s memory own ~loss:Autodiff.mse ~steps ~hidden ~batch ~lr
        in
        Array.mapi
          (fun k i -> Some (Array.sub out (k * s.width) (arity s i)))
          own

let fit s p =
  let own = s.own.(index p) in
  Array.iteri
    (fun k adv ->
      let i = own.(k) in
      s.policy.(i) <-
        (match adv with
        | Some r -> regret_match r
        | None -> uniform (arity s i)))
    (advantages s p)

let iterate s =
  let t = s.iteration + 1 in
  s.iteration <- t;
  List.iter
    (fun p ->
      accumulate s p t;
      for _ = 1 to s.config.traversals do
        ignore (traverse s p t s.tree.root)
      done;
      fit s p)
    [ Game.P0; P1 ]

let run s n =
  for _ = 1 to n do
    iterate s
  done

let iterations s = s.iteration
let touched s = s.touched
let samples s = Array.fold_left (fun n r -> n + Reservoir.seen r) 0 s.advantages

let profile s table =
  let out = ref Strategy.empty in
  Array.iteri
    (fun i (info : Tree.infoset) ->
      out := Strategy.add info.key ~actions:info.actions ~probs:table.(i) !out)
    s.tree.infosets;
  !out

let current s = profile s s.policy

let average_exact s =
  profile s
    (Array.mapi
       (fun i sum ->
         let total = Array.fold_left ( +. ) 0. sum in
         if total > 0. then Array.map (fun x -> x /. total) sum
         else uniform (arity s i))
       s.sums)

let average_network (s : t) =
  let s = { s with rng = Rng.copy s.rng } in
  let table = Array.map Array.copy s.policy in
  List.iter
    (fun p ->
      let own = s.own.(index p) and memory = s.strategies.(index p) in
      if Reservoir.length memory > 0 then
        match s.config.learner with
        | Exact ->
            Array.iteri
              (fun k m -> Option.iter (fun m -> table.(own.(k)) <- m) m)
              (exact_means s memory own)
        | Network { hidden; batch; lr; _ } ->
            let out =
              train s memory own ~loss:Autodiff.cross_entropy
                ~steps:s.config.average_steps ~hidden ~batch ~lr
            in
            let mask = Array.make (Array.length out) 0. in
            Array.iteri
              (fun k i -> Array.fill mask (k * s.width) (arity s i) 1.)
              own;
            let probs =
              Autodiff.softmax ~mask
                (Autodiff.const (Array.length own) s.width out)
            in
            Array.iteri
              (fun k i ->
                table.(i) <- Array.sub probs (k * s.width) (arity s i))
              own)
    [ Game.P0; P1 ];
  profile s table
