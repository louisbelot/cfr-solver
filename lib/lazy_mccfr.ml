type 's node =
  | Terminal of float array
  | Chance of (Rng.t -> 's)
  | Decision of {
      player : int;
      infoset : string;
      actions : string array;
      next : int -> 's;
    }

module type GAME = sig
  type state

  val name : string
  val players : int
  val root : state
  val node : state -> state node
end

module Of_multi (G : Multi.S) = struct
  type state = G.history

  let name = G.name
  let players = G.players
  let root = G.root

  let node h =
    match G.node h with
    | Multi.Terminal u -> Terminal u
    | Chance outcomes ->
        let probs = Array.of_list (List.map fst outcomes)
        and next = Array.of_list (List.map snd outcomes) in
        Chance (fun rng -> next.(Rng.pick rng probs 0 (Array.length probs)))
    | Decision { player; infoset; actions } ->
        let next = Array.of_list (List.map snd actions) in
        Decision
          {
            player;
            infoset;
            actions = Array.of_list (List.map fst actions);
            next = Array.get next;
          }
end

type policy = string -> int -> float array

type entry = {
  labels : string array;
  regret : float array;
  current : float array;
  sum : float array;
  mutable stamp : int;
}

module Make (G : GAME) = struct
  type t = {
    table : (string, entry) Hashtbl.t;
    rng : Rng.t;
    weighting : Cfr.algorithm;
    interval : int;
    fixed : int -> policy option;
    solo : bool;
    mutable passes : int;
    mutable iterations : int;
    mutable touched : int;
  }

  let create ?(seed = 1) ?(weighting = Cfr.Vanilla) ?(interval = 1)
      ?(fixed = fun _ -> None) () =
    if Cfr.predictive weighting then
      invalid_arg "Lazy_mccfr.create: predictive weighting";
    if interval < 1 then
      invalid_arg "Lazy_mccfr.create: interval must be positive";
    {
      table = Hashtbl.create 4096;
      rng = Rng.make seed;
      weighting;
      interval;
      fixed;
      solo =
        List.length
          (List.filter
             (fun p -> Option.is_none (fixed p))
             (List.init G.players Fun.id))
        = 1;
      passes = 0;
      iterations = 0;
      touched = 0;
    }

  let iterations t = t.iterations
  let touched t = t.touched
  let infosets t = Hashtbl.length t.table

  let slots t =
    Hashtbl.fold (fun _ e acc -> acc + Array.length e.labels) t.table 0

  let entry t key labels =
    match Hashtbl.find_opt t.table key with
    | Some e -> e
    | None ->
        let n = Array.length labels in
        let e =
          {
            labels;
            regret = Array.make n 0.;
            current = Array.make n 0.;
            sum = Array.make n 0.;
            stamp = -1;
          }
        in
        Hashtbl.add t.table key e;
        e

  let refresh t e =
    if e.stamp <> t.passes then begin
      let n = Array.length e.regret in
      let total = ref 0. in
      for a = 0 to n - 1 do
        total := !total +. Float.max e.regret.(a) 0.
      done;
      for a = 0 to n - 1 do
        e.current.(a) <-
          (if !total > 0. then Float.max e.regret.(a) 0. /. !total
           else 1. /. float_of_int n)
      done;
      e.stamp <- t.passes
    end

  let rec walk t i r s =
    t.touched <- t.touched + 1;
    match G.node s with
    | Terminal u -> u.(i)
    | Chance draw -> walk t i r (draw t.rng)
    | Decision { player; infoset; actions; next } -> (
        let n = Array.length actions in
        match t.fixed player with
        | Some f -> walk t i r (next (Rng.pick t.rng (f infoset n) 0 n))
        | None ->
            let e = entry t infoset actions in
            refresh t e;
            if player <> i then begin
              for a = 0 to n - 1 do
                e.sum.(a) <- e.sum.(a) +. e.current.(a)
              done;
              walk t i r (next (Rng.pick t.rng e.current 0 n))
            end
            else begin
              if t.solo then
                for a = 0 to n - 1 do
                  e.sum.(a) <- e.sum.(a) +. (r *. e.current.(a))
                done;
              let vals = Array.make n 0. and v = ref 0. in
              for a = 0 to n - 1 do
                let u = walk t i (r *. e.current.(a)) (next a) in
                vals.(a) <- u;
                v := !v +. (e.current.(a) *. u)
              done;
              for a = 0 to n - 1 do
                e.regret.(a) <- e.regret.(a) +. (vals.(a) -. !v)
              done;
              !v
            end)

  let traverse t i =
    ignore (walk t i 1. G.root);
    t.passes <- t.passes + 1

  let discount t (pos, neg, avg) =
    Hashtbl.iter
      (fun _ e ->
        if pos <> 1. || neg <> 1. then
          Array.iteri
            (fun a r -> e.regret.(a) <- (r *. if r > 0. then pos else neg))
            e.regret;
        if avg <> 1. then Array.iteri (fun a x -> e.sum.(a) <- x *. avg) e.sum)
      t.table

  let iterate t =
    for i = 0 to G.players - 1 do
      if Option.is_none (t.fixed i) then traverse t i
    done;
    t.iterations <- t.iterations + 1;
    match t.weighting with
    | Vanilla -> ()
    | w ->
        if t.iterations mod t.interval = 0 then
          discount t (Cfr.discounts w (t.iterations / t.interval))

  let run t n =
    for _ = 1 to n do
      iterate t
    done

  let export t f =
    Hashtbl.fold
      (fun key e s -> Strategy.add key ~actions:e.labels ~probs:(f e) s)
      t.table Strategy.empty

  let average_strategy t = export t (fun e -> Regret_matching.normalize e.sum)
  let current_strategy t = export t (fun e -> Regret_matching.strategy e.regret)

  let average t key n =
    match Hashtbl.find_opt t.table key with
    | Some e -> Regret_matching.normalize e.sum
    | None -> Array.make n (1. /. float_of_int n)

  let mass t key =
    match Hashtbl.find_opt t.table key with
    | Some e -> Array.fold_left ( +. ) 0. e.sum
    | None -> 0.

  let playout rng policy =
    let rec go s =
      match G.node s with
      | Terminal u -> u
      | Chance draw -> go (draw rng)
      | Decision { player; infoset; actions; next } ->
          let n = Array.length actions in
          go (next (Rng.pick rng (policy player infoset n) 0 n))
    in
    go G.root
end
