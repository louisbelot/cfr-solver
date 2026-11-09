type meta =
  | Exact
  | Nash
  | Uniform
  | Prd of { iterations : int; dt : float; gamma : float }
  | Alpharank of { alpha : float; m : int }

type oracle = Best | Cfr of int

type step = {
  iteration : int;
  sizes : int array;
  values : float array;
  nash_conv : float;
  seconds : float;
}

type member = { flat : float array; own : float array }

let member (tree : Multi_tree.t) p sigma =
  let flat = Multi_br.probs tree sigma in
  let own = Array.make (Array.length tree.keys) 0. in
  let rec go node x =
    match node with
    | Multi_tree.Leaf _ -> ()
    | Chance (_, children) -> Array.iter (fun c -> go c x) children
    | Decision (k, children) ->
        if tree.owner.(k) = p then begin
          own.(k) <- x;
          Array.iteri
            (fun a c -> go c (x *. flat.(tree.offset.(k) + a)))
            children
        end
        else Array.iter (fun c -> go c x) children
  in
  go tree.root 1.;
  { flat; own }

let mix (tree : Multi_tree.t) p weighted =
  let flat = Array.make (Multi_tree.slots tree) 0. in
  Array.iteri
    (fun k _ ->
      if tree.owner.(k) = p then begin
        let lo = tree.offset.(k) and hi = tree.offset.(k + 1) in
        let total = ref 0. in
        List.iter
          (fun (w, m) ->
            let x = w *. m.own.(k) in
            if x > 0. then begin
              total := !total +. x;
              for s = lo to hi - 1 do
                flat.(s) <- flat.(s) +. (x *. m.flat.(s))
              done
            end)
          weighted;
        for s = lo to hi - 1 do
          flat.(s) <-
            (if !total > 0. then flat.(s) /. !total
             else 1. /. float_of_int (hi - lo))
        done
      end)
    tree.keys;
  Multi_br.strategy tree p flat

let mixture tree p weighted =
  mix tree p (List.map (fun (w, s) -> (w, member tree p s)) weighted)

let pure (tree : Multi_tree.t) p =
  let flat = Array.make (Multi_tree.slots tree) 0. in
  Array.iteri
    (fun k _ -> if tree.owner.(k) = p then flat.(tree.offset.(k)) <- 1.)
    tree.keys;
  Multi_br.strategy tree p flat

let leaves (tree : Multi_tree.t) f =
  let rec go = function
    | Multi_tree.Leaf u -> f u
    | Chance (_, children) | Decision (_, children) -> Array.iter go children
  in
  go tree.root

let bound tree =
  let b = ref 0. in
  leaves tree (Array.iter (fun x -> b := Float.max !b (Float.abs x)));
  !b

let zero_sum tree =
  let ok = ref true in
  leaves tree (fun u ->
      if Float.abs (Array.fold_left ( +. ) 0. u) > 1e-9 then ok := false);
  !ok

type lp = {
  add_row : float array -> unit;
  add_col : float array -> unit;
  solution : unit -> float array * float array;
}

module F =
  Meta_lp.Make
    (struct
      include Lp.Approx

      let sub = ( -. )
      let sign x = if x > 1e-9 then 1 else if x < -1e-9 then -1 else 0
    end)
    (struct
      let delta = 1e-9
    end)

module E =
  Meta_lp.Make
    (Lp.Exact)
    (struct
      let delta = Rat.zero
    end)

let normalize v =
  let v = Array.map (Float.max 0.) v in
  let total = Array.fold_left ( +. ) 0. v in
  Array.map (fun x -> x /. total) v

let lp meta ~shift =
  match meta with
  | Nash ->
      let t = F.create ~shift in
      Some
        {
          add_row = F.add_row t;
          add_col = F.add_col t;
          solution =
            (fun () ->
              let read () =
                let x, y, _ = F.solution t in
                let x = normalize x and y = normalize y in
                (F.gap t (x, y), (x, y))
              in
              let g, xy = read () in
              if g <= 1e-10 then xy
              else begin
                F.refactor t;
                snd (read ())
              end);
        }
  | Exact ->
      let t = E.create ~shift:(Rat.recover shift)
      and q = Array.map Rat.recover in
      Some
        {
          add_row = (fun a -> E.add_row t (q a));
          add_col = (fun a -> E.add_col t (q a));
          solution =
            (fun () ->
              let x, y, _ = E.solution t in
              (Array.map Rat.to_float x, Array.map Rat.to_float y));
        }
  | Uniform | Prd _ | Alpharank _ -> None

let same a b = a.flat = b.flat

let run ?(meta = Nash) ?(oracle = Best) (module G : Multi.S) ~iterations =
  let start = Sys.time () in
  let tree = Multi_tree.build (module G) in
  let n = tree.players in
  let solver = lp meta ~shift:(bound tree +. 1.) in
  if Option.is_some solver && not (n = 2 && zero_sum tree) then
    invalid_arg "Psro.run: LP meta-solvers need a two-player zero-sum game";
  let pops = Array.init n (fun p -> [| member tree p (pure tree p) |]) in
  let table = Hashtbl.create 1024 in
  let key s = Array.fold_left (fun k i -> (k * (iterations + 2)) + i) 0 s in
  let sizes () = Array.map Array.length pops in
  let owner = Array.make (Multi_tree.slots tree) 0 in
  Array.iteri
    (fun k p ->
      for s = tree.offset.(k) to tree.offset.(k + 1) - 1 do
        owner.(s) <- p
      done)
    tree.owner;
  let eval s =
    let probs = Array.mapi (fun slot p -> pops.(p).(s.(p)).flat.(slot)) owner in
    Hashtbl.replace table (key s) (Multi_br.payoff tree probs)
  in
  let fill p =
    let k = sizes () in
    let i = k.(p) - 1 in
    k.(p) <- 1;
    for idx = 0 to Meta_game.count k - 1 do
      let s = Meta_game.decode k idx in
      s.(p) <- i;
      eval s
    done
  in
  let entry s = Hashtbl.find table (key s) in
  eval (Array.make n 0);
  Option.iter
    (fun lp ->
      lp.add_row [||];
      lp.add_col [| (entry [| 0; 0 |]).(0) |])
    solver;
  let lp_add p =
    Option.iter
      (fun lp ->
        let k = sizes () in
        if p = 0 then
          lp.add_row
            (Array.init k.(1) (fun j -> (entry [| k.(0) - 1; j |]).(0)))
        else
          lp.add_col
            (Array.init k.(0) (fun i -> (entry [| i; k.(1) - 1 |]).(0))))
      solver
  in
  let tensor () =
    let k = sizes () in
    {
      Meta_game.sizes = k;
      u =
        Array.init (Meta_game.count k) (fun idx ->
            entry (Meta_game.decode k idx));
    }
  in
  let weights () =
    let t = tensor () in
    let w =
      match (meta, solver) with
      | _, Some lp ->
          let x, y = lp.solution () in
          [| x; y |]
      | Prd { iterations; dt; gamma }, None ->
          Meta_game.prd ~iterations ~dt ~gamma t
      | Alpharank { alpha; m }, None ->
          Meta_game.(marginals t (alpharank ~alpha ~m t))
      | (Exact | Nash | Uniform), None ->
          Array.map (fun k -> Array.make k (1. /. float_of_int k)) t.sizes
    in
    (w, Meta_game.expected t w)
  in
  let steps = ref [] and profile = ref Strategy.empty in
  let rec loop it =
    let w, values = weights () in
    let mixes =
      Array.mapi
        (fun p wp ->
          mix tree p
            (Array.to_list (Array.mapi (fun i m -> (wp.(i), m)) pops.(p))))
        w
    in
    profile := Array.fold_left Strategy.union Strategy.empty mixes;
    steps :=
      {
        iteration = it;
        sizes = sizes ();
        values;
        nash_conv = Multi_br.nash_conv tree !profile;
        seconds = Sys.time () -. start;
      }
      :: !steps;
    if it < iterations then begin
      let others p =
        Array.fold_left Strategy.union Strategy.empty
          (Array.mapi (fun q m -> if q = p then Strategy.empty else m) mixes)
      in
      let fresh =
        Array.init n (fun p ->
            let sigma =
              match oracle with
              | Best -> snd (Multi_br.best tree p (others p))
              | Cfr k -> Cfr_br.respond tree p (others p) ~iterations:k
            in
            member tree p sigma)
      in
      let added = ref false in
      Array.iteri
        (fun p b ->
          if meta = Uniform || not (Array.exists (same b) pops.(p)) then begin
            pops.(p) <- Array.append pops.(p) [| b |];
            fill p;
            lp_add p;
            added := true
          end)
        fresh;
      if !added then loop (it + 1)
    end
  in
  loop 0;
  (List.rev !steps, !profile)
