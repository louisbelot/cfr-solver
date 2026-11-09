type meta = Exact | Nash | Uniform

type step = {
  iteration : int;
  sizes : int * int;
  value : float;
  nash_conv : float;
}

let rows m n entry =
  let cols =
    List.init n (fun j ->
        {
          Lp.coeffs =
            (m, Rat.neg Rat.one) :: List.init m (fun i -> (i, entry i j));
          rel = Ge;
          rhs = Rat.zero;
        })
  and sum =
    { Lp.coeffs = List.init m (fun i -> (i, Rat.one)); rel = Eq; rhs = Rat.one }
  in
  sum :: cols

let maximin a =
  let m = Array.length a and n = Array.length a.(0) in
  let shift =
    Rat.sub Rat.one (Array.fold_left (Array.fold_left Rat.min) a.(0).(0) a)
  in
  match
    Lp.maximize ~vars:(m + 1)
      [ (m, Rat.one) ]
      (rows m n (fun i j -> Rat.add a.(i).(j) shift))
  with
  | Optimal { value; x; _ } -> (Array.sub x 0 m, Rat.sub value shift)
  | Infeasible | Unbounded -> failwith "Psro.matrix: LP not optimal"

let transpose neg a =
  Array.init
    (Array.length a.(0))
    (fun j -> Array.init (Array.length a) (fun i -> neg a.(i).(j)))

let matrix a =
  if Array.length a = 0 || Array.length a.(0) = 0 then
    invalid_arg "Psro.matrix: empty game";
  let x, v = maximin a and y, w = maximin (transpose Rat.neg a) in
  if not (Rat.equal v (Rat.neg w)) then failwith "Psro.matrix: duality gap";
  (x, y, v)

let maximin_float a =
  let m = Array.length a and n = Array.length a.(0) in
  let shift = 1. -. Array.fold_left (Array.fold_left Float.min) a.(0).(0) a in
  match
    Lp.maximize_float ~vars:(m + 1)
      [ (m, Rat.one) ]
      (rows m n (fun i j -> Rat.of_float (a.(i).(j) +. shift)))
  with
  | Some (value, x) -> (Array.sub x 0 m, value -. shift)
  | None -> failwith "Psro.nash: LP not optimal"

let value a x y =
  let v = ref 0. in
  Array.iteri
    (fun i row -> Array.iteri (fun j u -> v := !v +. (x.(i) *. u *. y.(j))) row)
    a;
  !v

let nash a =
  let x, _ = maximin_float a and y, _ = maximin_float (transpose Float.neg a) in
  (x, y, value a x y)

let reach (tree : Tree.t) player sigma =
  let n = Array.length tree.infosets in
  let r = Array.make n 0. in
  let probs =
    Array.map
      (fun (i : Tree.infoset) ->
        Strategy.probs sigma i.key ~n:(Array.length i.actions))
      tree.infosets
  in
  let rec go node x =
    match node with
    | Tree.Terminal _ -> ()
    | Chance { children; _ } -> Array.iter (fun c -> go c x) children
    | Decision { infoset = k; children } ->
        if tree.infosets.(k).player = player then begin
          r.(k) <- x;
          Array.iteri (fun a c -> go c (x *. probs.(k).(a))) children
        end
        else Array.iter (fun c -> go c x) children
  in
  go tree.root 1.;
  (r, probs)

let mixture (tree : Tree.t) player weighted =
  let parts = List.map (fun (w, s) -> (w, reach tree player s)) weighted in
  let out = ref Strategy.empty in
  Array.iteri
    (fun k (i : Tree.infoset) ->
      if i.player = player then begin
        let n = Array.length i.actions in
        let acc = Array.make n 0. and total = ref 0. in
        List.iter
          (fun (w, (r, probs)) ->
            let x = w *. r.(k) in
            total := !total +. x;
            Array.iteri (fun a p -> acc.(a) <- acc.(a) +. (x *. p)) probs.(k))
          parts;
        let probs =
          if !total > 0. then Array.map (fun v -> v /. !total) acc
          else Array.make n (1. /. float_of_int n)
        in
        out := Strategy.add i.key ~actions:i.actions ~probs !out
      end)
    tree.infosets;
  !out

let pure (tree : Tree.t) player =
  Array.fold_left
    (fun s (i : Tree.infoset) ->
      if i.player <> player then s
      else
        Strategy.add i.key ~actions:i.actions
          ~probs:(Array.mapi (fun a _ -> if a = 0 then 1. else 0.) i.actions)
          s)
    Strategy.empty tree.infosets

let same a b =
  List.for_all2
    (fun (k, (e : Strategy.entry)) (k', (e' : Strategy.entry)) ->
      k = k' && e.probs = e'.probs)
    (Strategy.bindings a) (Strategy.bindings b)

let run ?(meta = Nash) (module G : Game.S) ~iterations =
  let tree = Tree.build (module G) in
  let mtree = Multi_tree.build (module Multi.Of_game (G)) in
  let pop = [| [| pure tree P0 |]; [| pure tree P1 |] |] in
  let entry s t = (Multi_br.values mtree (Strategy.union s t)).(0) in
  let payoff = ref [| [| entry pop.(0).(0) pop.(1).(0) |] |] in
  let steps = ref [] in
  let profile = ref Strategy.empty in
  let rec loop it =
    let a = !payoff in
    let w0, w1, value =
      match meta with
      | Exact ->
          let x, y, v = matrix (Array.map (Array.map Rat.recover) a) in
          (Array.map Rat.to_float x, Array.map Rat.to_float y, Rat.to_float v)
      | Nash -> nash a
      | Uniform ->
          let m = Array.length pop.(0) and n = Array.length pop.(1) in
          let x = Array.make m (1. /. float_of_int m)
          and y = Array.make n (1. /. float_of_int n) in
          (x, y, value a x y)
    in
    let mix p w =
      let i = match p with Game.P0 -> 0 | P1 -> 1 in
      mixture tree p
        (Array.to_list (Array.mapi (fun k s -> (w.(k), s)) pop.(i)))
    in
    let m0 = mix P0 w0 and m1 = mix P1 w1 in
    profile := Strategy.union m0 m1;
    let nash_conv = Multi_br.nash_conv mtree !profile in
    steps :=
      {
        iteration = it;
        sizes = (Array.length pop.(0), Array.length pop.(1));
        value;
        nash_conv;
      }
      :: !steps;
    if it < iterations then begin
      let b0 = snd (Multi_br.best mtree 0 m1)
      and b1 = snd (Multi_br.best mtree 1 m0) in
      let fresh i b = meta = Uniform || not (Array.exists (same b) pop.(i)) in
      let add0 = fresh 0 b0 and add1 = fresh 1 b1 in
      if add0 || add1 then begin
        if add0 then begin
          pop.(0) <- Array.append pop.(0) [| b0 |];
          payoff := Array.append !payoff [| Array.map (entry b0) pop.(1) |]
        end;
        if add1 then begin
          pop.(1) <- Array.append pop.(1) [| b1 |];
          payoff :=
            Array.mapi
              (fun i row -> Array.append row [| entry pop.(0).(i) b1 |])
              !payoff
        end;
        loop (it + 1)
      end
    end
  in
  loop 0;
  (List.rev !steps, !profile)
