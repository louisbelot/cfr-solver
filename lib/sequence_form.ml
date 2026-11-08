type side = {
  keys : string array;
  actions : string array array;
  parent : int array;
  first : int array;
  children : int list array;
  owner : int array;
}

type t = { sides : side array; payoff : (int * int * Rat.t) array }

let index = function Game.P0 -> 0 | P1 -> 1

let build game =
  let tree = Tree.build game in
  let n = Array.length tree.infosets in
  let parent = Array.make n (-1) and first = Array.make n (-1) in
  let next = [| 1; 1 |] and found = [| []; [] |] in
  let payoff = Hashtbl.create 1024 in
  let rec go node reach s0 s1 =
    match node with
    | Tree.Terminal u ->
        let v = Rat.mul reach (Rat.of_float u) in
        let old =
          Option.value ~default:Rat.zero (Hashtbl.find_opt payoff (s0, s1))
        in
        Hashtbl.replace payoff (s0, s1) (Rat.add old v)
    | Chance { probs; children } ->
        let ps = Array.map Rat.recover probs in
        if not (Rat.equal (Array.fold_left Rat.add Rat.zero ps) Rat.one) then
          invalid_arg "Sequence_form.build: chance probabilities";
        Array.iteri (fun k c -> go c (Rat.mul reach ps.(k)) s0 s1) children
    | Decision { infoset = i; children } ->
        let p = index tree.infosets.(i).player in
        let s = if p = 0 then s0 else s1 in
        if first.(i) < 0 then (
          parent.(i) <- s;
          first.(i) <- next.(p);
          next.(p) <- next.(p) + Array.length children;
          found.(p) <- i :: found.(p))
        else if parent.(i) <> s then
          invalid_arg "Sequence_form.build: imperfect recall";
        Array.iteri
          (fun a c ->
            let s = first.(i) + a in
            if p = 0 then go c reach s s1 else go c reach s0 s)
          children
  in
  go tree.root Rat.one 0 0;
  let side p =
    let ids = Array.of_list (List.rev found.(p)) in
    let children = Array.make next.(p) [] and owner = Array.make next.(p) 0 in
    Array.iteri
      (fun j i ->
        children.(parent.(i)) <- j :: children.(parent.(i));
        Array.iteri
          (fun a _ -> owner.(first.(i) + a) <- 1 + j)
          tree.infosets.(i).actions)
      ids;
    {
      keys = Array.map (fun i -> tree.infosets.(i).key) ids;
      actions = Array.map (fun i -> tree.infosets.(i).actions) ids;
      parent = Array.map (fun i -> parent.(i)) ids;
      first = Array.map (fun i -> first.(i)) ids;
      children = Array.map List.rev children;
      owner;
    }
  in
  {
    sides = [| side 0; side 1 |];
    payoff =
      Hashtbl.fold (fun (i, j) v acc -> (i, j, v) :: acc) payoff []
      |> List.filter (fun (_, _, v) -> not (Rat.is_zero v))
      |> List.sort (fun (i, j, _) (i', j', _) -> compare (i, j) (i', j'))
      |> Array.of_list;
  }

let sequences t p = Array.length t.sides.(index p).owner
let infosets t p = Array.length t.sides.(index p).keys
let entries t = Array.length t.payoff

let plan t p sigma =
  let s = t.sides.(index p) in
  let x = Array.make (Array.length s.owner) Rat.zero in
  x.(0) <- Rat.one;
  Array.iteri
    (fun j key ->
      let n = Array.length s.actions.(j) in
      let w = Array.map Rat.of_float (Strategy.probs sigma key ~n) in
      let total = Array.fold_left Rat.add Rat.zero w in
      let reach = x.(s.parent.(j)) in
      Array.iteri
        (fun a wa ->
          x.(s.first.(j) + a) <-
            (if Rat.is_zero total then Rat.div reach (Rat.of_int n)
             else Rat.mul reach (Rat.div wa total)))
        w)
    s.keys;
  x

let strategy t p x =
  let s = t.sides.(index p) in
  let acc = ref Strategy.empty in
  Array.iteri
    (fun j key ->
      let n = Array.length s.actions.(j) and reach = x.(s.parent.(j)) in
      let probs =
        Array.init n (fun a ->
            if Rat.is_zero reach then 1. /. Float.of_int n
            else Rat.to_float (Rat.div x.(s.first.(j) + a) reach))
      in
      acc := Strategy.add key ~actions:s.actions.(j) ~probs !acc)
    s.keys;
  !acc

let value t x y =
  Array.fold_left
    (fun acc (i, j, v) -> Rat.add acc (Rat.mul v (Rat.mul x.(i) y.(j))))
    Rat.zero t.payoff

let best_response t p y =
  let s = t.sides.(index p) in
  let g = Array.make (Array.length s.owner) Rat.zero in
  Array.iter
    (fun (i, j, v) ->
      match p with
      | Game.P0 -> g.(i) <- Rat.add g.(i) (Rat.mul v y.(j))
      | P1 -> g.(j) <- Rat.sub g.(j) (Rat.mul v y.(i)))
    t.payoff;
  let n = Array.length s.keys in
  let best = Array.make n Rat.zero in
  let below seq =
    List.fold_left (fun acc j -> Rat.add acc best.(j)) g.(seq) s.children.(seq)
  in
  for j = n - 1 downto 0 do
    let f = s.first.(j) in
    let v = ref (below f) in
    for a = 1 to Array.length s.actions.(j) - 1 do
      v := Rat.max !v (below (f + a))
    done;
    best.(j) <- !v
  done;
  below 0

let nash_conv t sigma =
  Rat.add
    (best_response t P0 (plan t P1 sigma))
    (best_response t P1 (plan t P0 sigma))

let exploitability t sigma = Rat.div (nash_conv t sigma) (Rat.of_int 2)

type solution = {
  value : Rat.t;
  plans : Rat.t array * Rat.t array;
  strategy : Strategy.t;
  pivots : int * int;
}

let maximin ~warm t p =
  let me = t.sides.(index p) and opp = t.sides.(1 - index p) in
  let nx = Array.length me.owner and nq = 1 + Array.length opp.keys in
  let qp k = nx + k and qm k = nx + nq + k in
  let free k c = [ (qp k, c); (qm k, Rat.neg c) ] in
  let realization =
    { Lp.coeffs = [ (0, Rat.one) ]; rel = Eq; rhs = Rat.one }
    :: List.mapi
         (fun j par ->
           let n = Array.length me.actions.(j) in
           {
             Lp.coeffs =
               (par, Rat.neg Rat.one)
               :: List.init n (fun a -> (me.first.(j) + a, Rat.one));
             rel = Eq;
             rhs = Rat.zero;
           })
         (Array.to_list me.parent)
  in
  let terms = Array.make (Array.length opp.owner) [] in
  Array.iter
    (fun (i, j, v) ->
      match p with
      | Game.P0 -> terms.(j) <- (i, Rat.neg v) :: terms.(j)
      | P1 -> terms.(i) <- (j, v) :: terms.(i))
    t.payoff;
  let duals =
    List.init (Array.length opp.owner) (fun tau ->
        {
          Lp.coeffs =
            free opp.owner.(tau) Rat.one
            @ List.concat_map
                (fun j -> free (1 + j) (Rat.neg Rat.one))
                opp.children.(tau)
            @ terms.(tau);
          rel = Le;
          rhs = Rat.zero;
        })
  in
  match
    Lp.maximize ~warm
      ~vars:(nx + (2 * nq))
      (free 0 Rat.one) (realization @ duals)
  with
  | Optimal { value; x; pivots } -> (value, Array.sub x 0 nx, pivots)
  | Infeasible | Unbounded -> failwith "Sequence_form.solve: LP not optimal"

let solve ?(warm = true) t =
  let v0, x, k0 = maximin ~warm t P0 and v1, y, k1 = maximin ~warm t P1 in
  if not (Rat.equal v0 (Rat.neg v1)) then
    failwith "Sequence_form.solve: duality gap";
  {
    value = v0;
    plans = (x, y);
    strategy = Strategy.union (strategy t P0 x) (strategy t P1 y);
    pivots = (k0, k1);
  }
