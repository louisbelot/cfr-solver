open Multi_tree

type objective = Welfare | Player of int
type concept = Efce | Efcce | Nfcce

type plan = {
  tp : Treeplex.t;
  index : (int * int, int) Hashtbl.t;
  leaves : (int * int * float array) list;
  pairs : int;
}

type result = { values : float array; vars : int; rows : int; x : float array }

let plan tree =
  if tree.players <> 2 then invalid_arg "Efce_lp: two players only";
  let tp = Treeplex.build tree in
  let linked = Hashtbl.create 1024 in
  let leaves = ref [] in
  let path = [| []; [] |] and last = [| -1; -1 |] in
  let rec go = function
    | Leaf u -> leaves := (last.(0), last.(1), u) :: !leaves
    | Chance _ -> invalid_arg "Efce_lp: chance nodes"
    | Decision (k, children) ->
        let p = tree.owner.(k) in
        List.iter
          (fun k' ->
            Hashtbl.replace linked (if p = 0 then (k, k') else (k', k)) ())
          path.(1 - p);
        let l = last.(p) and saved = path.(p) in
        path.(p) <- k :: saved;
        Array.iteri
          (fun a c ->
            last.(p) <- tree.offset.(k) + a;
            go c)
          children;
        path.(p) <- saved;
        last.(p) <- l
  in
  go tree.root;
  let index = Hashtbl.create 4096 in
  let add pair =
    if not (Hashtbl.mem index pair) then
      Hashtbl.add index pair (Hashtbl.length index)
  in
  add (-1, -1);
  for s = 0 to slots tree - 1 do
    if tree.owner.(Treeplex.infoset tp s) = 0 then add (s, -1) else add (-1, s)
  done;
  Hashtbl.iter
    (fun (k0, k1) () ->
      for s0 = tree.offset.(k0) to tree.offset.(k0 + 1) - 1 do
        for s1 = tree.offset.(k1) to tree.offset.(k1 + 1) - 1 do
          add (s0, s1)
        done
      done)
    linked;
  { tp; index; leaves = !leaves; pairs = Hashtbl.length index }

let pairs p = p.pairs
let treeplex p = p.tp
let leaves p = p.leaves
let index p s0 s1 = Hashtbl.find_opt p.index (s0, s1)

let consistency p =
  let tp = p.tp in
  let tree = tp.tree in
  let off = tree.offset in
  let var pair = Hashtbl.find p.index pair in
  let with_other side s other = if side = 0 then (s, other) else (other, s) in
  let partners = Array.make (slots tree) [ -1 ] in
  Hashtbl.iter
    (fun (s0, s1) _ ->
      if s0 >= 0 && s1 >= 0 then begin
        partners.(s0) <- s1 :: partners.(s0);
        partners.(s1) <- s0 :: partners.(s1)
      end)
    p.index;
  let rows = ref [] in
  Array.iteri
    (fun k parent ->
      let pl = tree.owner.(k) in
      List.iter
        (fun o ->
          let children =
            Array.init
              (off.(k + 1) - off.(k))
              (fun a -> var (with_other pl (off.(k) + a) o))
          in
          rows := (var (with_other pl parent o), children) :: !rows)
        (List.sort_uniq compare partners.(off.(k))))
    tp.parent;
  List.rev !rows

let residual p x =
  List.fold_left
    (fun worst (parent, children) ->
      let total = Array.fold_left (fun acc c -> acc +. x.(c)) 0. children in
      Float.max worst (Float.abs (total -. x.(parent))))
    (Float.abs (x.(Hashtbl.find p.index (-1, -1)) -. 1.))
    (consistency p)

let problem ?(objective = Welfare) ?(slack = 0.) ?(concept = Efce) p =
  let tp = p.tp in
  let tree = tp.tree in
  let off = tree.offset in
  let var pair =
    match Hashtbl.find_opt p.index pair with
    | Some i -> i
    | None -> invalid_arg "Efce_lp: irrelevant pair"
  in
  let with_other side s other = if side = 0 then (s, other) else (other, s) in
  let rat = Rat.of_float in
  let rows = ref [] in
  let row coeffs rel rhs = rows := { Lp.coeffs; rel; rhs } :: !rows in
  row [ (var (-1, -1), Rat.one) ] Lp.Eq Rat.one;
  List.iter
    (fun (parent, children) ->
      row
        ((parent, Rat.neg Rat.one)
        :: Array.to_list (Array.map (fun c -> (c, Rat.one)) children))
        Lp.Eq Rat.zero)
    (consistency p);
  let next = ref p.pairs in
  let fresh () =
    let i = !next in
    next := i + 2;
    i
  in
  let free i c = [ (i, c); (i + 1, Rat.neg c) ] in
  let ancestors s =
    let rec go s acc =
      if s < 0 then acc else go tp.parent.(Treeplex.infoset tp s) (s :: acc)
    in
    go s []
  in
  let ending = Array.make (slots tree) []
  and above = Array.make (slots tree) [] in
  List.iter
    (fun ((s0, s1, _) as z) ->
      List.iter
        (fun s ->
          if s >= 0 then begin
            ending.(s) <- z :: ending.(s);
            List.iter (fun a -> above.(a) <- z :: above.(a)) (ancestors s)
          end)
        [ s0; s1 ])
    p.leaves;
  let slots_of k = List.init (off.(k + 1) - off.(k)) (fun a -> off.(k) + a) in
  let under k = List.concat_map (fun s -> above.(s)) (slots_of k) in
  let triggers =
    match concept with
    | Efce ->
        List.init (slots tree) (fun s ->
            let k = Treeplex.infoset tp s in
            (tree.owner.(k), [ k ], s, above.(s)))
    | Efcce ->
        List.init (Array.length tp.parent) (fun k ->
            (tree.owner.(k), [ k ], tp.parent.(k), under k))
    | Nfcce ->
        List.init 2 (fun pl ->
            let roots =
              List.filter
                (fun k -> tree.owner.(k) = pl && tp.parent.(k) < 0)
                (Array.to_list tp.order)
            in
            (pl, roots, -1, List.concat_map under roots))
  in
  List.iter
    (fun (pl, roots, seq, follow) ->
      let v = Hashtbl.create 16 in
      let region =
        List.concat_map (fun k -> Array.to_list tp.subtree.(k)) roots
      in
      List.iter (fun j -> Hashtbl.add v j (fresh ())) region;
      let follow =
        List.map (fun (s0, s1, u) -> (var (s0, s1), rat u.(pl))) follow
      in
      row
        (List.concat_map
           (fun k -> free (Hashtbl.find v k) (Rat.neg Rat.one))
           roots
        @ follow)
        Lp.Ge (rat (-.slack));
      List.iter
        (fun j ->
          for s' = off.(j) to off.(j + 1) - 1 do
            let children =
              List.concat_map
                (fun j' -> free (Hashtbl.find v j') (Rat.neg Rat.one))
                (Array.to_list tp.below.(s'))
            in
            let leaves =
              List.map
                (fun (s0, s1, u) ->
                  let o = if pl = 0 then s1 else s0 in
                  (var (with_other pl seq o), Rat.neg (rat u.(pl))))
                ending.(s')
            in
            row
              (free (Hashtbl.find v j) Rat.one @ children @ leaves)
              Lp.Ge Rat.zero
          done)
        region)
    triggers;
  let weight u =
    match objective with Welfare -> u.(0) +. u.(1) | Player i -> u.(i)
  in
  let objective =
    List.map (fun (s0, s1, u) -> (var (s0, s1), rat (weight u))) p.leaves
  in
  (!next, objective, List.rev !rows)

let solve ?(exact = false) ?objective ?slack ?concept p =
  let vars, objective, rows = problem ?objective ?slack ?concept p in
  let solution =
    if exact then
      match Lp.maximize ~vars objective rows with
      | Optimal { x; _ } -> Some (Array.map Rat.to_float x)
      | Infeasible | Unbounded -> None
    else Option.map snd (Lp.bound_float ~vars objective rows)
  in
  Option.map
    (fun x ->
      let values = Array.make 2 0. in
      List.iter
        (fun (s0, s1, u) ->
          let w = x.(Hashtbl.find p.index (s0, s1)) in
          values.(0) <- values.(0) +. (w *. u.(0));
          values.(1) <- values.(1) +. (w *. u.(1)))
        p.leaves;
      { values; vars; rows = List.length rows; x })
    solution

let mass p r s0 s1 =
  match Hashtbl.find_opt p.index (s0, s1) with
  | Some i -> r.x.(i)
  | None -> invalid_arg "Efce_lp.mass: irrelevant pair"
