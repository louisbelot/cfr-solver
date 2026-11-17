open Multi_tree

type t = {
  tree : Multi_tree.t;
  parent : int array;
  below : int array array;
  order : int array;
  subtree : int array array;
  local : int array array;
}

let infoset_of tree s =
  let rec go lo hi =
    if hi - lo <= 1 then lo
    else
      let mid = (lo + hi) / 2 in
      if tree.offset.(mid) <= s then go mid hi else go lo mid
  in
  go 0 (Array.length tree.keys)

let build tree =
  let n = Array.length tree.keys in
  let parent = Array.make n (-2) in
  let order = ref [] in
  let last = Array.make tree.players (-1) in
  let rec go = function
    | Leaf _ -> ()
    | Chance (_, children) -> Array.iter go children
    | Decision (k, children) ->
        let p = tree.owner.(k) in
        if parent.(k) = -2 then begin
          parent.(k) <- last.(p);
          order := k :: !order
        end
        else if parent.(k) <> last.(p) then
          invalid_arg "Treeplex.build: imperfect recall";
        let saved = last.(p) in
        Array.iteri
          (fun a c ->
            last.(p) <- tree.offset.(k) + a;
            go c)
          children;
        last.(p) <- saved
  in
  go tree.root;
  let order = Array.of_list (List.rev !order) in
  let below = Array.make (slots tree) [] in
  for k = 0 to n - 1 do
    if parent.(k) >= 0 then below.(parent.(k)) <- k :: below.(parent.(k))
  done;
  let subtree = Array.make n [] in
  for m = n - 1 downto 0 do
    let j = order.(m) in
    let rec up k =
      subtree.(k) <- j :: subtree.(k);
      if parent.(k) >= 0 then up (infoset_of tree parent.(k))
    in
    up j
  done;
  let subtree = Array.map Array.of_list subtree in
  let local =
    Array.map
      (fun sub ->
        let pos = Array.make (Array.length sub + 1) 0 in
        Array.iteri
          (fun m j ->
            pos.(m + 1) <- pos.(m) + tree.offset.(j + 1) - tree.offset.(j))
          sub;
        pos)
      subtree
  in
  { tree; parent; below = Array.map Array.of_list below; order; subtree; local }

let infoset t s = infoset_of t.tree s
let actions t k = t.tree.offset.(k + 1) - t.tree.offset.(k)

let plans t flat =
  let x = Array.make (Array.length flat) 0. in
  Array.iter
    (fun k ->
      let p = t.parent.(k) in
      let w = if p < 0 then 1. else x.(p) in
      for s = t.tree.offset.(k) to t.tree.offset.(k + 1) - 1 do
        x.(s) <- w *. flat.(s)
      done)
    t.order;
  x

let gradients t flat =
  let tree = t.tree in
  let n = tree.players in
  let g = Array.init n (fun _ -> Array.make (slots tree) 0.) in
  let total = Array.make n 0. in
  let reach = Array.make n 1. and last = Array.make n (-1) in
  let rec go node chance =
    match node with
    | Leaf u ->
        for i = 0 to n - 1 do
          let w = ref chance in
          for j = 0 to n - 1 do
            if j <> i then w := !w *. reach.(j)
          done;
          total.(i) <- total.(i) +. (!w *. reach.(i) *. u.(i));
          if last.(i) >= 0 then
            g.(i).(last.(i)) <- g.(i).(last.(i)) +. (!w *. u.(i))
        done
    | Chance (q, children) ->
        Array.iteri (fun j c -> go c (chance *. q.(j))) children
    | Decision (k, children) ->
        let p = tree.owner.(k) in
        let r = reach.(p) and l = last.(p) in
        Array.iteri
          (fun a c ->
            let s = tree.offset.(k) + a in
            reach.(p) <- r *. flat.(s);
            last.(p) <- s;
            go c chance)
          children;
        reach.(p) <- r;
        last.(p) <- l
  in
  go tree.root 1.;
  (g, total)

let sums tp g x =
  let off = tp.tree.offset in
  let part = Array.make (Array.length x) 0. in
  for m = Array.length tp.order - 1 downto 0 do
    let k = tp.order.(m) in
    let gk = g.(tp.tree.owner.(k)) in
    for s = off.(k) to off.(k + 1) - 1 do
      let acc = ref (gk.(s) *. x.(s)) in
      Array.iter
        (fun j ->
          for s' = off.(j) to off.(j + 1) - 1 do
            acc := !acc +. part.(s')
          done)
        tp.below.(s);
      part.(s) <- !acc
    done
  done;
  part

let sample t rng flat =
  let off = t.tree.offset in
  let f = Array.make (Array.length flat) 0. in
  for k = 0 to Array.length t.parent - 1 do
    f.(off.(k) + Rng.pick rng flat off.(k) (actions t k)) <- 1.
  done;
  f
