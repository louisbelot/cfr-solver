open Multi_tree

type op =
  | Fill of { src : int; dst : int array; at : int }
  | Sum of { dst : int; src : int array }

type t = { plan : Efce_lp.plan; ops : op array; size : int; width : int }

let build plan =
  let tp = Efce_lp.treeplex plan in
  let tree = tp.tree in
  let off = tree.offset in
  let find p s o =
    if p = 0 then Efce_lp.index plan s o else Efce_lp.index plan o s
  in
  let rel p s o = Option.is_some (find p s o) in
  let get p s o =
    match find p s o with
    | Some i -> i
    | None -> invalid_arg "Xi.build: irrelevant pair"
  in
  let mine p = List.filter (fun k -> tree.owner.(k) = p) in
  let all = Array.to_list tp.order in
  let roots =
    Array.init 2 (fun p ->
        mine p (List.filter (fun k -> tp.parent.(k) < 0) all))
  in
  let children p s = if s < 0 then roots.(p) else Array.to_list tp.below.(s) in
  let descendants p s =
    if s < 0 then mine p all
    else List.concat_map (fun j -> Array.to_list tp.subtree.(j)) (children p s)
  in
  let connected i j = rel tree.owner.(i) off.(i) off.(j) in
  let slots k = Array.init (off.(k + 1) - off.(k)) (fun a -> off.(k) + a) in
  let ops = ref [] and width = ref 0 in
  let fill src dst =
    ops := Fill { src; dst; at = !width } :: !ops;
    width := !width + Array.length dst
  in
  let rec decompose s0 s1 =
    let seq = [| s0; s1 |] in
    let critical p =
      List.filter
        (fun i -> List.exists (connected i) (children (1 - p) seq.(1 - p)))
        (children p seq.(p))
    in
    let p, crit =
      match critical 0 with ([] | [ _ ]) as c -> (0, c) | _ -> (1, critical 1)
    in
    if List.length crit > 1 then invalid_arg "Xi.build: no critical player";
    let s = seq.(p) and o = seq.(1 - p) in
    List.iter
      (fun i ->
        if rel p off.(i) o then begin
          let acts = slots i in
          fill (get p s o) (Array.map (fun sa -> get p sa o) acts);
          Array.iter
            (fun sa -> if p = 0 then decompose sa o else decompose o sa)
            acts
        end)
      (children p s);
    List.iter
      (fun j ->
        if rel (1 - p) off.(j) s then
          match crit with
          | [ i ] when connected i j ->
              Array.iter
                (fun ja ->
                  ops :=
                    Sum
                      {
                        dst = get p s ja;
                        src = Array.map (fun ib -> get p ib ja) (slots i);
                      }
                    :: !ops)
                (slots j)
          | _ -> fill (get p s tp.parent.(j)) (Array.map (get p s) (slots j)))
      (descendants (1 - p) o)
  in
  decompose (-1) (-1);
  {
    plan;
    ops = Array.of_list (List.rev !ops);
    size = Efce_lp.pairs plan;
    width = !width;
  }

let size t = t.size
let ops t = t.ops
let root t = Option.get (Efce_lp.index t.plan (-1) (-1))
let[@inline] pos x = if x > 0. then x else 0.

let forward t pick =
  let x = Array.make t.size 0. in
  x.(root t) <- 1.;
  Array.iter
    (function
      | Fill { src; dst; at } ->
          Array.iteri (fun a d -> x.(d) <- x.(src) *. pick at a) dst
      | Sum { dst; src } ->
          x.(dst) <- Array.fold_left (fun acc s -> acc +. x.(s)) 0. src)
    t.ops;
  x

let best t g =
  let choice = Array.make t.width 0 in
  let l = Array.copy g in
  for m = Array.length t.ops - 1 downto 0 do
    match t.ops.(m) with
    | Fill { src; dst; at } ->
        let b = ref 0 in
        Array.iteri (fun a d -> if l.(d) > l.(dst.(!b)) then b := a) dst;
        choice.(at) <- !b;
        l.(src) <- l.(src) +. l.(dst.(!b))
    | Sum { dst; src } -> Array.iter (fun s -> l.(s) <- l.(s) +. l.(dst)) src
  done;
  (l.(root t), forward t (fun at a -> if choice.(at) = a then 1. else 0.))

type learner = {
  xi : t;
  predictive : bool;
  regret : float array;
  last : float array;
  y : float array;
  x : float array;
  q : float array;
}

let learner ?(predictive = false) xi =
  {
    xi;
    predictive;
    regret = Array.make xi.width 0.;
    last = Array.make xi.width 0.;
    y = Array.make xi.width 0.;
    x = Array.make xi.size 0.;
    q = Array.make xi.size 0.;
  }

let play l =
  let x = l.x and y = l.y and r = l.regret and m = l.last in
  Array.fill x 0 (Array.length x) 0.;
  x.(root l.xi) <- 1.;
  Array.iter
    (function
      | Fill { src; dst; at } ->
          let n = Array.length dst in
          let total = ref 0. in
          for a = at to at + n - 1 do
            total := !total +. pos (r.(a) +. m.(a))
          done;
          for a = 0 to n - 1 do
            let w = pos (r.(at + a) +. m.(at + a)) in
            y.(at + a) <-
              (if !total > 0. then w /. !total else 1. /. float_of_int n);
            x.(dst.(a)) <- x.(src) *. y.(at + a)
          done
      | Sum { dst; src } ->
          let acc = ref 0. in
          for i = 0 to Array.length src - 1 do
            acc := !acc +. x.(src.(i))
          done;
          x.(dst) <- !acc)
    l.xi.ops;
  x

let update l g =
  let q = l.q and y = l.y and r = l.regret and m = l.last in
  Array.blit g 0 q 0 (Array.length q);
  let ops = l.xi.ops in
  for k = Array.length ops - 1 downto 0 do
    match ops.(k) with
    | Fill { src; dst; at } ->
        let v = ref 0. in
        for a = 0 to Array.length dst - 1 do
          v := !v +. (y.(at + a) *. q.(dst.(a)))
        done;
        for a = 0 to Array.length dst - 1 do
          let d = q.(dst.(a)) -. !v in
          if l.predictive then m.(at + a) <- d;
          r.(at + a) <- pos (r.(at + a) +. d)
        done;
        q.(src) <- q.(src) +. !v
    | Sum { dst; src } ->
        for i = 0 to Array.length src - 1 do
          q.(src.(i)) <- q.(src.(i)) +. q.(dst)
        done
  done
