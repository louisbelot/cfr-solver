type side = {
  range : Range.t;
  n : int;
  c1 : int array;
  c2 : int array;
  w : float array;
  twin : int array;
}

type board = { strength : int array array; order : int array array }

type decision = {
  id : int;
  player : int;
  path : string;
  dealt : int list;
  pot : float;
  committed : float;
  to_call : float;
  actions : Bet_tree.action array;
  children : node array;
  regret : float array array;
  sum : float array array;
}

and node =
  | Decision of decision
  | Chance of { cards : int array; weight : float; children : node array }
  | Fold of { folder : int; win : float }
  | Showdown of { win : float; board : board }

type scratch = {
  strat : float array array;
  next : float array array;
  vals : float array array;
  acc : float array;
  norm : float array;
  mask : float array array;
  tmp : float array array;
}

type profile = float array array array

type t = {
  pot : float;
  sides : side array;
  root : node;
  decisions : decision array;
  scratch : scratch array;
  cards : float array;
  total : float;
  algorithm : Cfr.algorithm;
  mutable iterations : int;
  mutable weight : float;
}

let side range other =
  let ids = Array.make 1326 (-1) in
  Array.iteri (fun j c -> ids.(Range.index c) <- j) other.Range.hands;
  {
    range;
    n = Range.size range;
    c1 = Array.map fst range.hands;
    c2 = Array.map snd range.hands;
    w = Array.copy range.weights;
    twin = Array.map (fun c -> ids.(Range.index c)) range.hands;
  }

let holds s h c = s.c1.(h) = c || s.c2.(h) = c

let eval_board sides cards =
  let strength s =
    Array.init s.n (fun h ->
        if List.exists (holds s h) cards then -1
        else Hand.eval (Array.of_list (s.c1.(h) :: s.c2.(h) :: cards)))
  in
  let strength = Array.map strength sides in
  let order st =
    let ids =
      List.filter (fun h -> st.(h) >= 0) (List.init (Array.length st) Fun.id)
    in
    Array.of_list (List.stable_sort (fun a b -> compare st.(a) st.(b)) ids)
  in
  { strength; order = Array.map order strength }

let pair_mass t =
  let s = t.sides.(0) and o = t.sides.(1) in
  let cards = Array.make 52 0. and total = ref 0. in
  for j = 0 to o.n - 1 do
    total := !total +. o.w.(j);
    cards.(o.c1.(j)) <- cards.(o.c1.(j)) +. o.w.(j);
    cards.(o.c2.(j)) <- cards.(o.c2.(j)) +. o.w.(j)
  done;
  let z = ref 0. in
  for h = 0 to s.n - 1 do
    let same = if s.twin.(h) >= 0 then o.w.(s.twin.(h)) else 0. in
    z :=
      !z +. (s.w.(h) *. (!total -. cards.(s.c1.(h)) -. cards.(s.c2.(h)) +. same))
  done;
  !z

let create ?(algorithm = Cfr.Vanilla) (cfg : Bet_tree.config) ~board r0 r1 =
  let b = Array.length board in
  if b < 3 || b > 5 then invalid_arg "Subgame.create: board";
  if List.length (List.sort_uniq compare (Array.to_list board)) <> b then
    invalid_arg "Subgame.create: repeated board card";
  if List.length cfg.streets <> 6 - b then
    invalid_arg "Subgame.create: one street per card left to deal";
  let clean (r : Range.t) =
    let w = Array.make 1326 0. in
    Array.iteri (fun j c -> w.(Range.index c) <- r.weights.(j)) r.hands;
    Range.of_weights ~board w
  in
  let r0 = clean r0 and r1 = clean r1 in
  let sides = [| side r0 r1; side r1 r0 |] in
  let decisions = ref []
  and count = ref 0
  and depth = ref 0
  and width = ref 1 in
  let rec expand node dealt path d ev =
    depth := max !depth d;
    match (node : Bet_tree.node) with
    | Folded { folder; win } -> Fold { folder; win }
    | Showdown { win } -> Showdown { win; board = Lazy.force ev }
    | Chance child ->
        let seen = Array.to_list board @ dealt in
        let cards =
          Array.of_list
            (List.filter (fun c -> not (List.mem c seen)) (List.init 52 Fun.id))
        in
        let left = 52 - List.length seen - 4 in
        let children =
          Array.map
            (fun c ->
              let dealt = dealt @ [ c ] in
              expand child dealt
                (path ^ "/" ^ Hand.to_string c)
                (d + 1)
                (lazy (eval_board sides (Array.to_list board @ dealt))))
            cards
        in
        Chance { cards; weight = 1. /. float_of_int left; children }
    | Decision { player; pot; committed; to_call; actions; children } ->
        let id = !count in
        incr count;
        let k = Array.length actions and n = sides.(player).n in
        width := max !width k;
        let children =
          Array.mapi
            (fun a c ->
              expand c dealt
                (path ^ Bet_tree.action_to_string actions.(a))
                (d + 1) ev)
            children
        in
        let dn =
          {
            id;
            player;
            path;
            dealt;
            pot;
            committed;
            to_call;
            actions;
            children;
            regret = Array.init k (fun _ -> Array.make n 0.);
            sum = Array.init k (fun _ -> Array.make n 0.);
          }
        in
        decisions := dn :: !decisions;
        Decision dn
  in
  let root =
    expand (Bet_tree.build cfg) [] "" 0
      (lazy (eval_board sides (Array.to_list board)))
  in
  let decisions = Array.of_list !decisions in
  Array.sort (fun a b -> compare a.id b.id) decisions;
  let n = max sides.(0).n sides.(1).n and k = !width in
  let block () = Array.init k (fun _ -> Array.make n 0.) in
  let scratch =
    Array.init (!depth + 1) (fun _ ->
        {
          strat = block ();
          next = block ();
          vals = block ();
          acc = Array.make n 0.;
          norm = Array.make n 0.;
          mask = Array.init 2 (fun _ -> Array.make n 0.);
          tmp = Array.init 2 (fun _ -> Array.make n 0.);
        })
  in
  let t =
    {
      pot = cfg.pot;
      sides;
      root;
      decisions;
      scratch;
      cards = Array.make 52 0.;
      total = 0.;
      algorithm;
      iterations = 0;
      weight = 1.;
    }
  in
  let total = pair_mass t in
  if total <= 0. then invalid_arg "Subgame.create: no compatible pair of hands";
  { t with total }

let iterations t = t.iterations
let decisions t = Array.length t.decisions
let pot t = t.pot
let range t p = t.sides.(p).range

let fold_values t ~win i opp out =
  let s = t.sides.(i) and o = t.sides.(1 - i) and cards = t.cards in
  Array.fill cards 0 52 0.;
  let total = ref 0. in
  for j = 0 to o.n - 1 do
    let r = opp.(j) in
    if r <> 0. then begin
      total := !total +. r;
      cards.(o.c1.(j)) <- cards.(o.c1.(j)) +. r;
      cards.(o.c2.(j)) <- cards.(o.c2.(j)) +. r
    end
  done;
  for h = 0 to s.n - 1 do
    let same = if s.twin.(h) >= 0 then opp.(s.twin.(h)) else 0. in
    out.(h) <- win *. (!total -. cards.(s.c1.(h)) -. cards.(s.c2.(h)) +. same)
  done

let showdown_values t ~win board i opp out =
  let s = t.sides.(i) and o = t.sides.(1 - i) and cards = t.cards in
  let mine = board.order.(i) and theirs = board.order.(1 - i) in
  let sm = board.strength.(i) and st = board.strength.(1 - i) in
  Array.fill out 0 s.n 0.;
  let add j total =
    let r = opp.(j) in
    cards.(o.c1.(j)) <- cards.(o.c1.(j)) +. r;
    cards.(o.c2.(j)) <- cards.(o.c2.(j)) +. r;
    total +. r
  in
  Array.fill cards 0 52 0.;
  let total = ref 0. and j = ref 0 in
  for x = 0 to Array.length mine - 1 do
    let h = mine.(x) in
    while !j < Array.length theirs && st.(theirs.(!j)) < sm.(h) do
      total := add theirs.(!j) !total;
      incr j
    done;
    out.(h) <- win *. (!total -. cards.(s.c1.(h)) -. cards.(s.c2.(h)))
  done;
  Array.fill cards 0 52 0.;
  let total = ref 0. and j = ref (Array.length theirs - 1) in
  for x = Array.length mine - 1 downto 0 do
    let h = mine.(x) in
    while !j >= 0 && st.(theirs.(!j)) > sm.(h) do
      total := add theirs.(!j) !total;
      decr j
    done;
    out.(h) <-
      out.(h) -. (win *. (!total -. cards.(s.c1.(h)) -. cards.(s.c2.(h))))
  done

let terminal t node i opp out =
  match node with
  | Fold { folder; win } ->
      fold_values t ~win:(if folder = i then -.win else win) i opp out
  | Showdown { win; board } -> showdown_values t ~win board i opp out
  | Decision _ | Chance _ -> assert false

let mask t p c src dst =
  let s = t.sides.(p) in
  for h = 0 to s.n - 1 do
    dst.(h) <- (if holds s h c then 0. else src.(h))
  done

let gather t i c weight src out =
  let s = t.sides.(i) in
  for h = 0 to s.n - 1 do
    if not (holds s h c) then out.(h) <- out.(h) +. (weight *. src.(h))
  done

let regret_matching dn n sc =
  let k = Array.length dn.actions in
  Array.fill sc.norm 0 n 0.;
  for a = 0 to k - 1 do
    let r = dn.regret.(a) in
    for h = 0 to n - 1 do
      sc.norm.(h) <- sc.norm.(h) +. Float.max r.(h) 0.
    done
  done;
  let u = 1. /. float_of_int k in
  for a = 0 to k - 1 do
    let r = dn.regret.(a) and s = sc.strat.(a) in
    for h = 0 to n - 1 do
      let z = sc.norm.(h) in
      s.(h) <- (if z > 0. then Float.max r.(h) 0. /. z else u)
    done
  done

let pair p mine other = if p = 0 then [| mine; other |] else [| other; mine |]

let rec walk t node d active reach out =
  match node with
  | Fold _ | Showdown _ ->
      for i = 0 to 1 do
        if active.(i) then terminal t node i reach.(1 - i) out.(i)
      done
  | Chance { cards; weight; children } ->
      let sc = t.scratch.(d) in
      for i = 0 to 1 do
        if active.(i) then Array.fill out.(i) 0 t.sides.(i).n 0.
      done;
      Array.iteri
        (fun j c ->
          mask t 0 c reach.(0) sc.mask.(0);
          mask t 1 c reach.(1) sc.mask.(1);
          walk t children.(j) (d + 1) active sc.mask sc.tmp;
          for i = 0 to 1 do
            if active.(i) then gather t i c weight sc.tmp.(i) out.(i)
          done)
        cards
  | Decision dn ->
      let p = dn.player and o = 1 - dn.player in
      let n = t.sides.(p).n and m = t.sides.(o).n in
      let sc = t.scratch.(d) in
      regret_matching dn n sc;
      if active.(p) then Array.fill out.(p) 0 n 0.;
      if active.(o) then Array.fill out.(o) 0 m 0.;
      let own = reach.(p) in
      Array.iteri
        (fun a child ->
          let s = sc.strat.(a) and next = sc.next.(a) and v = sc.vals.(a) in
          for h = 0 to n - 1 do
            next.(h) <- own.(h) *. s.(h)
          done;
          walk t child (d + 1) active (pair p next reach.(o)) (pair p v sc.acc);
          if active.(p) then
            for h = 0 to n - 1 do
              out.(p).(h) <- out.(p).(h) +. (s.(h) *. v.(h))
            done;
          if active.(o) then
            for h = 0 to m - 1 do
              out.(o).(h) <- out.(o).(h) +. sc.acc.(h)
            done)
        dn.children;
      if active.(p) then
        for a = 0 to Array.length dn.actions - 1 do
          let r = dn.regret.(a) and sum = dn.sum.(a) in
          let s = sc.strat.(a) and v = sc.vals.(a) in
          for h = 0 to n - 1 do
            r.(h) <- r.(h) +. (v.(h) -. out.(p).(h));
            sum.(h) <- sum.(h) +. (t.weight *. own.(h) *. s.(h))
          done
        done

let discount t active n =
  let pos, neg, _ = Cfr.discounts t.algorithm n in
  if pos <> 1. || neg <> 1. then
    Array.iter
      (fun dn ->
        if active.(dn.player) then
          Array.iter
            (fun r ->
              Array.iteri
                (fun h x -> r.(h) <- (x *. if x > 0. then pos else neg))
                r)
            dn.regret)
      t.decisions

let iterate t =
  let n = t.iterations + 1 in
  t.weight <- Cfr.weight t.algorithm n;
  let pass active =
    let reach = Array.map (fun s -> Array.copy s.w) t.sides in
    let out = Array.map (fun s -> Array.make s.n 0.) t.sides in
    walk t t.root 0 active reach out;
    discount t active n
  in
  if Cfr.alternating t.algorithm then begin
    pass [| true; false |];
    pass [| false; true |]
  end
  else pass [| true; true |];
  t.iterations <- n

let run t n =
  for _ = 1 to n do
    iterate t
  done

let average t =
  Array.map
    (fun dn ->
      let k = Array.length dn.actions and n = t.sides.(dn.player).n in
      let p = Array.init k (fun _ -> Array.make n 0.) in
      for h = 0 to n - 1 do
        let z = ref 0. in
        for a = 0 to k - 1 do
          z := !z +. dn.sum.(a).(h)
        done;
        for a = 0 to k - 1 do
          p.(a).(h) <-
            (if !z > 0. then dn.sum.(a).(h) /. !z else 1. /. float_of_int k)
        done
      done;
      p)
    t.decisions

let current t =
  Array.map
    (fun dn ->
      let n = t.sides.(dn.player).n and sc = t.scratch.(0) in
      regret_matching dn n sc;
      Array.init (Array.length dn.actions) (fun a -> Array.sub sc.strat.(a) 0 n))
    t.decisions

type mode = Best | Mean

let rec eval t (prof : profile) mode node d i opp out =
  match node with
  | Fold _ | Showdown _ -> terminal t node i opp out
  | Chance { cards; weight; children } ->
      let sc = t.scratch.(d) in
      Array.fill out 0 t.sides.(i).n 0.;
      Array.iteri
        (fun j c ->
          mask t (1 - i) c opp sc.mask.(1 - i);
          eval t prof mode children.(j) (d + 1) i sc.mask.(1 - i) sc.tmp.(i);
          gather t i c weight sc.tmp.(i) out)
        cards
  | Decision dn ->
      let sc = t.scratch.(d) and sigma = prof.(dn.id) in
      let n = t.sides.(i).n in
      if dn.player = i then begin
        Array.iteri
          (fun a child -> eval t prof mode child (d + 1) i opp sc.vals.(a))
          dn.children;
        for h = 0 to n - 1 do
          out.(h) <-
            (match mode with
            | Best ->
                let best = ref sc.vals.(0).(h) in
                for a = 1 to Array.length dn.actions - 1 do
                  best := Float.max !best sc.vals.(a).(h)
                done;
                !best
            | Mean ->
                let v = ref 0. in
                for a = 0 to Array.length dn.actions - 1 do
                  v := !v +. (sigma.(a).(h) *. sc.vals.(a).(h))
                done;
                !v)
        done
      end
      else begin
        Array.fill out 0 n 0.;
        let m = t.sides.(1 - i).n in
        Array.iteri
          (fun a child ->
            let next = sc.next.(a) and s = sigma.(a) in
            for h = 0 to m - 1 do
              next.(h) <- opp.(h) *. s.(h)
            done;
            eval t prof mode child (d + 1) i next sc.acc;
            for h = 0 to n - 1 do
              out.(h) <- out.(h) +. sc.acc.(h)
            done)
          dn.children
      end

let expected t prof mode i =
  let s = t.sides.(i) in
  let out = Array.make s.n 0. in
  eval t prof mode t.root 0 i (Array.copy t.sides.(1 - i).w) out;
  let v = ref 0. in
  Array.iteri (fun h x -> v := !v +. (s.w.(h) *. x)) out;
  !v /. t.total

let value t prof = expected t prof Mean 0
let best_response t i prof = expected t prof Best i

let exploitability t prof =
  (best_response t 0 prof +. best_response t 1 prof) /. 2.

let key (c1, c2) path = Range.combo_to_string (c1, c2) ^ ":" ^ path

let strategy t (prof : profile) =
  Array.fold_left
    (fun acc dn ->
      let s = t.sides.(dn.player) in
      let actions = Array.map Bet_tree.action_to_string dn.actions in
      let acc = ref acc in
      for h = 0 to s.n - 1 do
        if not (List.exists (holds s h) dn.dealt) then
          acc :=
            Strategy.add
              (key s.range.hands.(h) dn.path)
              ~actions
              ~probs:(Array.map (fun p -> p.(h)) prof.(dn.id))
              !acc
      done;
      !acc)
    Strategy.empty t.decisions

type line = {
  path : string;
  player : int;
  actions : string array;
  freqs : float array;
  mass : float;
}

let lines ?(chance = false) t (prof : profile) =
  let out = ref [] in
  let rec go node reach =
    match node with
    | Fold _ | Showdown _ -> ()
    | Chance { cards; children; _ } ->
        if chance then
          Array.iteri
            (fun j c ->
              let reach =
                Array.mapi
                  (fun p r ->
                    let m = Array.make t.sides.(p).n 0. in
                    mask t p c r m;
                    m)
                  reach
              in
              go children.(j) reach)
            cards
    | Decision dn ->
        let p = dn.player and sigma = prof.(dn.id) in
        let own = reach.(p) in
        let mass = Array.fold_left ( +. ) 0. own in
        let freqs =
          Array.map
            (fun s ->
              let f = ref 0. in
              Array.iteri (fun h r -> f := !f +. (r *. s.(h))) own;
              if mass > 0. then !f /. mass else 0.)
            sigma
        in
        out :=
          {
            path = dn.path;
            player = p;
            actions = Array.map Bet_tree.action_to_string dn.actions;
            freqs;
            mass;
          }
          :: !out;
        Array.iteri
          (fun a child ->
            let reach = Array.copy reach in
            reach.(p) <- Array.mapi (fun h r -> r *. sigma.(a).(h)) own;
            go child reach)
          dn.children
  in
  go t.root (Array.map (fun s -> Array.copy s.w) t.sides);
  List.rev !out
