external ( .!() ) : float array -> int -> float = "%array_unsafe_get"
external ( .!()<- ) : float array -> int -> float -> unit = "%array_unsafe_set"
external ( .%() ) : int array -> int -> int = "%array_unsafe_get"

type f32 = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

external ( .^() ) : f32 -> int -> float = "%caml_ba_unsafe_ref_1"
external ( .^()<- ) : f32 -> int -> float -> unit = "%caml_ba_unsafe_set_1"

type store = Double of float array array | Single of f32 array

let store single k n =
  if single then
    Single
      (Array.init k (fun _ ->
           let b = Bigarray.(Array1.create Float32 C_layout n) in
           Bigarray.Array1.fill b 0.;
           b))
  else Double (Array.init k (fun _ -> Array.make n 0.))

let get st a h = match st with Double r -> r.(a).(h) | Single r -> r.(a).^(h)

let set st a h x =
  match st with Double r -> r.(a).(h) <- x | Single r -> r.(a).^(h) <- x

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
  all_in : float;
  actions : Bet_tree.action array;
  children : node array;
  regret : store;
  sum : store;
  orbits : int array array;
}

and image = { card : int; suit : Suit.perm; perm : int array array }

and node =
  | Decision of decision
  | Chance of {
      cards : int array;
      weight : float;
      children : node array;
      images : image array array;
    }
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
  iso : bool;
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

let weights (r : Range.t) =
  let w = Array.make 1326 0. in
  Array.iteri (fun j c -> w.(Range.index c) <- r.weights.(j)) r.hands;
  w

let symmetries board r0 r1 =
  let w0 = weights r0 and w1 = weights r1 in
  let keeps w p =
    Array.for_all
      (fun c -> w.(Range.index (Suit.combo p c)) = w.(Range.index c))
      Range.combos
  in
  List.filter
    (fun p -> Suit.fixes p board && keeps w0 p && keeps w1 p)
    (Array.to_list Suit.all)

let hand_map s p =
  let ids = Array.make 1326 (-1) in
  Array.iteri (fun j c -> ids.(Range.index c) <- j) s.range.hands;
  Array.map (fun c -> ids.(Range.index (Suit.combo p c))) s.range.hands

let hand_orbits maps group =
  let n = Array.length (maps Suit.identity) in
  let seen = Array.make n false and out = ref [] in
  for h = 0 to n - 1 do
    if not seen.(h) then begin
      let o = List.sort_uniq compare (List.map (fun p -> (maps p).(h)) group) in
      List.iter (fun x -> seen.(x) <- true) o;
      if List.length o > 1 then out := Array.of_list o :: !out
    end
  done;
  Array.of_list (List.rev !out)

let create ?(algorithm = Cfr.Vanilla) ?(iso = true) ?(single = false)
    (cfg : Bet_tree.config) ~board r0 r1 =
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
  let group =
    if iso then symmetries (Array.to_list board) r0 r1 else [ Suit.identity ]
  in
  let maps = Hashtbl.create 16 in
  let map i p =
    match Hashtbl.find_opt maps (i, p) with
    | Some m -> m
    | None ->
        let m = hand_map sides.(i) p in
        Hashtbl.add maps (i, p) m;
        m
  in
  let orbit_cache = Hashtbl.create 16 in
  let orbits i g =
    match Hashtbl.find_opt orbit_cache (i, g) with
    | Some o -> o
    | None ->
        let o = hand_orbits (map i) g in
        Hashtbl.add orbit_cache (i, g) o;
        o
  in
  let decisions = ref []
  and count = ref 0
  and depth = ref 0
  and width = ref 1 in
  let rec expand node g dealt path d ev =
    depth := max !depth d;
    match (node : Bet_tree.node) with
    | Folded { folder; win } -> Fold { folder; win }
    | Showdown { win } -> Showdown { win; board = Lazy.force ev }
    | Chance child ->
        let seen = Array.to_list board @ dealt in
        let classes =
          Array.of_list
            (Suit.orbits g
               (List.filter
                  (fun c -> not (List.mem c seen))
                  (List.init 52 Fun.id)))
        in
        let left = 52 - List.length seen - 4 in
        let children =
          Array.map
            (fun (c, _) ->
              let dealt = dealt @ [ c ] in
              expand child (Suit.stabilizer g [ c ]) dealt
                (path ^ "/" ^ Hand.to_string c)
                (d + 1)
                (lazy (eval_board sides (Array.to_list board @ dealt))))
            classes
        in
        let images =
          Array.map
            (fun (c, members) ->
              Array.of_list
                (List.filter_map
                   (fun (card, suit) ->
                     if card = c then None
                     else
                       let back = Suit.inverse suit in
                       Some { card; suit; perm = [| map 0 back; map 1 back |] })
                   members))
            classes
        in
        Chance
          {
            cards = Array.map fst classes;
            weight = 1. /. float_of_int left;
            children;
            images;
          }
    | Decision { player; pot; committed; to_call; all_in; actions; children } ->
        let id = !count in
        incr count;
        let k = Array.length actions and n = sides.(player).n in
        width := max !width k;
        let children =
          Array.mapi
            (fun a c ->
              expand c g dealt
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
            all_in;
            actions;
            children;
            regret = store single k n;
            sum = store single k n;
            orbits = orbits player g;
          }
        in
        decisions := dn :: !decisions;
        Decision dn
  in
  let root =
    expand (Bet_tree.build cfg) group [] "" 0
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
      iso;
      iterations = 0;
      weight = 1.;
    }
  in
  let total = pair_mass t in
  if total <= 0. then invalid_arg "Subgame.create: no compatible pair of hands";
  { t with total }

let iterations t = t.iterations

let memory t =
  Array.fold_left
    (fun acc dn ->
      let bytes = function
        | Double r -> Array.fold_left (fun n x -> n + (8 * Array.length x)) 0 r
        | Single r ->
            Array.fold_left (fun n x -> n + (4 * Bigarray.Array1.dim x)) 0 r
      in
      acc + bytes dn.regret + bytes dn.sum)
    0 t.decisions

let decisions t = Array.length t.decisions
let pot t = t.pot
let range t p = t.sides.(p).range

let fold_values t ~win i opp out =
  let s = t.sides.(i) and o = t.sides.(1 - i) and cards = t.cards in
  Array.fill cards 0 52 0.;
  let total = ref 0. in
  for j = 0 to o.n - 1 do
    let r = opp.!(j) in
    if r <> 0. then begin
      total := !total +. r;
      let a = o.c1.%(j) and b = o.c2.%(j) in
      cards.!(a) <- cards.!(a) +. r;
      cards.!(b) <- cards.!(b) +. r
    end
  done;
  for h = 0 to s.n - 1 do
    let tw = s.twin.%(h) in
    let same = if tw >= 0 then opp.!(tw) else 0. in
    out.!(h) <-
      win *. (!total -. cards.!(s.c1.%(h)) -. cards.!(s.c2.%(h)) +. same)
  done

let showdown_values t ~win board i opp out =
  let s = t.sides.(i) and o = t.sides.(1 - i) and cards = t.cards in
  let mine = board.order.(i) and theirs = board.order.(1 - i) in
  let sm = board.strength.(i) and st = board.strength.(1 - i) in
  let m = Array.length theirs in
  Array.fill out 0 s.n 0.;
  Array.fill cards 0 52 0.;
  let total = ref 0. and j = ref 0 in
  for x = 0 to Array.length mine - 1 do
    let h = mine.%(x) in
    let sh = sm.%(h) in
    while !j < m && st.%(theirs.%(!j)) < sh do
      let k = theirs.%(!j) in
      let r = opp.!(k) and a = o.c1.%(k) and b = o.c2.%(k) in
      cards.!(a) <- cards.!(a) +. r;
      cards.!(b) <- cards.!(b) +. r;
      total := !total +. r;
      incr j
    done;
    out.!(h) <- win *. (!total -. cards.!(s.c1.%(h)) -. cards.!(s.c2.%(h)))
  done;
  Array.fill cards 0 52 0.;
  let total = ref 0. and j = ref (m - 1) in
  for x = Array.length mine - 1 downto 0 do
    let h = mine.%(x) in
    let sh = sm.%(h) in
    while !j >= 0 && st.%(theirs.%(!j)) > sh do
      let k = theirs.%(!j) in
      let r = opp.!(k) and a = o.c1.%(k) and b = o.c2.%(k) in
      cards.!(a) <- cards.!(a) +. r;
      cards.!(b) <- cards.!(b) +. r;
      total := !total +. r;
      decr j
    done;
    out.!(h) <-
      out.!(h) -. (win *. (!total -. cards.!(s.c1.%(h)) -. cards.!(s.c2.%(h))))
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
    dst.!(h) <- (if s.c1.%(h) = c || s.c2.%(h) = c then 0. else src.!(h))
  done

let gather t i c weight src out =
  let s = t.sides.(i) in
  for h = 0 to s.n - 1 do
    if s.c1.%(h) <> c && s.c2.%(h) <> c then
      out.!(h) <- out.!(h) +. (weight *. src.!(h))
  done

let gather_image t i im weight src out =
  let s = t.sides.(i) and c = im.card and p = im.perm.(i) in
  for h = 0 to s.n - 1 do
    if s.c1.%(h) <> c && s.c2.%(h) <> c then
      out.!(h) <- out.!(h) +. (weight *. src.!(p.%(h)))
  done

let symmetrize dn =
  Array.iter
    (fun o ->
      let m = Array.length o in
      for a = 0 to Array.length dn.actions - 1 do
        let first = get dn.regret a o.(0) and z = ref 0. and equal = ref true in
        for x = 0 to m - 1 do
          let r = get dn.regret a o.(x) in
          z := !z +. r;
          equal := !equal && r = first
        done;
        if not !equal then begin
          let mean = !z /. float_of_int m in
          for x = 0 to m - 1 do
            set dn.regret a o.(x) mean
          done
        end
      done)
    dn.orbits

let regret_matching dn n sc =
  let k = Array.length dn.actions and norm = sc.norm in
  Array.fill norm 0 n 0.;
  for a = 0 to k - 1 do
    match dn.regret with
    | Double r ->
        let r = r.(a) in
        for h = 0 to n - 1 do
          norm.!(h) <- norm.!(h) +. Float.max r.!(h) 0.
        done
    | Single r ->
        let r = r.(a) in
        for h = 0 to n - 1 do
          norm.!(h) <- norm.!(h) +. Float.max r.^(h) 0.
        done
  done;
  let u = 1. /. float_of_int k in
  for a = 0 to k - 1 do
    let s = sc.strat.(a) in
    match dn.regret with
    | Double r ->
        let r = r.(a) in
        for h = 0 to n - 1 do
          let z = norm.!(h) in
          s.!(h) <- (if z > 0. then Float.max r.!(h) 0. /. z else u)
        done
    | Single r ->
        let r = r.(a) in
        for h = 0 to n - 1 do
          let z = norm.!(h) in
          s.!(h) <- (if z > 0. then Float.max r.^(h) 0. /. z else u)
        done
  done

let pair p mine other = if p = 0 then [| mine; other |] else [| other; mine |]

let rec walk t node d active reach out =
  match node with
  | Fold _ | Showdown _ ->
      for i = 0 to 1 do
        if active.(i) then terminal t node i reach.(1 - i) out.(i)
      done
  | Chance { cards; weight; children; images } ->
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
            if active.(i) then begin
              gather t i c weight sc.tmp.(i) out.(i);
              Array.iter
                (fun im -> gather_image t i im weight sc.tmp.(i) out.(i))
                images.(j)
            end
          done)
        cards
  | Decision dn ->
      let p = dn.player and o = 1 - dn.player in
      let n = t.sides.(p).n and m = t.sides.(o).n in
      let sc = t.scratch.(d) in
      regret_matching dn n sc;
      let own = reach.(p) and mine = out.(p) and theirs = out.(o) in
      if active.(p) then Array.fill mine 0 n 0.;
      if active.(o) then Array.fill theirs 0 m 0.;
      Array.iteri
        (fun a child ->
          let s = sc.strat.(a) and next = sc.next.(a) and v = sc.vals.(a) in
          for h = 0 to n - 1 do
            next.!(h) <- own.!(h) *. s.!(h)
          done;
          walk t child (d + 1) active (pair p next reach.(o)) (pair p v sc.acc);
          if active.(p) then
            for h = 0 to n - 1 do
              mine.!(h) <- mine.!(h) +. (s.!(h) *. v.!(h))
            done;
          if active.(o) then begin
            let acc = sc.acc in
            for h = 0 to m - 1 do
              theirs.!(h) <- theirs.!(h) +. acc.!(h)
            done
          end)
        dn.children;
      if active.(p) then begin
        for a = 0 to Array.length dn.actions - 1 do
          let s = sc.strat.(a) and v = sc.vals.(a) and w = t.weight in
          match (dn.regret, dn.sum) with
          | Double r, Double sum ->
              let r = r.(a) and sum = sum.(a) in
              for h = 0 to n - 1 do
                r.!(h) <- r.!(h) +. (v.!(h) -. mine.!(h));
                sum.!(h) <- sum.!(h) +. (w *. own.!(h) *. s.!(h))
              done
          | Single r, Single sum ->
              let r = r.(a) and sum = sum.(a) in
              for h = 0 to n - 1 do
                r.^(h) <- r.^(h) +. (v.!(h) -. mine.!(h));
                sum.^(h) <- sum.^(h) +. (w *. own.!(h) *. s.!(h))
              done
          | _ -> assert false
        done;
        symmetrize dn
      end

let discount t active n =
  let pos, neg, _ = Cfr.discounts t.algorithm n in
  if pos <> 1. || neg <> 1. then
    Array.iter
      (fun dn ->
        if active.(dn.player) then
          match dn.regret with
          | Double rs ->
              Array.iter
                (fun r ->
                  for h = 0 to Array.length r - 1 do
                    let x = r.!(h) in
                    r.!(h) <- (x *. if x > 0. then pos else neg)
                  done)
                rs
          | Single rs ->
              Array.iter
                (fun r ->
                  for h = 0 to Bigarray.Array1.dim r - 1 do
                    let x = r.^(h) in
                    r.^(h) <- (x *. if x > 0. then pos else neg)
                  done)
                rs)
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
          z := !z +. get dn.sum a h
        done;
        for a = 0 to k - 1 do
          p.(a).(h) <-
            (if !z > 0. then get dn.sum a h /. !z else 1. /. float_of_int k)
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
  | Chance { cards; weight; children; images } ->
      let sc = t.scratch.(d) in
      Array.fill out 0 t.sides.(i).n 0.;
      Array.iteri
        (fun j c ->
          mask t (1 - i) c opp sc.mask.(1 - i);
          eval t prof mode children.(j) (d + 1) i sc.mask.(1 - i) sc.tmp.(i);
          gather t i c weight sc.tmp.(i) out;
          Array.iter
            (fun im -> gather_image t i im weight sc.tmp.(i) out)
            images.(j))
        cards
  | Decision dn ->
      let sc = t.scratch.(d) and sigma = prof.(dn.id) in
      let n = t.sides.(i).n in
      if dn.player = i then begin
        Array.iteri
          (fun a child -> eval t prof mode child (d + 1) i opp sc.vals.(a))
          dn.children;
        let k = Array.length dn.actions in
        (match mode with
        | Best -> Array.blit sc.vals.(0) 0 out 0 n
        | Mean -> Array.fill out 0 n 0.);
        for a = 0 to k - 1 do
          let v = sc.vals.(a) and s = sigma.(a) in
          match mode with
          | Best ->
              for h = 0 to n - 1 do
                out.!(h) <- Float.max out.!(h) v.!(h)
              done
          | Mean ->
              for h = 0 to n - 1 do
                out.!(h) <- out.!(h) +. (s.!(h) *. v.!(h))
              done
        done
      end
      else begin
        Array.fill out 0 n 0.;
        let m = t.sides.(1 - i).n and acc = sc.acc in
        Array.iteri
          (fun a child ->
            let next = sc.next.(a) and s = sigma.(a) in
            for h = 0 to m - 1 do
              next.!(h) <- opp.!(h) *. s.!(h)
            done;
            eval t prof mode child (d + 1) i next acc;
            for h = 0 to n - 1 do
              out.!(h) <- out.!(h) +. acc.!(h)
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
  let rec go node suit path acc =
    match node with
    | Fold _ | Showdown _ -> acc
    | Chance { cards; children; images; _ } ->
        let acc = ref acc in
        Array.iteri
          (fun j c ->
            let deal card suit =
              acc :=
                go children.(j) suit
                  (path ^ "/" ^ Hand.to_string (Suit.apply suit card))
                  !acc
            in
            deal c suit;
            Array.iter (fun im -> deal c (Suit.compose suit im.suit)) images.(j))
          cards;
        !acc
    | Decision dn ->
        let s = t.sides.(dn.player) in
        let actions = Array.map Bet_tree.action_to_string dn.actions in
        let acc = ref acc in
        for h = 0 to s.n - 1 do
          if not (List.exists (holds s h) dn.dealt) then
            acc :=
              Strategy.add
                (key (Suit.combo suit s.range.hands.(h)) path)
                ~actions
                ~probs:(Array.map (fun p -> p.(h)) prof.(dn.id))
                !acc
        done;
        let acc = ref !acc in
        Array.iteri
          (fun a child ->
            acc :=
              go child suit
                (path ^ Bet_tree.action_to_string dn.actions.(a))
                !acc)
          dn.children;
        !acc
  in
  go t.root Suit.identity "" Strategy.empty

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

let combine t (p0 : profile) (p1 : profile) =
  Array.map
    (fun (dn : decision) -> if dn.player = 0 then p0.(dn.id) else p1.(dn.id))
    t.decisions

let kinds (dn : decision) =
  Array.map
    (function
      | Bet_tree.Bet x as a ->
          let pot = dn.pot
          and committed = dn.committed
          and to_call = dn.to_call in
          `Bet (Bet_tree.fraction ~pot ~committed ~to_call a, x >= dn.all_in)
      | Fold -> `Fold
      | Check -> `Check
      | Call -> `Call)
    dn.actions

let same x y =
  match (x, y) with
  | `Bet (f, a), `Bet (g, b) ->
      (a && b) || ((not (a || b)) && Float.abs (f -. g) < 1e-9)
  | x, y -> x = y

let find_index p a =
  let rec go i =
    if i = Array.length a then None else if p a.(i) then Some i else go (i + 1)
  in
  go 0

let play kind targets =
  match (find_index (same kind) targets, kind) with
  | Some j, _ -> j
  | None, `Bet (x, _) -> (
      let best = ref None in
      Array.iteri
        (fun j k ->
          match (k, !best) with
          | `Bet (f, _), Some (_, g) when Float.abs (f -. x) >= g -> ()
          | `Bet (f, _), _ -> best := Some (j, Float.abs (f -. x))
          | _ -> ())
        targets;
      match (!best, find_index (same `Call) targets) with
      | Some (j, _), _ | None, Some j -> j
      | None, None -> invalid_arg "Subgame.translate: no bet or call to play")
  | None, _ ->
      invalid_arg "Subgame.translate: action missing from the full tree"

let map_action low kind targets =
  match find_index (same kind) targets with
  | Some i -> [ (i, 1.) ]
  | None -> (
      match kind with
      | `Bet (x, _) -> (
          let bets =
            List.filter_map
              (fun i ->
                match targets.(i) with `Bet (f, _) -> Some (i, f) | _ -> None)
              (List.init (Array.length targets) Fun.id)
          in
          let pick better =
            List.fold_left
              (fun acc (i, f) ->
                match acc with
                | Some (_, g) when not (better f g) -> acc
                | _ -> Some (i, f))
              None
          in
          let lower = pick ( > ) (List.filter (fun (_, f) -> f <= x) bets)
          and upper = pick ( < ) (List.filter (fun (_, f) -> f >= x) bets) in
          match (lower, upper) with
          | Some (i, a), Some (k, b) when a < b ->
              let p = low ~a ~b x in
              List.filter (fun (_, q) -> q > 0.) [ (i, p); (k, 1. -. p) ]
          | Some (i, _), _ | None, Some (i, _) -> [ (i, 1.) ]
          | None, None -> invalid_arg "Subgame.translate: no bet to map to")
      | _ ->
          invalid_arg "Subgame.translate: action missing from the abstraction")

let translate ~low ~abstract (prof : profile) full =
  Array.iteri
    (fun p s ->
      let a = abstract.sides.(p) in
      if a.range.hands <> s.range.hands || a.w <> s.w then
        invalid_arg "Subgame.translate: ranges differ")
    full.sides;
  if abstract.iso <> full.iso then
    invalid_arg "Subgame.translate: one tree folds suits and the other not";
  let out = Array.make (Array.length full.decisions) [||] in
  let rec go node beliefs =
    match node with
    | Fold _ | Showdown _ -> ()
    | Chance { children; _ } ->
        Array.iteri
          (fun j child ->
            go child
              (Array.map
                 (List.filter_map (fun (m, w, r) ->
                      match m with
                      | Chance c -> Some (c.children.(j), w, r)
                      | _ -> None))
                 beliefs))
          children
    | Decision fd ->
        let p = fd.player and n = full.sides.(fd.player).n in
        let k = Array.length fd.actions and fk = kinds fd in
        let beliefs =
          Array.map
            (List.filter_map (fun (m, w, r) ->
                 match m with
                 | Decision ad when ad.player = p -> Some (ad, w, r)
                 | _ -> None))
            beliefs
        in
        let num = Array.init k (fun _ -> Array.make n 0.)
        and flat = Array.init k (fun _ -> Array.make n 0.)
        and den = Array.make n 0.
        and total = ref 0. in
        List.iter
          (fun (ad, w, own) ->
            let sigma = prof.(ad.id) in
            Array.iteri
              (fun i kind ->
                let j = play kind fk in
                for h = 0 to n - 1 do
                  num.(j).(h) <- num.(j).(h) +. (w *. own.(h) *. sigma.(i).(h));
                  flat.(j).(h) <- flat.(j).(h) +. (w *. sigma.(i).(h))
                done)
              (kinds ad);
            for h = 0 to n - 1 do
              den.(h) <- den.(h) +. (w *. own.(h))
            done;
            total := !total +. w)
          beliefs.(p);
        out.(fd.id) <-
          Array.init k (fun j ->
              Array.init n (fun h ->
                  if den.(h) > 0. then num.(j).(h) /. den.(h)
                  else if !total > 0. then flat.(j).(h) /. !total
                  else 1. /. float_of_int k));
        Array.iteri
          (fun j child ->
            let mine =
              List.concat_map
                (fun (ad, w, own) ->
                  List.filter_map
                    (fun (i, kind) ->
                      if play kind fk <> j then None
                      else
                        let s = prof.(ad.id).(i) in
                        Some
                          ( ad.children.(i),
                            w,
                            Array.mapi (fun h x -> x *. s.(h)) own ))
                    (List.mapi
                       (fun i kind -> (i, kind))
                       (Array.to_list (kinds ad))))
                beliefs.(p)
            and theirs =
              List.concat_map
                (fun (ad, w, own) ->
                  List.map
                    (fun (i, q) -> (ad.children.(i), w *. q, own))
                    (map_action low fk.(j) (kinds ad)))
                beliefs.(1 - p)
            in
            go child (if p = 0 then [| mine; theirs |] else [| theirs; mine |]))
          fd.children
  in
  go full.root
    (Array.map
       (fun s -> [ (abstract.root, 1., Array.copy s.w) ])
       abstract.sides);
  out
