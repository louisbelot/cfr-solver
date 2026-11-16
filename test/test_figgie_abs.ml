open Cfr_solver
open Figgie

module A = Figgie_abs.Make (struct
  let players = 4
  let rounds = 3
  let pace = Figgie_abs.Clock
end)

module E = Figgie_abs.Make (struct
  let players = 5
  let rounds = 4
  let pace = Figgie_abs.Events
end)

module One = Figgie_abs.Make (struct
  let players = 4
  let rounds = 1
  let pace = Figgie_abs.Clock
end)

module Two = Figgie_abs.Make (struct
  let players = 4
  let rounds = 2
  let pace = Figgie_abs.Events
end)

let hand = [| 5; 2; 2; 1 |]
let post = posterior hand

let goal =
  let p = goal_probs post in
  let j = ref 0 in
  Array.iteri (fun i x -> if x > p.(!j) then j := i) p;
  !j

let at i q = Array.init 4 (fun k -> if k = i then q else None)

let spot ?(bids = Array.make 4 None) ?(offers = Array.make 4 None) () =
  Figgie_abs.spot ~dealt:hand ~seen:hand ~hand ~others:[| 10; 10; 10 |] ~bids
    ~offers ~me:0 ~phase:1

let labels () =
  let s = spot () in
  Alcotest.(check (array string))
    "quiet book"
    [| "pass"; "bid-lo"; "bid-hi"; "offer-lo"; "offer-hi" |]
    s.labels;
  Alcotest.(check string) "key" "1" (String.sub s.key 0 1);
  Array.iter
    (function
      | Bid (x, px) | Offer (x, px) ->
          Alcotest.(check int) "goal suit" goal (index x);
          Alcotest.(check bool) "price" true (px >= 1)
      | _ -> ())
    s.moves;
  let cheap = spot ~offers:(at goal (Some { px = 1; who = 2 })) () in
  Alcotest.(check bool) "lift" true (Array.mem "lift" cheap.labels);
  Alcotest.(check char) "cheap offer" '3' cheap.key.[4];
  let rich = spot ~offers:(at goal (Some { px = 99; who = 2 })) () in
  Alcotest.(check char) "rich offer" '2' rich.key.[4];
  let mine = spot ~bids:(at goal (Some { px = 3; who = 0 })) () in
  Alcotest.(check char) "own bid" '1' mine.key.[3];
  Alcotest.(check bool) "no hit" false (Array.mem "hit" mine.labels);
  let other = (goal + 1) mod 4 in
  let junk = spot ~bids:(at other (Some { px = 60; who = 1 })) () in
  Alcotest.(check char) "junk" '1' junk.key.[5];
  Alcotest.(check bool)
    "dump" true
    (junk.moves.(Array.length junk.moves - 1) = Sell suits.(other))

let walk rng f =
  let rec go s =
    match A.node s with
    | Lazy_mccfr.Terminal u -> u
    | Chance draw -> go (draw rng)
    | Decision { infoset; actions; next; _ } ->
        f infoset actions;
        go (next (Rng.int rng (Array.length actions)))
  in
  go A.root

let key_fixes_actions () =
  let seen = Hashtbl.create 256 and rng = Rng.make 3 in
  for _ = 1 to 400 do
    ignore
      (walk rng (fun key actions ->
           match Hashtbl.find_opt seen key with
           | Some a -> Alcotest.(check (array string)) key a actions
           | None -> Hashtbl.add seen key actions))
  done;
  Alcotest.(check bool) "variety" true (Hashtbl.length seen > 50)

let zero_sum () =
  let rng = Rng.make 8 in
  for _ = 1 to 400 do
    let u = walk rng (fun _ _ -> ()) in
    Alcotest.(check (float 1e-9)) "sum" 0. (Array.fold_left ( +. ) 0. u)
  done

let true_deck_survives () =
  let rng = Rng.make 5 in
  let bots =
    [|
      Figgie_bots.noise;
      Figgie_bots.noise;
      Figgie_bots.counter ();
      Figgie_bots.noise;
    |]
  in
  for _ = 1 to 40 do
    let t = play rng ~ticks:40 bots in
    let deck = Array.find_index (( = ) t.deck) decks |> Option.get in
    let dealt =
      Array.mapi
        (fun p h ->
          let h = Array.copy h in
          List.iter
            (fun (tr : trade) ->
              let s = index tr.suit in
              if tr.buyer = p then h.(s) <- h.(s) - 1;
              if tr.seller = p then h.(s) <- h.(s) + 1)
            t.trades;
          h)
        t.hands
    in
    Array.iteri
      (fun p h ->
        let post =
          posterior
            ~seen:(Figgie_abs.seen_cards ~players:4 ~seat:p h t.trades)
            h
        in
        Alcotest.(check bool) "true deck" true (post.(deck) > 0.))
      dealt
  done

let bot_trades () =
  let uniform _ n = Array.make n (1. /. float_of_int n) in
  let b = Figgie_abs.bot ~name:"abs" ~pace:Clock ~rounds:3 uniform in
  let rng = Rng.make 2 in
  let trades = ref 0 and mine = ref 0 in
  for _ = 1 to 30 do
    let t =
      play rng ~ticks:30
        [| b; Figgie_bots.fundamentalist (); Figgie_bots.noise; b |]
    in
    trades := !trades + List.length t.trades;
    List.iter
      (fun (tr : trade) ->
        if List.mem tr.buyer [ 0; 3 ] || List.mem tr.seller [ 0; 3 ] then
          incr mine)
      t.trades
  done;
  Alcotest.(check bool) "abstract bot trades" true (!mine > 0)

let phases rng choose =
  let rec go acc s =
    match E.node s with
    | Lazy_mccfr.Terminal u -> (List.rev acc, u)
    | Chance draw -> go acc (draw rng)
    | Decision { infoset; actions; next; _ } ->
        go
          ((Char.code infoset.[0] - 48, actions) :: acc)
          (next (choose actions))
  in
  go [] E.root

let takes = [ "pass"; "lift"; "hit"; "dump" ]

let event_rounds () =
  let rng = Rng.make 6 in
  let closed = ref 0 in
  for _ = 1 to 400 do
    let ph, u = phases rng (fun a -> Rng.int rng (Array.length a)) in
    Alcotest.(check (float 1e-9)) "zero sum" 0. (Array.fold_left ( +. ) 0. u);
    ignore
      (List.fold_left
         (fun (prev, run) (p, actions) ->
           Alcotest.(check bool)
             "phase steps by one" true
             (p = prev || p = prev + 1);
           Alcotest.(check bool) "at most four rounds and a close" true (p <= 4);
           if p = 4 then begin
             incr closed;
             Array.iter
               (fun l -> Alcotest.(check bool) l true (List.mem l takes))
               actions
           end;
           let run = if p = prev then run + 1 else 1 in
           Alcotest.(check bool) "one turn each per round" true (run <= 5);
           (p, run))
         (0, 0) ph)
  done;
  Alcotest.(check bool) "closing rounds happen" true (!closed > 0);
  let ph, _ = phases rng (fun _ -> 0) in
  Alcotest.(check (list int))
    "quiet round ends it" [ 0; 0; 0; 0; 0 ] (List.map fst ph);
  let ph, _ =
    phases rng (fun a ->
        match Array.find_index (( = ) "bid-hi") a with Some i -> i | None -> 0)
  in
  Alcotest.(check int) "first bid ends the round" 1 (fst (List.nth ph 1))

let event_bot () =
  let calls = ref 0 in
  let always label _ n =
    incr calls;
    Array.init n (fun i -> if i = label then 1. else 0.)
  in
  let run label =
    calls := 0;
    let b = Figgie_abs.bot ~name:"e" ~pace:Events ~rounds:4 (always label) in
    let p = Figgie_bots.passive in
    ignore (play (Rng.make 9) ~ticks:50 [| b; p; p; p |]);
    !calls
  in
  Alcotest.(check int) "pass once, then wait" 1 (run 0);
  Alcotest.(check int) "bid, see it, repeat is rejected" 2 (run 2)

module Field = Figgie_abs.Against (struct
  let players = 4
  let rounds = 3
  let pace = Figgie_abs.Events
  let field = Figgie_bots.[| fundamentalist (); counter (); bottom_feeder |]
end)

module Idle = Figgie_abs.Against (struct
  let players = 4
  let rounds = 3
  let pace = Figgie_abs.Events
  let field = Figgie_bots.[| passive; passive; passive |]
end)

let decisions (module G : Lazy_mccfr.GAME) rng choose =
  let n = ref 0 in
  let rec go s =
    match G.node s with
    | Lazy_mccfr.Terminal u -> u
    | Chance draw -> go (draw rng)
    | Decision { player; actions; next; _ } ->
        Alcotest.(check int) "only seat 0 decides" 0 player;
        incr n;
        go (next (choose actions))
  in
  let u = go G.root in
  Alcotest.(check (float 1e-9)) "zero sum" 0. (Array.fold_left ( +. ) 0. u);
  !n

let against_field () =
  let rng = Rng.make 12 in
  let random a = Rng.int rng (Array.length a) in
  for _ = 1 to 300 do
    ignore (decisions (module Field) rng random)
  done;
  let idle = ref 0 and busy = ref 0 in
  for _ = 1 to 100 do
    idle := !idle + decisions (module Idle) rng (fun _ -> 0);
    busy := !busy + decisions (module Field) rng (fun _ -> 0)
  done;
  Alcotest.(check int) "passive field: one quiet round" 100 !idle;
  Alcotest.(check bool) "the bots make events" true (!busy > 200);
  Alcotest.check_raises "field size"
    (Invalid_argument "Figgie_abs.Against: one bot per other seat") (fun () ->
      let module _ = Figgie_abs.Against (struct
        let players = 5
        let rounds = 2
        let pace = Figgie_abs.Events
        let field = [| Figgie_bots.passive |]
      end) in
      ())

let gain (module G : Lazy_mccfr.GAME) iterations =
  let module Z = Lazy_mccfr.Make (G) in
  let z = Z.create ~seed:4 () in
  Z.run z iterations;
  let uniform _ n = Array.make n (1. /. float_of_int n) in
  let n = 20_000 in
  let score learner k =
    (Z.playout (Rng.make k) (fun p -> if p = 0 then learner else uniform)).(0)
  in
  let d = Array.init n (fun k -> score (Z.average z) k -. score uniform k) in
  let m = Array.fold_left ( +. ) 0. d /. float_of_int n in
  let v = Array.fold_left (fun a x -> a +. ((x -. m) ** 2.)) 0. d in
  let se = sqrt (v /. float_of_int (n * (n - 1))) in
  Alcotest.(check bool)
    (Printf.sprintf "gain %.2f, stderr %.2f" m se)
    true
    (m > 4. *. se)

let learns () = gain (module One) 400
let learns_events () = gain (module Two) 400

let tests =
  [
    Alcotest.test_case "spot" `Quick labels;
    Alcotest.test_case "key fixes actions" `Quick key_fixes_actions;
    Alcotest.test_case "zero sum" `Quick zero_sum;
    Alcotest.test_case "true deck survives" `Quick true_deck_survives;
    Alcotest.test_case "bot trades" `Quick bot_trades;
    Alcotest.test_case "event rounds" `Quick event_rounds;
    Alcotest.test_case "event bot" `Quick event_bot;
    Alcotest.test_case "against the field" `Quick against_field;
    Alcotest.test_case "learns" `Slow learns;
    Alcotest.test_case "learns on events" `Slow learns_events;
  ]
