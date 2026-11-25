type request =
  | Create of {
      id : int;
      game : string;
      algorithm : string;
      sampling : string;
      seed : int;
      last : bool;
    }
  | Step of { id : int; budget : float; target : int }
  | Table of { id : int }
  | History of { id : int; key : string }
  | Deal of { id : int; seat : int; seed : int }
  | Act of { id : int; action : string }
  | River of {
      id : int;
      board : string;
      pot : float;
      bet : float;
      cap : int;
      oop : string;
      ip : string;
      algorithm : string;
      last : bool;
    }
  | Lines of { id : int }
  | Spot of { id : int; path : string }
  | Drop of { id : int }

type tree = {
  session : Session.t;
  mutable sent : int;
  mutable seat : Game.player;
  mutable hand : Play.state option;
}

type river = {
  sub : Subgame.t;
  last : bool;
  mutable next : int;
  mutable at : int;
  mutable fresh : Json.t list;
}

type run = Tree of tree | River of river
type t = { runs : (int, run) Hashtbl.t; now : unit -> float }

let create ~now = { runs = Hashtbl.create 8; now }
let short x = if x = 0. then 0. else float_of_string (Printf.sprintf "%.6g" x)

let floats a =
  Json.List (Array.to_list (Array.map (fun x -> Json.Float (short x)) a))

let error id message =
  Json.(
    Object
      [ ("type", String "error"); ("id", Int id); ("message", String message) ])

let point (p : Session.point) =
  Json.Object
    ([
       ("iteration", Json.Int p.iteration);
       ("touched", Int p.touched);
       ("average", Float (short p.average));
     ]
    @
    match p.current with
    | Some c -> [ ("current", Json.Float (short c)) ]
    | None -> [])

let arrays (r : Session.row) =
  [
    ("average", floats r.average);
    ("current", floats r.current);
    ("regret", floats r.regret);
  ]

let row (r : Session.row) =
  Json.Object (("iteration", Json.Int r.iteration) :: arrays r)

let seat : Game.player -> int = function P0 -> 0 | P1 -> 1

let infoset (s : Tree.infoset) =
  [
    ("key", Json.String s.key);
    ("player", Int (seat s.player));
    ( "actions",
      List (Array.to_list (Array.map (fun a -> Json.String a) s.actions)) );
  ]

let start t id game algorithm sampling seed last =
  match
    (Catalog.game game, Catalog.algorithm algorithm, Catalog.sampling sampling)
  with
  | None, _, _ -> error id ("unknown game " ^ game)
  | _, None, _ -> error id ("unknown algorithm " ^ algorithm)
  | _, _, None -> error id ("unknown sampling " ^ sampling)
  | _, Some a, Some (Some _) when Cfr.predictive a ->
      error id (algorithm ^ " needs full passes")
  | Some g, Some algorithm, Some sampling ->
      let session = Session.create ~algorithm ?sampling ~seed ~last g in
      Hashtbl.replace t.runs id
        (Tree { session; sent = 0; seat = P0; hand = None });
      Json.Object
        [
          ("type", String "created");
          ("id", Int id);
          ("game", String game);
          ("infosets", Int (Array.length (Session.infosets session)));
        ]

let drive t budget target ~count ~iterate =
  let start = t.now () in
  let rec go () =
    if count () < target then begin
      iterate ();
      if t.now () -. start < budget then go ()
    end
  in
  go ();
  count () >= target

let progress id iteration touched finished points =
  Json.Object
    ([
       ("type", Json.String "progress");
       ("id", Int id);
       ("iteration", Int iteration);
     ]
    @ (match touched with Some n -> [ ("touched", Json.Int n) ] | None -> [])
    @ [ ("done", Bool finished); ("points", List points) ])

let step t id run budget target =
  let s = run.session in
  let finished =
    drive t budget target
      ~count:(fun () -> Session.iterations s)
      ~iterate:(fun () -> Session.iterate s)
  in
  if finished then Session.checkpoint s;
  let points = Session.points s in
  let fresh = List.filteri (fun i _ -> i >= run.sent) points in
  run.sent <- List.length points;
  progress id (Session.iterations s)
    (Some (Session.touched s))
    finished (List.map point fresh)

let record r =
  let sub = r.sub in
  let e prof =
    let x = Subgame.exploitability sub prof in
    Json.Float (short (100. *. x /. Subgame.pot sub))
  in
  let n = Subgame.iterations sub in
  r.at <- n;
  r.fresh <-
    Json.Object
      ([ ("iteration", Json.Int n); ("average", e (Subgame.average sub)) ]
      @ if r.last then [ ("current", e (Subgame.current sub)) ] else [])
    :: r.fresh

let river_step t id r budget target =
  let sub = r.sub in
  let iterate () =
    Subgame.iterate sub;
    let n = Subgame.iterations sub in
    if n >= r.next then begin
      record r;
      r.next <- Session.schedule 1.25 n
    end
  in
  let finished =
    drive t budget target ~count:(fun () -> Subgame.iterations sub) ~iterate
  in
  if finished && r.at < Subgame.iterations sub then record r;
  let fresh = List.rev r.fresh in
  r.fresh <- [];
  progress id (Subgame.iterations sub) None finished fresh

let river t id board pot bet cap oop ip algorithm last =
  match Catalog.algorithm algorithm with
  | None -> error id ("unknown algorithm " ^ algorithm)
  | Some algorithm -> (
      match
        let board = Hand.parse board in
        if Array.length board <> 5 then invalid_arg "a river board has 5 cards";
        if not (pot > 0. && bet > 0.) then
          invalid_arg "pot and bet must be positive";
        if cap < 1 || cap > 6 then invalid_arg "cap must be 1 to 6";
        let range s =
          let r = Range.of_string ~board s in
          if Range.size r = 0 then invalid_arg ("empty range " ^ s);
          r
        in
        let r0 = range oop and r1 = range ip in
        Subgame.create ~algorithm (Bet_tree.limit ~cap ~pot ~bet 1) ~board r0 r1
      with
      | exception Invalid_argument m -> error id m
      | sub ->
          Hashtbl.replace t.runs id
            (River { sub; last; next = 1; at = 0; fresh = [] });
          let hands p = Json.Int (Range.size (Subgame.range sub p)) in
          Json.Object
            [
              ("type", String "created");
              ("id", Int id);
              ("decisions", Int (Subgame.decisions sub));
              ("hands", List [ hands 0; hands 1 ]);
            ])

let strings a = Json.List (Array.to_list (Array.map (fun x -> Json.String x) a))

let lines id r =
  let sub = r.sub in
  let prof = Subgame.average sub in
  let line (l : Subgame.line) =
    Json.Object
      [
        ("path", String l.path);
        ("player", Int l.player);
        ("actions", strings l.actions);
        ("freqs", floats l.freqs);
        ("mass", Float (short l.mass));
      ]
  in
  Json.Object
    [
      ("type", String "lines");
      ("id", Int id);
      ("iteration", Int (Subgame.iterations sub));
      ("pot", Float (Subgame.pot sub));
      ("value", Float (short (Subgame.value sub prof)));
      ("lines", List (List.map line (Subgame.lines sub prof)));
    ]

let classes (s : Subgame.spot) =
  let k = Array.length s.actions in
  let groups = Hashtbl.create 169 in
  Array.iteri
    (fun h hand ->
      let label = Range.label hand in
      let reach, mix, n =
        Option.value
          (Hashtbl.find_opt groups label)
          ~default:(0., Array.make k 0., 0)
      in
      Array.iteri
        (fun a p -> mix.(a) <- mix.(a) +. (s.reach.(h) *. p))
        s.probs.(h);
      Hashtbl.replace groups label (reach +. s.reach.(h), mix, n + 1))
    s.hands;
  Hashtbl.fold
    (fun label (reach, mix, n) acc ->
      ( label,
        reach,
        Array.map (fun x -> if reach > 0. then x /. reach else 0.) mix,
        n )
      :: acc)
    groups []
  |> List.sort compare

let spot id r path =
  match Subgame.spot r.sub (Subgame.average r.sub) path with
  | None -> error id ("no node " ^ path)
  | Some s ->
      let group (label, reach, freqs, n) =
        Json.Object
          [
            ("label", Json.String label);
            ("combos", Int n);
            ("reach", Float (short reach));
            ("freqs", floats freqs);
          ]
      in
      let hand h c =
        Json.Object
          [
            ("combo", Json.String (Range.combo_to_string c));
            ("reach", Float (short s.reach.(h)));
            ("probs", floats s.probs.(h));
          ]
      in
      Json.Object
        [
          ("type", String "spot");
          ("id", Int id);
          ("path", String path);
          ("player", Int s.player);
          ("actions", strings s.actions);
          ("classes", List (List.map group (classes s)));
          ("hands", List (Array.to_list (Array.mapi hand s.hands)));
        ]

let table id run =
  let s = run.session in
  let rows = Session.table s in
  Json.Object
    [
      ("type", String "table");
      ("id", Int id);
      ("iteration", Int (Session.iterations s));
      ( "infosets",
        List
          (Array.to_list
             (Array.mapi
                (fun i info -> Json.Object (infoset info @ arrays rows.(i)))
                (Session.infosets s))) );
    ]

let history id run key =
  match Session.history run.session key with
  | None -> error id ("unknown infoset " ^ key)
  | Some rows ->
      Json.Object
        [
          ("type", String "history");
          ("id", Int id);
          ("key", String key);
          ("rows", List (List.map row rows));
        ]

let hand id run extra =
  let moves, prompt, payoff =
    match run.hand with
    | Some (Play.Turn { infoset; actions; moves; _ }) ->
        ( moves,
          Json.Object
            [
              ("infoset", String infoset);
              ("actions", List (List.map (fun a -> Json.String a) actions));
            ],
          Json.Null )
    | Some (Over { payoff; moves }) -> (moves, Null, Float payoff)
    | None -> ([], Null, Null)
  in
  let over = payoff <> Null in
  let move (m : Play.move) =
    Json.Object
      ([ ("player", Json.Int (seat m.player)); ("action", String m.action) ]
      @
      if over || m.player = run.seat then [ ("infoset", String m.infoset) ]
      else [])
  in
  Json.Object
    ([
       ("type", Json.String "hand");
       ("id", Int id);
       ("seat", Int (seat run.seat));
       ("moves", List (List.map move moves));
       ("prompt", prompt);
       ("payoff", payoff);
     ]
    @ extra)

let deal id run who seed =
  let module G = (val Session.game run.session) in
  let module P = Play.Make (G) in
  let module B = Best_response.Make (G) in
  let module E = Eval.Make (G) in
  let bot = Session.average run.session in
  run.seat <- who;
  run.hand <- Some (P.start ~rng:(Rng.make seed) ~seat:who ~bot);
  hand id run
    [
      ("value", Float (short (Game.sign who *. E.expected_value bot)));
      ("edge", Float (short (B.value who bot)));
    ]

let act id run action =
  match run.hand with
  | Some (Play.Turn { actions; resume; _ }) when List.mem action actions ->
      run.hand <- Some (resume action);
      hand id run []
  | Some (Turn _) -> error id ("illegal action " ^ action)
  | Some (Over _) | None -> error id "no hand in progress"

let handle t request =
  let find id k =
    match Hashtbl.find_opt t.runs id with
    | None -> error id (Printf.sprintf "no run %d" id)
    | Some run -> k run
  in
  let tree id k =
    find id (function
      | Tree run -> k run
      | River _ -> error id "not a game run")
  in
  let river_run id k =
    find id (function River r -> k r | Tree _ -> error id "not a river run")
  in
  match request with
  | Create { id; game; algorithm; sampling; seed; last } ->
      start t id game algorithm sampling seed last
  | River { id; board; pot; bet; cap; oop; ip; algorithm; last } ->
      river t id board pot bet cap oop ip algorithm last
  | Step { id; budget; target } ->
      find id (function
        | Tree run -> step t id run budget target
        | River r -> river_step t id r budget target)
  | Table { id } -> tree id (table id)
  | History { id; key } -> tree id (fun run -> history id run key)
  | Deal { id; seat = 0; seed } -> tree id (fun run -> deal id run P0 seed)
  | Deal { id; seat = 1; seed } -> tree id (fun run -> deal id run P1 seed)
  | Deal { id; seat; _ } -> error id (Printf.sprintf "no seat %d" seat)
  | Act { id; action } -> tree id (fun run -> act id run action)
  | Lines { id } -> river_run id (lines id)
  | Spot { id; path } -> river_run id (fun r -> spot id r path)
  | Drop { id } ->
      Hashtbl.remove t.runs id;
      Json.Object [ ("type", String "dropped"); ("id", Int id) ]
