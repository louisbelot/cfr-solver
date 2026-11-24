type request =
  | Create of { id : int; game : string; algorithm : string; last : bool }
  | Step of { id : int; budget : float; target : int }
  | Table of { id : int }
  | History of { id : int; key : string }
  | Deal of { id : int; seat : int; seed : int }
  | Act of { id : int; action : string }
  | Drop of { id : int }

type run = {
  session : Session.t;
  mutable sent : int;
  mutable seat : Game.player;
  mutable hand : Play.state option;
}

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

let start t id game algorithm last =
  match (Catalog.game game, Catalog.algorithm algorithm) with
  | None, _ -> error id ("unknown game " ^ game)
  | _, None -> error id ("unknown algorithm " ^ algorithm)
  | Some g, Some algorithm ->
      let session = Session.create ~algorithm ~last g in
      Hashtbl.replace t.runs id { session; sent = 0; seat = P0; hand = None };
      Json.Object
        [
          ("type", String "created");
          ("id", Int id);
          ("game", String game);
          ("infosets", Int (Array.length (Session.infosets session)));
        ]

let step t id run budget target =
  let s = run.session and start = t.now () in
  let rec go () =
    if Session.iterations s < target then begin
      Session.iterate s;
      if t.now () -. start < budget then go ()
    end
  in
  go ();
  let finished = Session.iterations s >= target in
  if finished then Session.checkpoint s;
  let points = Session.points s in
  let fresh = List.filteri (fun i _ -> i >= run.sent) points in
  run.sent <- List.length points;
  Json.Object
    [
      ("type", String "progress");
      ("id", Int id);
      ("iteration", Int (Session.iterations s));
      ("touched", Int (Session.touched s));
      ("done", Bool finished);
      ("points", List (List.map point fresh));
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
  match request with
  | Create { id; game; algorithm; last } -> start t id game algorithm last
  | Step { id; budget; target } ->
      find id (fun run -> step t id run budget target)
  | Table { id } -> find id (table id)
  | History { id; key } -> find id (fun run -> history id run key)
  | Deal { id; seat = 0; seed } -> find id (fun run -> deal id run P0 seed)
  | Deal { id; seat = 1; seed } -> find id (fun run -> deal id run P1 seed)
  | Deal { id; seat; _ } -> error id (Printf.sprintf "no seat %d" seat)
  | Act { id; action } -> find id (fun run -> act id run action)
  | Drop { id } ->
      Hashtbl.remove t.runs id;
      Json.Object [ ("type", String "dropped"); ("id", Int id) ]
