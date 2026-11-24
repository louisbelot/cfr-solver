open Cfr_solver

let clock () =
  let t = ref 0. in
  fun () ->
    t := !t +. 1.;
    !t

let field name = function
  | Json.Object kvs -> List.assoc name kvs
  | _ -> Alcotest.fail ("not an object, looking for " ^ name)

let kind j = match field "type" j with Json.String s -> s | _ -> "?"
let int name j = match field name j with Json.Int i -> i | _ -> -1
let list name j = match field name j with Json.List l -> l | _ -> []

let fresh () =
  let e = Explorer.create ~now:(clock ()) in
  ignore
    (Explorer.handle e
       (Create { id = 1; game = "kuhn"; algorithm = "cfr+"; last = true }));
  e

let rejects_unknown_names () =
  let e = Explorer.create ~now:(clock ()) in
  let reply game algorithm =
    kind (Explorer.handle e (Create { id = 1; game; algorithm; last = false }))
  in
  Alcotest.(check string) "game" "error" (reply "chess" "cfr");
  Alcotest.(check string) "algorithm" "error" (reply "kuhn" "sgd");
  Alcotest.(check string) "both known" "created" (reply "leduc" "dcfr+")

let one_iteration_per_tick () =
  let e = fresh () in
  let r = Explorer.handle e (Step { id = 1; budget = 0.5; target = 100 }) in
  Alcotest.(check int) "iteration" 1 (int "iteration" r);
  Alcotest.(check bool) "not done" true (field "done" r = Json.Bool false)

let points_sent_once () =
  let e = fresh () in
  let rec drain acc =
    let r = Explorer.handle e (Step { id = 1; budget = 3.5; target = 60 }) in
    let acc = acc @ List.map (int "iteration") (list "points" r) in
    if field "done" r = Json.Bool true then (acc, int "iteration" r)
    else drain acc
  in
  let seen, last = drain [] in
  Alcotest.(check int) "stops at target" 60 last;
  Alcotest.(check (list int))
    "each checkpoint once"
    [ 1; 2; 3; 4; 5; 7; 9; 12; 15; 19; 24; 30; 38; 48; 60 ]
    seen

let last_point_at_target () =
  let e = fresh () in
  let r = Explorer.handle e (Step { id = 1; budget = 1e9; target = 50 }) in
  let points = list "points" r in
  Alcotest.(check int)
    "last" 50
    (int "iteration" (List.nth points (List.length points - 1)))

let table_and_history () =
  let e = fresh () in
  ignore (Explorer.handle e (Step { id = 1; budget = 1e9; target = 40 }));
  let t = Explorer.handle e (Table { id = 1 }) in
  Alcotest.(check int) "infosets" 12 (List.length (list "infosets" t));
  List.iter
    (fun i ->
      Alcotest.(check int) "two actions" 2 (List.length (list "average" i)))
    (list "infosets" t);
  let h = Explorer.handle e (History { id = 1; key = "Kb" }) in
  Alcotest.(check int) "rows" 14 (List.length (list "rows" h));
  Alcotest.(check string)
    "unknown key" "error"
    (kind (Explorer.handle e (History { id = 1; key = "nope" })))

let solved () =
  let e = fresh () in
  ignore (Explorer.handle e (Step { id = 1; budget = 1e9; target = 2000 }));
  e

let float name j =
  match field name j with
  | Json.Float x -> x
  | Json.Int i -> float_of_int i
  | _ -> nan

let deal_reports_values () =
  let e = solved () in
  let r0 = Explorer.handle e (Deal { id = 1; seat = 0; seed = 7 }) in
  let r1 = Explorer.handle e (Deal { id = 1; seat = 1; seed = 7 }) in
  Alcotest.(check (float 2e-3)) "seat 0" (-1. /. 18.) (float "value" r0);
  Alcotest.(check (float 2e-3)) "seat 1" (1. /. 18.) (float "value" r1);
  Alcotest.(check bool)
    "best response at least the value" true
    (float "edge" r0 >= float "value" r0 && float "edge" r1 >= float "value" r1);
  Alcotest.(check string)
    "seat 2" "error"
    (kind (Explorer.handle e (Deal { id = 1; seat = 2; seed = 7 })))

let hides_the_bot_until_showdown () =
  let e = solved () in
  let rec play r n =
    match field "prompt" r with
    | Json.Null -> (r, n)
    | prompt ->
        List.iter
          (fun m ->
            if int "player" m = 0 then
              Alcotest.(check bool)
                "hidden" true
                (match m with
                | Json.Object kvs -> not (List.mem_assoc "infoset" kvs)
                | _ -> false))
          (list "moves" r);
        let a =
          match list "actions" prompt with Json.String a :: _ -> a | _ -> "?"
        in
        play (Explorer.handle e (Act { id = 1; action = a })) (n + 1)
  in
  for seed = 1 to 20 do
    let r, n = play (Explorer.handle e (Deal { id = 1; seat = 1; seed })) 0 in
    Alcotest.(check bool) "acted" true (n >= 1);
    Alcotest.(check bool) "payoff" true (field "payoff" r <> Json.Null);
    List.iter
      (fun m ->
        Alcotest.(check bool) "revealed" true (field "infoset" m <> Json.Null))
      (list "moves" r);
    Alcotest.(check string)
      "over" "error"
      (kind (Explorer.handle e (Act { id = 1; action = "p" })))
  done

let rejects_illegal_actions () =
  let e = solved () in
  ignore (Explorer.handle e (Deal { id = 1; seat = 0; seed = 3 }));
  Alcotest.(check string)
    "raise in kuhn" "error"
    (kind (Explorer.handle e (Act { id = 1; action = "r" })))

let drop_forgets () =
  let e = fresh () in
  ignore (Explorer.handle e (Drop { id = 1 }));
  Alcotest.(check string)
    "gone" "error"
    (kind (Explorer.handle e (Step { id = 1; budget = 1.; target = 1 })))

let tests =
  [
    Alcotest.test_case "rejects unknown names" `Quick rejects_unknown_names;
    Alcotest.test_case "one iteration per tick" `Quick one_iteration_per_tick;
    Alcotest.test_case "points sent once" `Quick points_sent_once;
    Alcotest.test_case "last point at target" `Quick last_point_at_target;
    Alcotest.test_case "table and history" `Quick table_and_history;
    Alcotest.test_case "deal reports values" `Quick deal_reports_values;
    Alcotest.test_case "hides the bot until showdown" `Quick
      hides_the_bot_until_showdown;
    Alcotest.test_case "rejects illegal actions" `Quick rejects_illegal_actions;
    Alcotest.test_case "drop forgets" `Quick drop_forgets;
  ]
