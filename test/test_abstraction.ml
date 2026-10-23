open Cfr_solver

let d = Leduc.standard

let renamed =
  {
    Leduc.name = "renamed";
    round1 = (fun c -> "x" ^ string_of_int c);
    round2 = (fun c b -> Printf.sprintf "y%d.%d" c b);
  }

let blind =
  { Leduc.name = "blind"; round1 = (fun _ -> "*"); round2 = (fun _ _ -> "*") }

let identity () =
  let module A = Abstraction.Make (Leduc) ((val Leduc.game d)) in
  List.iter (fun (k, a) -> Alcotest.(check string) k k a) A.map;
  Alcotest.(check int) "size" 288 A.size

let rename_commutes () =
  let module R = (val Leduc.game ~abstraction:renamed d) in
  let module A = Abstraction.Make (Leduc) (R) in
  let module CR = Cfr.Make (R) in
  let module CF = Cfr.Make (Leduc) in
  let r = CR.create ~algorithm:Plus () and f = CF.create ~algorithm:Plus () in
  CR.run r 50;
  CF.run f 50;
  let lifted = A.lift (CR.average_strategy r) in
  List.iter2
    (fun (k, (x : Strategy.entry)) (k', (y : Strategy.entry)) ->
      Alcotest.(check string) "key" k k';
      Alcotest.(check (array (float 0.))) k x.probs y.probs)
    (Strategy.bindings (CF.average_strategy f))
    (Strategy.bindings lifted)

let blind_size () =
  let module G = (val Leduc.game ~abstraction:blind d) in
  let module A = Abstraction.Make (Leduc) (G) in
  Alcotest.(check int) "abstract infosets" 36 A.size;
  Alcotest.(check int) "full infosets" 288 (List.length A.map)

module E = Eval.Make (Leduc)

let random_abstraction =
  QCheck2.Gen.(
    let* k1 = int_range 1 3 and* k2 = int_range 1 5 in
    let* r1 = array_size (return 3) (int_bound (k1 - 1))
    and* r2 = array_size (return 9) (int_bound (k2 - 1))
    and* recall = bool
    and* seed = int in
    return (r1, r2, recall, seed))

let value_is_preserved (r1, r2, recall, seed) =
  let abstraction =
    {
      Leduc.name = "random";
      round1 = (fun c -> string_of_int r1.(c));
      round2 =
        (fun c b ->
          (if recall then string_of_int r1.(c) else "")
          ^ "."
          ^ string_of_int r2.((3 * c) + b));
    }
  in
  let module G = (val Leduc.game ~abstraction d) in
  let module A = Abstraction.Make (Leduc) (G) in
  let module EA = Eval.Make (G) in
  let rng = Random.State.make [| seed |] in
  let sigma =
    List.fold_left
      (fun s (key, actions) ->
        let w = Array.map (fun _ -> Random.State.float rng 1.) actions in
        let total = Array.fold_left ( +. ) 0. w in
        Strategy.add key ~actions ~probs:(Array.map (fun x -> x /. total) w) s)
      Strategy.empty
      (EA.infosets P0 @ EA.infosets P1)
  in
  Float.abs (EA.expected_value sigma -. E.expected_value (A.lift sigma)) < 1e-12

let value_property () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:40 ~name:"lifted value" random_abstraction
       value_is_preserved)

module Coin = struct
  type history = Root | Seen of int | Done

  let name = "coin"
  let root = Root

  let node = function
    | Root -> Game.Chance [ (0.5, Seen 0); (0.5, Seen 1) ]
    | Seen _ ->
        Decision
          { player = P0; infoset = "x"; actions = [ ("a", Done); ("b", Done) ] }
    | Done -> Terminal 0.
end

module Peek = struct
  include Coin

  let node = function
    | Coin.Seen c ->
        Game.Decision
          {
            player = P0;
            infoset = "x" ^ string_of_int c;
            actions = [ ("a", Coin.Done); ("b", Done) ];
          }
    | h -> Coin.node h
end

let refinement_rejected () =
  match
    let module A = Abstraction.Make (Coin) (Peek) in
    A.size
  with
  | _ -> Alcotest.fail "accepted a refinement"
  | exception Invalid_argument _ -> ()

let tests =
  [
    Alcotest.test_case "exact is the identity" `Quick identity;
    Alcotest.test_case "renaming commutes with CFR+" `Quick rename_commutes;
    Alcotest.test_case "blind abstraction" `Quick blind_size;
    Alcotest.test_case "lifting keeps the value" `Quick value_property;
    Alcotest.test_case "rejects refinements" `Quick refinement_rejected;
  ]
