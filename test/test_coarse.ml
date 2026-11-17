open Cfr_solver

let kuhn2 = Multi_tree.build (module (val Kuhn_n.game 2))
let kuhn3 = Multi_tree.build (module (val Kuhn_n.game 3))

let close ?(eps = 1e-12) what a b =
  if Float.abs (a -. b) > eps then Alcotest.failf "%s: %.15g vs %.15g" what a b

let same what a b =
  Array.iteri
    (fun s v -> close ~eps:1e-10 (Printf.sprintf "%s %d" what s) v b.(s))
    a

module Noisy = struct
  include (val Kuhn_n.game 3)

  let node h =
    match node h with
    | Multi.Terminal u ->
        Multi.Terminal
          (Array.mapi
             (fun i v ->
               v +. (float_of_int (Hashtbl.hash (h, i) mod 1000) /. 1e4))
             u)
    | n -> n
end

let matches_cfr () =
  let module C = Multi_cfr.Make (Noisy) in
  let tree = Multi_tree.build (module Noisy) in
  let c = C.create () and t = Coarse.create tree in
  for i = 1 to 200 do
    C.iterate c;
    Coarse.iterate t;
    if i mod 20 = 0 then begin
      same "current" (Coarse.current t)
        (Multi_br.probs tree (C.current_strategy c));
      same "average" (Coarse.average t)
        (Multi_br.probs tree (C.average_strategy c))
    end
  done

let sample_is_pure () =
  let tp = Treeplex.build kuhn3 in
  let rng = Rng.make 1 in
  let flat =
    Coarse.(
      current
        (let t = create kuhn3 in
         run t 7;
         t))
  in
  let n = 20000 in
  let freq = Array.make (Array.length flat) 0. in
  for _ = 1 to n do
    let p = Treeplex.sample tp rng flat in
    Array.iteri
      (fun k _ ->
        let ones = ref 0 in
        for s = kuhn3.offset.(k) to kuhn3.offset.(k + 1) - 1 do
          if p.(s) = 1. then incr ones
          else if p.(s) <> 0. then Alcotest.fail "mixed"
        done;
        if !ones <> 1 then Alcotest.fail "not one action")
      kuhn3.keys;
    Array.iteri (fun s v -> freq.(s) <- freq.(s) +. (v /. float_of_int n)) p
  done;
  Array.iteri
    (fun s f -> close ~eps:0.015 (Printf.sprintf "slot %d" s) flat.(s) f)
    freq

let cfr_s () =
  let t = Coarse.create ~rng:(Rng.make 2) kuhn3 in
  Coarse.run t 500;
  let early = Correlated.gap ~concept:Nfcce (Coarse.mixture t) in
  Coarse.run t 19500;
  let late = Correlated.gap ~concept:Nfcce (Coarse.mixture t) in
  if not (late < early /. 3. && late < 0.02) then
    Alcotest.failf "nfcce gap %g -> %g" early late

let expected_play () =
  let t = Coarse.create kuhn3 in
  Coarse.run t 250;
  let early = Correlated.gap ~concept:Nfcce (Coarse.mixture t) in
  Coarse.run t 3750;
  let late = Correlated.gap ~concept:Nfcce (Coarse.mixture t) in
  if not (late < early /. 4.) then
    Alcotest.failf "nfcce gap %g -> %g" early late

let sampled_trigger () =
  let t = Efce.create ~plus:true ~rng:(Rng.make 4) kuhn2 in
  Efce.run t 500;
  let early = Correlated.gap (Efce.mixture t) in
  Efce.run t 19500;
  let late = Correlated.gap (Efce.mixture t) in
  if not (late < early /. 3. && late < 0.02) then
    Alcotest.failf "efce gap %g -> %g" early late

let tests =
  [
    Alcotest.test_case "expected play is cfr" `Quick matches_cfr;
    Alcotest.test_case "samples are pure" `Quick sample_is_pure;
    Alcotest.test_case "cfr-s reaches nfcce" `Quick cfr_s;
    Alcotest.test_case "cfr nfcce gap shrinks" `Quick expected_play;
    Alcotest.test_case "sampled trigger dynamics" `Quick sampled_trigger;
  ]
