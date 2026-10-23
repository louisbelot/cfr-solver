open Cfr_solver

let phases () =
  let d = 4 and rounds = 500 in
  let b = Barrier.create d
  and arrived = Atomic.make 0
  and early = Atomic.make 0 in
  let go () =
    for r = 1 to rounds do
      Atomic.incr arrived;
      Barrier.await b;
      let n = Atomic.get arrived in
      if n < r * d || n > (r + 1) * d then Atomic.incr early
    done
  in
  let others = List.init (d - 1) (fun _ -> Domain.spawn go) in
  go ();
  List.iter Domain.join others;
  Alcotest.(check int) "arrivals" (rounds * d) (Atomic.get arrived);
  Alcotest.(check int) "out of phase" 0 (Atomic.get early)

let single () =
  let b = Barrier.create 1 in
  Barrier.await b;
  Barrier.await b

let rejects () =
  Alcotest.check_raises "zero parties"
    (Invalid_argument "Barrier.create: parties must be positive") (fun () ->
      ignore (Barrier.create 0))

let tests =
  [
    Alcotest.test_case "keeps domains in phase" `Quick phases;
    Alcotest.test_case "one party" `Quick single;
    Alcotest.test_case "parties" `Quick rejects;
  ]
