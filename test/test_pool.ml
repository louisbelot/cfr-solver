open Cfr_solver

let once_each () =
  let p = Pool.create () in
  List.iter
    (fun d ->
      let hits = Array.init d (fun _ -> Atomic.make 0) in
      Pool.run p d (fun j -> Atomic.incr hits.(j));
      Array.iteri
        (fun j h ->
          Alcotest.(check int) (Printf.sprintf "d=%d j=%d" d j) 1 (Atomic.get h))
        hits)
    [ 3; 1; 5; 2; 6; 6; 4 ];
  Alcotest.(check int) "workers" 5 (Pool.workers p);
  Pool.shutdown p

let same_domains () =
  let p = Pool.create () in
  let ids () =
    let seen = Array.make 4 (-1) in
    Pool.run p 4 (fun j -> seen.(j) <- (Domain.self () :> int));
    seen
  in
  let first = ids () in
  for _ = 1 to 50 do
    Alcotest.(check (array int)) "placement" first (ids ())
  done;
  Alcotest.(check int) "caller runs 0" (Domain.self () :> int) first.(0);
  Pool.shutdown p

let many_rounds () =
  let p = Pool.create () and total = Atomic.make 0 in
  for _ = 1 to 2000 do
    Pool.run p 4 (fun j -> ignore (Atomic.fetch_and_add total (j + 1)))
  done;
  Alcotest.(check int) "sum" (2000 * 10) (Atomic.get total);
  Pool.shutdown p

exception Boom of int

let failures () =
  let p = Pool.create () in
  Alcotest.check_raises "worker" (Boom 2) (fun () ->
      Pool.run p 3 (fun j -> if j = 2 then raise (Boom j)));
  Alcotest.check_raises "caller" (Boom 0) (fun () ->
      Pool.run p 3 (fun j -> if j = 0 then raise (Boom j)));
  let hits = Atomic.make 0 in
  Pool.run p 3 (fun _ -> Atomic.incr hits);
  Alcotest.(check int) "usable after" 3 (Atomic.get hits);
  Pool.shutdown p

let rejects () =
  let p = Pool.create () in
  Alcotest.check_raises "zero"
    (Invalid_argument "Pool.run: domains must be positive") (fun () ->
      Pool.run p 0 ignore);
  Pool.run p 2 ignore;
  Pool.shutdown p;
  Alcotest.(check int) "joined" 0 (Pool.workers p);
  Alcotest.check_raises "closed"
    (Invalid_argument "Pool.run: pool is shut down") (fun () ->
      Pool.run p 2 ignore)

let tests =
  [
    Alcotest.test_case "runs every index once" `Quick once_each;
    Alcotest.test_case "same domain per index" `Quick same_domains;
    Alcotest.test_case "many rounds" `Quick many_rounds;
    Alcotest.test_case "failures propagate" `Quick failures;
    Alcotest.test_case "arguments and shutdown" `Quick rejects;
  ]
