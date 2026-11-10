open Cfr_solver

let keeps_prefix () =
  let r = Reservoir.create ~capacity:5 (Rng.make 1) in
  List.iter (Reservoir.add r) [ 1; 2; 3 ];
  Alcotest.(check (list int)) "all kept" [ 1; 2; 3 ] (Reservoir.to_list r);
  List.iter (Reservoir.add r) [ 4; 5; 6; 7 ];
  Alcotest.(check int) "length" 5 (Reservoir.length r);
  Alcotest.(check int) "seen" 7 (Reservoir.seen r)

let uniform_inclusion () =
  let runs = 20_000 and n = 100 and capacity = 10 in
  let hits = Array.make n 0 and rng = Rng.make 9 in
  for _ = 1 to runs do
    let r = Reservoir.create ~capacity (Rng.split rng) in
    for i = 0 to n - 1 do
      Reservoir.add r i
    done;
    List.iter (fun i -> hits.(i) <- hits.(i) + 1) (Reservoir.to_list r)
  done;
  Array.iteri
    (fun i h ->
      let p = float_of_int h /. float_of_int runs in
      if Float.abs (p -. 0.1) > 0.01 then Alcotest.failf "item %d kept %.4f" i p)
    hits

let tests =
  [
    Alcotest.test_case "keeps the first items" `Quick keeps_prefix;
    Alcotest.test_case "uniform inclusion" `Quick uniform_inclusion;
  ]
