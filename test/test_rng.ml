open Cfr_solver

let draws g n = List.init n (fun _ -> Rng.bits g)

let reference () =
  let g = Rng.make 1234567 in
  Alcotest.(check (list string))
    "splitmix64"
    [
      "6457827717110365317";
      "3203168211198807973";
      "9817491932198370423";
      "4593380528125082431";
      "16408922859458223821";
    ]
    (List.init 5 (fun _ -> Printf.sprintf "%Lu" (Rng.next64 g)))

let top_bits () =
  Alcotest.(check (list int))
    "seed 1"
    [ 608340859; 800777064; 1042606267; 477127076; 477025590 ]
    (draws (Rng.make 1) 5)

let copy_and_split () =
  let g = Rng.make 9 in
  let c = Rng.copy g in
  Alcotest.(check (list int)) "copy" (draws g 20) (draws c 20);
  let s = Rng.split g in
  Alcotest.(check bool) "split" false (draws g 20 = draws s 20)

let int_is_uniform () =
  let g = Rng.make 3 and counts = Array.make 3 0 in
  for _ = 1 to 300_000 do
    let k = Rng.int g 3 in
    counts.(k) <- counts.(k) + 1
  done;
  Array.iteri
    (fun k c ->
      if abs (c - 100_000) > 1_000 then Alcotest.failf "bucket %d: %d" k c)
    counts

let tests =
  [
    Alcotest.test_case "published splitmix64 outputs" `Quick reference;
    Alcotest.test_case "bits are the top 30" `Quick top_bits;
    Alcotest.test_case "copy and split" `Quick copy_and_split;
    Alcotest.test_case "int is uniform" `Quick int_is_uniform;
  ]
