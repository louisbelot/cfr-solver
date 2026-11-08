open Cfr_solver

let prop ~count name gen f =
  Alcotest.test_case name `Quick (fun () ->
      QCheck2.Test.check_exn (QCheck2.Test.make ~count ~name gen f))

module Z = Bigint

let frac =
  QCheck2.Gen.(
    map2
      (fun n d -> Rat.of_ints n d)
      (int_range (-1000) 1000) (int_range 1 1000))

let rat = Alcotest.testable Rat.pp Rat.equal

let normalized (q : Rat.t) =
  Z.sign q.den > 0 && Z.equal (Z.gcd q.num q.den) Z.one

let naive_add (a : Rat.t) (b : Rat.t) =
  Rat.make (Z.add (Z.mul a.num b.den) (Z.mul b.num a.den)) (Z.mul a.den b.den)

let naive_mul (a : Rat.t) (b : Rat.t) =
  Rat.make (Z.mul a.num b.num) (Z.mul a.den b.den)

let field () =
  prop ~count:2000 "field laws"
    QCheck2.Gen.(triple frac frac frac)
    (fun (a, b, c) ->
      let s = Rat.add a b and p = Rat.mul a b in
      normalized s && normalized p
      && Rat.equal s (naive_add a b)
      && Rat.equal p (naive_mul a b)
      && Rat.equal (Rat.add s c) (Rat.add a (Rat.add b c))
      && Rat.equal
           (Rat.mul (Rat.add a b) c)
           (Rat.add (Rat.mul a c) (Rat.mul b c))
      && Rat.is_zero (Rat.sub a a)
      && (Rat.is_zero a || Rat.equal (Rat.div a a) Rat.one)
      && Rat.compare a b = Float.compare (Rat.to_float a) (Rat.to_float b))

let roundtrip () =
  let gen =
    QCheck2.Gen.(
      map2
        (fun m e -> Float.ldexp m e)
        (float_range (-1.) 1.) (int_range (-1060) 1020))
  in
  prop ~count:2000 "float roundtrip" gen (fun x ->
      Float.equal (Rat.to_float (Rat.of_float x)) x)

let exact () =
  Alcotest.check rat "0.1"
    (Rat.make (Z.of_string "3602879701896397") (Z.shift_left Z.one 55))
    (Rat.of_float 0.1);
  Alcotest.check rat "-6" (Rat.of_int (-6)) (Rat.of_float (-6.));
  Alcotest.check rat "2^70"
    (Rat.make (Z.shift_left Z.one 70) Z.one)
    (Rat.of_float (Float.ldexp 1. 70));
  Alcotest.check rat "sign" (Rat.of_ints 1 (-2)) (Rat.of_ints (-3) 6);
  Alcotest.(check (float 0.)) "tiny" 5e-324 (Rat.to_float (Rat.of_float 5e-324));
  Alcotest.check_raises "zero" Division_by_zero (fun () ->
      ignore (Rat.inv Rat.zero))

let recover () =
  List.iter
    (fun (n, d) ->
      Alcotest.check rat
        (Printf.sprintf "%d/%d" n d)
        (Rat.of_ints n d)
        (Rat.recover (Float.of_int n /. Float.of_int d)))
    [ (1, 3); (2, 7); (-5, 12); (1, 6); (4, 1); (0, 1); (1, 120); (13, 1326) ];
  Alcotest.check_raises "pi" (Invalid_argument "Rat.recover: no small fraction")
    (fun () -> ignore (Rat.recover ~max_den:1000 Float.pi))

let tests =
  [
    field ();
    roundtrip ();
    Alcotest.test_case "exact floats" `Quick exact;
    Alcotest.test_case "recover" `Quick recover;
  ]
