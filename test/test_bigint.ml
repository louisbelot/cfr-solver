open Cfr_solver

let prop ~count name gen f =
  Alcotest.test_case name `Quick (fun () ->
      QCheck2.Test.check_exn (QCheck2.Test.make ~count ~name gen f))

module Tiny = Bigint.Make (struct
  let bits = 3
end)

module Z = Bigint

let small = QCheck2.Gen.int_range (-(1 lsl 28)) (1 lsl 28)

let digits =
  QCheck2.Gen.(
    map2
      (fun neg s ->
        (if neg then "-" else "")
        ^ string_of_int (1 + (Char.code s.[0] mod 9))
        ^ s)
      bool
      (string_size ~gen:(char_range '0' '9') (int_range 1 60)))

let natives () =
  let gen = QCheck2.Gen.(pair small small) in
  prop ~count:2000 "matches int" gen (fun (a, b) ->
      let za = Z.of_int a and zb = Z.of_int b in
      let ok op x = Z.to_string op = string_of_int x in
      ok (Z.add za zb) (a + b)
      && ok (Z.sub za zb) (a - b)
      && ok (Z.mul za zb) (a * b)
      && Z.compare za zb = compare a b
      && Tiny.to_string (Tiny.mul (Tiny.of_int a) (Tiny.of_int b))
         = string_of_int (a * b)
      && (b = 0
         ||
         let q, r = Z.div_rem za zb
         and q', r' = Tiny.div_rem (Tiny.of_int a) (Tiny.of_int b) in
         ok q (a / b)
         && ok r (a mod b)
         && Tiny.to_string q' = string_of_int (a / b)
         && Tiny.to_string r' = string_of_int (a mod b)))

let division (type a) (module M : Bigint.S with type t = a) name =
  prop ~count:1000 name
    QCheck2.Gen.(pair digits digits)
    (fun (a, b) ->
      let a = M.of_string a and b = M.of_string b in
      let q, r = M.div_rem a b in
      M.equal a (M.add (M.mul q b) r)
      && M.compare (M.abs r) (M.abs b) < 0
      && (M.sign r = 0 || M.sign r = M.sign a))

let limbs_agree () =
  prop ~count:1000 "limb widths agree"
    QCheck2.Gen.(pair digits digits)
    (fun (a, b) ->
      let za = Z.of_string a and zb = Z.of_string b in
      let ta = Tiny.of_string a and tb = Tiny.of_string b in
      Z.to_string (Z.mul za zb) = Tiny.to_string (Tiny.mul ta tb)
      && Z.to_string (Z.div za zb) = Tiny.to_string (Tiny.div ta tb)
      && Z.to_string (Z.gcd za zb) = Tiny.to_string (Tiny.gcd ta tb)
      && Z.to_string za = a
      && Tiny.to_string ta = a)

let gcd () =
  prop ~count:500 "gcd"
    QCheck2.Gen.(triple digits digits digits)
    (fun (a, b, c) ->
      let a = Z.of_string a
      and b = Z.of_string b
      and c = Z.abs (Z.of_string c) in
      let g = Z.gcd (Z.mul a c) (Z.mul b c) in
      Z.sign (Z.rem g c) = 0
      && Z.equal (Z.gcd (Z.div (Z.mul a c) g) (Z.div (Z.mul b c) g)) Z.one)

let known () =
  let fact n =
    List.fold_left
      (fun acc k -> Z.mul acc (Z.of_int k))
      Z.one (List.init n succ)
  in
  Alcotest.(check string)
    "30!" "265252859812191058636308480000000"
    (Z.to_string (fact 30));
  Alcotest.(check string)
    "2^100" "1267650600228229401496703205376"
    (Z.to_string (Z.shift_left Z.one 100));
  Alcotest.(check string)
    "2^80 float" "1208925819614629174706176"
    (Z.to_string (Z.of_float (Float.ldexp 1. 80)));
  Alcotest.(check string)
    "min_int" (string_of_int min_int)
    (Z.to_string (Z.of_int min_int));
  Alcotest.(check int) "numbits" 101 (Z.numbits (Z.shift_left Z.one 100));
  Alcotest.(check string)
    "shift right" "-9"
    (Z.to_string (Z.shift_right (Z.of_int (-37)) 2));
  let f = Z.to_float (Z.of_string "1000000000000000000000000000000") in
  Alcotest.(check bool) "to_float" true (Float.abs (f -. 1e30) < 1e15);
  Alcotest.check_raises "zero" Division_by_zero (fun () ->
      ignore (Z.div Z.one Z.zero))

let tests =
  [
    natives ();
    division (module Z) "division";
    division (module Tiny) "division, 3-bit limbs";
    limbs_agree ();
    gcd ();
    Alcotest.test_case "known values" `Quick known;
  ]
