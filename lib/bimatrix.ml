type history = int list

let game name payoff =
  let rows = Array.length payoff and cols = Array.length payoff.(0) in
  (module struct
    type nonrec history = history

    let name = name
    let players = 2
    let root = []

    let node = function
      | [] ->
          Multi.Decision
            {
              player = 0;
              infoset = "row";
              actions = List.init rows (fun a -> (string_of_int a, [ a ]));
            }
      | [ a ] ->
          Decision
            {
              player = 1;
              infoset = "col";
              actions = List.init cols (fun b -> (string_of_int b, [ a; b ]));
            }
      | a :: b :: _ -> Terminal payoff.(a).(b)
  end : Multi.S)

let of_pair a b =
  Array.mapi (fun i r -> Array.mapi (fun j x -> [| x; b.(i).(j) |]) r) a

let chicken =
  game "chicken"
    (of_pair [| [| 0.; 7. |]; [| 2.; 6. |] |] [| [| 0.; 2. |]; [| 7.; 6. |] |])

let shapley =
  game "shapley"
    (of_pair
       [| [| 0.; 1.; 0. |]; [| 0.; 0.; 1. |]; [| 1.; 0.; 0. |] |]
       [| [| 0.; 0.; 1. |]; [| 1.; 0.; 0. |]; [| 0.; 1.; 0. |] |])
