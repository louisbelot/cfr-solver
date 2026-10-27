type perm = int array

let identity = [| 0; 1; 2; 3 |]

let all =
  let rec perms = function
    | [] -> [ [] ]
    | xs ->
        List.concat_map
          (fun x -> List.map (List.cons x) (perms (List.filter (( <> ) x) xs)))
          xs
  in
  Array.of_list (List.map Array.of_list (perms [ 0; 1; 2; 3 ]))

let apply p c = Hand.card (Hand.rank c) p.(Hand.suit c)
let compose p q = Array.map (fun s -> p.(s)) q

let inverse p =
  let q = Array.make 4 0 in
  Array.iteri (fun s x -> q.(x) <- s) p;
  q

let combo p (a, b) =
  let a = apply p a and b = apply p b in
  if a > b then (a, b) else (b, a)

let fixes p cards =
  let sorted l = List.sort compare l in
  sorted (List.map (apply p) cards) = sorted cards

let stabilizer group cards = List.filter (fun p -> fixes p cards) group

let orbits group cards =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun c ->
      if Hashtbl.mem seen c then None
      else begin
        let members =
          List.fold_left
            (fun acc p ->
              let d = apply p c in
              if Hashtbl.mem seen d then acc
              else begin
                Hashtbl.add seen d ();
                (d, p) :: acc
              end)
            [] group
        in
        Some (c, List.rev members)
      end)
    (List.sort compare cards)
