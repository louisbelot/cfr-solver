let checkpoints n =
  let rec go p acc =
    if p >= n then acc
    else go (p * 10) (List.filter (fun k -> k < n) [ 5 * p; 2 * p; p ] @ acc)
  in
  List.rev (n :: go 1 [])

let log_checkpoints per_decade n =
  let steps =
    Float.to_int
      (Float.ceil (Float.log10 (float_of_int n) *. float_of_int per_decade))
  in
  List.init (steps + 1) (fun k ->
      Float.to_int
        (Float.round (10. ** (float_of_int k /. float_of_int per_decade))))
  |> List.filter (fun k -> k < n)
  |> List.cons n |> List.sort_uniq compare

let trim_exponent s =
  match String.index_opt s 'e' with
  | Some i when String.length s - i = 5 && s.[i + 2] = '0' ->
      String.sub s 0 (i + 2) ^ String.sub s (i + 3) 2
  | _ -> s

let sci x = trim_exponent (Printf.sprintf "%.3e" x)
let short_float x = trim_exponent (Printf.sprintf "%.6g" x)

let write_file path contents =
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc contents)

let positive =
  let parse s =
    match int_of_string_opt s with
    | Some n when n > 0 -> Ok n
    | _ -> Error (`Msg "expected a positive integer")
  in
  Cmdliner.Arg.conv (parse, Format.pp_print_int)
