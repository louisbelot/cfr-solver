type series = { label : string; dashed : bool; points : (int * float) list }

let width = 760.
let height = 440.
let left = 72.
let right = 128.
let top = 48.
let bottom = 56.

let light =
  [| "#2a78d6"; "#eb6834"; "#1baf7a"; "#eda100"; "#e87ba4"; "#008300" |]

let dark =
  [| "#3987e5"; "#d95926"; "#199e70"; "#c98500"; "#d55181"; "#008300" |]

let style =
  let rules colors =
    String.concat ""
      (List.init (Array.length colors) (fun i ->
           Printf.sprintf ".s%d{stroke:%s}.d%d{fill:%s}" i colors.(i) i
             colors.(i)))
  in
  Printf.sprintf
    "<style>text{font:12px \
     system-ui,sans-serif;fill:#52514e}.title{font-size:15px;fill:#0b0b0b}.bg{fill:#fcfcfb}.grid{stroke:#e4e3df}.axis{stroke:#8a8984}%s@media \
     (prefers-color-scheme:dark){text{fill:#c3c2b7}.title{fill:#f0efec}.bg{fill:#1a1a19}.grid{stroke:#33332f}.axis{stroke:#6b6a65}%s}</style>"
    (rules light) (rules dark)

let decades lo hi =
  ( Float.to_int (Float.floor (Float.log10 lo)),
    Float.to_int (Float.ceil (Float.log10 hi)) )

let power k = Printf.sprintf "10<tspan dy=\"-6\" font-size=\"9\">%d</tspan>" k

let loglog ~title ~xlabel ~ylabel series =
  if List.length series > Array.length light then
    invalid_arg "Plot.loglog: too many series";
  let series =
    List.map
      (fun s ->
        { s with points = List.filter (fun (x, y) -> x > 0 && y > 0.) s.points })
      series
  in
  let all = List.concat_map (fun s -> s.points) series in
  let fold f init g = List.fold_left (fun acc p -> f acc (g p)) init all in
  let x0, x1 =
    decades
      (fold Float.min infinity (fun (x, _) -> float_of_int x))
      (fold Float.max 0. (fun (x, _) -> float_of_int x))
  in
  let y0, y1 = decades (fold Float.min infinity snd) (fold Float.max 0. snd) in
  let x1 = max x1 (x0 + 1) and y1 = max y1 (y0 + 1) in
  let plot_w = width -. left -. right and plot_h = height -. top -. bottom in
  let sx x =
    left
    +. (plot_w *. (Float.log10 x -. float_of_int x0) /. float_of_int (x1 - x0))
  in
  let sy y =
    top +. plot_h
    -. (plot_h *. (Float.log10 y -. float_of_int y0) /. float_of_int (y1 - y0))
  in
  let b = Buffer.create 8192 in
  let add fmt = Printf.bprintf b fmt in
  add
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %g %g\" \
     width=\"%g\" height=\"%g\">%s"
    width height width height style;
  add "<rect class=\"bg\" width=\"%g\" height=\"%g\"/>" width height;
  add "<text class=\"title\" x=\"%g\" y=\"28\">%s</text>" left title;
  for k = x0 to x1 do
    let x = sx (10. ** float_of_int k) in
    add "<line class=\"grid\" x1=\"%.1f\" y1=\"%g\" x2=\"%.1f\" y2=\"%g\"/>" x
      top x (top +. plot_h);
    add "<text x=\"%.1f\" y=\"%g\" text-anchor=\"middle\">%s</text>" x
      (top +. plot_h +. 20.)
      (power k)
  done;
  for k = y0 to y1 do
    let y = sy (10. ** float_of_int k) in
    add "<line class=\"grid\" x1=\"%g\" y1=\"%.1f\" x2=\"%g\" y2=\"%.1f\"/>"
      left y (left +. plot_w) y;
    add "<text x=\"%g\" y=\"%.1f\" text-anchor=\"end\">%s</text>" (left -. 8.)
      (y +. 4.) (power k)
  done;
  add "<line class=\"axis\" x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\"/>" left
    (top +. plot_h) (left +. plot_w) (top +. plot_h);
  add "<line class=\"axis\" x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\"/>" left top
    left (top +. plot_h);
  add "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\">%s</text>"
    (left +. (plot_w /. 2.))
    (height -. 12.) xlabel;
  add
    "<text transform=\"translate(18 %g) rotate(-90)\" \
     text-anchor=\"middle\">%s</text>"
    (top +. (plot_h /. 2.))
    ylabel;
  let ends =
    List.mapi
      (fun i s ->
        match List.rev s.points with
        | [] -> None
        | (x, y) :: _ -> Some (i, s.label, sx (float_of_int x), sy y))
      series
    |> List.filter_map Fun.id
    |> List.sort (fun (_, _, _, a) (_, _, _, b) -> compare a b)
  in
  List.iteri
    (fun i s ->
      add
        "<polyline class=\"s%d\" fill=\"none\" stroke-width=\"2\" \
         stroke-linejoin=\"round\"%s points=\""
        i
        (if s.dashed then " stroke-dasharray=\"6 4\"" else "");
      List.iter
        (fun (x, y) -> add "%.1f,%.1f " (sx (float_of_int x)) (sy y))
        s.points;
      add "\"/>")
    series;
  ignore
    (List.fold_left
       (fun prev (i, label, x, y) ->
         let ly = Float.max y (prev +. 15.) in
         add "<circle class=\"d%d\" cx=\"%.1f\" cy=\"%.1f\" r=\"4\"/>" i x y;
         add "<text x=\"%.1f\" y=\"%.1f\">%s</text>" (x +. 10.) (ly +. 4.) label;
         ly)
       neg_infinity ends);
  add "</svg>\n";
  Buffer.contents b
