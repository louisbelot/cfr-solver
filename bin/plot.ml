type series = { label : string; dashed : bool; points : (int * float) list }

let width = 760.
let height = 440.
let left = 72.
let right = 140.
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

let step span =
  List.find
    (fun s -> span / s <= 10)
    [ 1; 2; 5; 10; 20; 50; 100; 200; 500; 1000 ]

let chart ~xlog ~title ~xlabel ~ylabel series =
  if List.length series > Array.length light then
    invalid_arg "Plot: too many series";
  let series =
    List.map
      (fun s ->
        { s with points = List.filter (fun (x, y) -> x > 0 && y > 0.) s.points })
      series
  in
  let all = List.concat_map (fun s -> s.points) series in
  let fold f init g = List.fold_left (fun acc p -> f acc (g p)) init all in
  let lo = fold min max_int fst and hi = fold max 0 fst in
  let x0, x1 =
    if xlog then decades (float_of_int lo) (float_of_int hi) else (lo, hi)
  in
  let y0, y1 = decades (fold Float.min infinity snd) (fold Float.max 0. snd) in
  let x1 = max x1 (x0 + 1) and y1 = max y1 (y0 + 1) in
  let plot_w = width -. left -. right and plot_h = height -. top -. bottom in
  let sx x =
    let x = if xlog then Float.log10 x else x in
    left +. (plot_w *. (x -. float_of_int x0) /. float_of_int (x1 - x0))
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
  let ticks =
    if xlog then
      List.init
        (x1 - x0 + 1)
        (fun k -> (10. ** float_of_int (x0 + k), power (x0 + k)))
    else
      let s = step (x1 - x0) in
      let first = (x0 + s - 1) / s * s in
      List.init
        (((x1 - first) / s) + 1)
        (fun k ->
          let x = first + (k * s) in
          (float_of_int x, string_of_int x))
  in
  List.iter
    (fun (v, label) ->
      let x = sx v in
      add "<line class=\"grid\" x1=\"%.1f\" y1=\"%g\" x2=\"%.1f\" y2=\"%g\"/>" x
        top x (top +. plot_h);
      add "<text x=\"%.1f\" y=\"%g\" text-anchor=\"middle\">%s</text>" x
        (top +. plot_h +. 20.)
        label)
    ticks;
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
        | (x, y) :: _ -> Some (i, s, sx (float_of_int x), sy y))
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
  let key = left +. plot_w +. 12. in
  ignore
    (List.fold_left
       (fun prev (i, s, x, y) ->
         let ly = Float.max y (prev +. 16.) in
         add "<circle class=\"d%d\" cx=\"%.1f\" cy=\"%.1f\" r=\"4\"/>" i x y;
         add
           "<line class=\"s%d\" stroke-width=\"2\"%s x1=\"%.1f\" \
            y1=\"%.1f\"             x2=\"%.1f\" y2=\"%.1f\"/>"
           i
           (if s.dashed then " stroke-dasharray=\"4 3\"" else "")
           key ly (key +. 18.) ly;
         add "<text x=\"%.1f\" y=\"%.1f\">%s</text>" (key +. 24.) (ly +. 4.)
           s.label;
         ly)
       neg_infinity ends);
  add "</svg>\n";
  Buffer.contents b

let loglog = chart ~xlog:true
let semilogy = chart ~xlog:false

let ramp =
  [|
    "#cde2fb";
    "#b7d3f6";
    "#9ec5f4";
    "#86b6ef";
    "#6da7ec";
    "#5598e7";
    "#3987e5";
    "#2a78d6";
    "#256abf";
    "#1c5cab";
    "#184f95";
    "#104281";
    "#0d366b";
  |]

let heatmap ~title ~xlabel ~ylabel ~xs ~ys panels =
  let cw = 30. and ch = 24. and gap = 28. and l = 64. and t = 68. in
  let nx = List.length xs and ny = List.length ys in
  let pw = cw *. float_of_int nx in
  let w = l +. (float_of_int (List.length panels) *. (pw +. gap)) in
  let h = t +. (ch *. float_of_int ny) +. 96. in
  let step v x =
    let logs = Array.concat (Array.to_list v) |> Array.map Float.log10 in
    let lo = Array.fold_left Float.min infinity logs
    and hi = Array.fold_left Float.max neg_infinity logs in
    let u = if hi > lo then (hi -. Float.log10 x) /. (hi -. lo) else 1. in
    Float.to_int (Float.round (u *. float_of_int (Array.length ramp - 1)))
  in
  let b = Buffer.create 32768 in
  let add fmt = Printf.bprintf b fmt in
  add
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %g %g\" \
     width=\"%g\" height=\"%g\">%s"
    w h w h style;
  add "<rect class=\"bg\" width=\"%g\" height=\"%g\"/>" w h;
  add "<text class=\"title\" x=\"%g\" y=\"26\">%s</text>" l title;
  add
    "<text transform=\"translate(18 %g) rotate(-90)\" \
     text-anchor=\"middle\">%s</text>"
    (t +. (ch *. float_of_int ny /. 2.))
    ylabel;
  List.iteri
    (fun j y ->
      add "<text x=\"%g\" y=\"%g\" text-anchor=\"end\">%s</text>" (l -. 8.)
        (t +. (ch *. float_of_int j) +. (ch /. 2.) +. 4.)
        y)
    ys;
  List.iteri
    (fun p (name, v) ->
      let x0 = l +. (float_of_int p *. (pw +. gap)) in
      add "<text x=\"%g\" y=\"%g\">%s</text>" x0 (t -. 10.) name;
      let best = ref (0, 0) in
      Array.iteri
        (fun j row ->
          Array.iteri
            (fun i e ->
              let bj, bi = !best in
              if e < v.(bj).(bi) then best := (j, i);
              let k = step v e in
              let x = x0 +. (cw *. float_of_int i)
              and y = t +. (ch *. float_of_int j) in
              add
                "<rect x=\"%.1f\" y=\"%.1f\" width=\"%g\" height=\"%g\" \
                 rx=\"2\" fill=\"%s\"><title>%s, %s %s, %s %s: \
                 %.3g</title></rect>"
                (x +. 1.) (y +. 1.) (cw -. 2.) (ch -. 2.) ramp.(k) name ylabel
                (List.nth ys j) xlabel (List.nth xs i) e;
              add
                "<text x=\"%.1f\" y=\"%.1f\" text-anchor=\"middle\" \
                 font-size=\"10\" style=\"fill:%s\">%.1f</text>"
                (x +. (cw /. 2.))
                (y +. (ch /. 2.) +. 3.5)
                (if k >= 6 then "#ffffff" else "#0b0b0b")
                (Float.log10 e))
            row)
        v;
      let bj, bi = !best in
      add
        "<rect x=\"%.1f\" y=\"%.1f\" width=\"%g\" height=\"%g\" rx=\"3\" \
         fill=\"none\" stroke=\"#eb6834\" stroke-width=\"2.5\"/>"
        (x0 +. (cw *. float_of_int bi))
        (t +. (ch *. float_of_int bj))
        cw ch;
      List.iteri
        (fun i x ->
          add "<text x=\"%.1f\" y=\"%g\" text-anchor=\"middle\">%s</text>"
            (x0 +. (cw *. float_of_int i) +. (cw /. 2.))
            (t +. (ch *. float_of_int ny) +. 16.)
            x)
        xs;
      add "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\">%s</text>"
        (x0 +. (pw /. 2.))
        (t +. (ch *. float_of_int ny) +. 34.)
        xlabel)
    panels;
  let ly = t +. (ch *. float_of_int ny) +. 58. in
  let n = Array.length ramp in
  Array.iteri
    (fun k c ->
      add "<rect x=\"%g\" y=\"%g\" width=\"16\" height=\"12\" fill=\"%s\"/>"
        (l +. (16. *. float_of_int k))
        ly c)
    ramp;
  add
    "<text x=\"%g\" y=\"%g\">cells show log10 exploitability, shaded from the \
     worst (light) to the best (dark) cell of each panel, best ringed</text>"
    (l +. (16. *. float_of_int n) +. 10.)
    (ly +. 10.);
  add "</svg>\n";
  Buffer.contents b

type curve = {
  name : string;
  color : int;
  dash : bool;
  xy : (float * float) list;
}

let nice span =
  List.find
    (fun s -> span /. s <= 8.)
    [ 0.05; 0.1; 0.2; 0.25; 0.5; 1.; 2.; 5.; 10.; 20. ]

let lines ~title ~xlabel ~ylabel ~note curves =
  let all = List.concat_map (fun c -> c.xy) curves in
  let xmax = List.fold_left (fun m (x, _) -> Float.max m x) 0. all
  and ymax = List.fold_left (fun m (_, y) -> Float.max m y) 0. all in
  let ys = nice ymax and xs = nice xmax in
  let ymax = ys *. Float.ceil (ymax /. ys)
  and xmax = xs *. Float.ceil (xmax /. xs) in
  let plot_w = width -. left -. right and plot_h = height -. top -. bottom in
  let sx x = left +. (plot_w *. x /. xmax)
  and sy y = top +. plot_h -. (plot_h *. y /. ymax) in
  let b = Buffer.create 16384 in
  let add fmt = Printf.bprintf b fmt in
  add
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %g %g\" \
     width=\"%g\" height=\"%g\">%s"
    width height width height style;
  add "<rect class=\"bg\" width=\"%g\" height=\"%g\"/>" width height;
  add "<text class=\"title\" x=\"%g\" y=\"22\">%s</text>" left title;
  add "<text x=\"%g\" y=\"40\" font-size=\"11\">%s</text>" left note;
  let ticks span s =
    List.init
      (Float.to_int (Float.round (span /. s)) + 1)
      (fun k -> s *. float_of_int k)
  in
  List.iter
    (fun x ->
      add "<line class=\"grid\" x1=\"%.1f\" y1=\"%g\" x2=\"%.1f\" y2=\"%g\"/>"
        (sx x) top (sx x) (top +. plot_h);
      add "<text x=\"%.1f\" y=\"%g\" text-anchor=\"middle\">%g</text>" (sx x)
        (top +. plot_h +. 20.)
        x)
    (ticks xmax xs);
  List.iter
    (fun y ->
      add "<line class=\"grid\" x1=\"%g\" y1=\"%.1f\" x2=\"%g\" y2=\"%.1f\"/>"
        left (sy y) (left +. plot_w) (sy y);
      add "<text x=\"%g\" y=\"%.1f\" text-anchor=\"end\">%g</text>" (left -. 8.)
        (sy y +. 4.)
        y)
    (ticks ymax ys);
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
  List.iter
    (fun c ->
      add
        "<polyline class=\"s%d\" fill=\"none\" stroke-width=\"%s\" \
         stroke-linejoin=\"round\"%s points=\""
        c.color
        (if c.dash then "1.5" else "2")
        (if c.dash then " stroke-dasharray=\"5 4\"" else "");
      List.iter (fun (x, y) -> add "%.1f,%.1f " (sx x) (sy y)) c.xy;
      add "\"/>")
    curves;
  let key = left +. plot_w +. 20. in
  curves
  |> List.filter (fun c -> not c.dash)
  |> List.filter_map (fun c ->
      match List.rev c.xy with [] -> None | (_, y) :: _ -> Some (c, sy y))
  |> List.sort (fun (_, a) (_, b) -> compare a b)
  |> List.fold_left
       (fun prev (c, y) ->
         let ly = Float.max y (prev +. 16.) in
         add
           "<line class=\"s%d\" stroke-width=\"2\" x1=\"%.1f\" y1=\"%.1f\" \
            x2=\"%.1f\" y2=\"%.1f\"/>"
           c.color key ly (key +. 18.) ly;
         add "<text x=\"%.1f\" y=\"%.1f\">%s</text>" (key +. 24.) (ly +. 4.)
           c.name;
         ly)
       neg_infinity
  |> ignore;
  add "</svg>\n";
  Buffer.contents b
