"use strict";

const ALGORITHMS = ["cfr", "cfr+", "lcfr", "dcfr", "dcfr+", "pcfr+", "pdcfr+"];
const LABELS = {
  cfr: "CFR", "cfr+": "CFR+", lcfr: "Linear CFR", dcfr: "DCFR",
  "dcfr+": "DCFR+", "pcfr+": "PCFR+", "pdcfr+": "PDCFR+",
};

const GAMES = {
  kuhn: {
    iterations: 10000, last: true,
    notation: "Kuhn poker: three cards, one bet. An infoset is your card (J, Q, K) and the moves so far, p for check or fold and b for bet or call. Payoffs are in antes.",
  },
  leduc: {
    iterations: 2000, last: true,
    notation: "Leduc hold'em: six cards, a private card and one board card, two betting rounds. Keys read private card, board card after it once dealt, a colon, then the betting with rounds split by /; c is check or call, r raise, f fold.",
  },
  "liars-1d4": {
    iterations: 2000, last: true,
    notation: "Liar's dice with one four-sided die each, where a 4 counts as any face. A key is your roll, a colon, then the bids so far; 2x3 means at least two dice show 3. Calling liar ends the game.",
  },
  liars: {
    iterations: 200, last: false,
    notation: "Liar's dice with one six-sided die each, where a 6 counts as any face: 24,576 infosets and about 295,000 nodes per pass, so it runs much slower than the others. A key is your roll, a colon, then the bids so far; 2x3 means at least two dice show 3.",
  },
  "goofspiel-4": {
    iterations: 2000, last: true,
    notation: "Goofspiel with four cards: both players bid a card for the revealed prize and the higher bid takes it. A key is seat | prizes so far | your bids | your results (W, L, T). Payoff is win or loss.",
  },
  goofspiel: {
    iterations: 300, last: true,
    notation: "Goofspiel with five cards: both players bid a card for the revealed prize and the higher bid takes it. A key is seat | prizes so far | your bids | your results (W, L, T). Payoff is win or loss.",
  },
};

const $ = (id) => document.getElementById(id);
const css = (name) => getComputedStyle(document.documentElement).getPropertyValue(name).trim();
const SVG = "http://www.w3.org/2000/svg";

const worker = new Worker("worker.bc.js");
const waiting = [];
worker.onmessage = (e) => waiting.shift()(JSON.parse(e.data));
worker.onerror = (e) => {
  $("table-note").textContent = "The solver stopped with an error: " + e.message;
};
const call = (msg) => new Promise((resolve) => { waiting.push(resolve); worker.postMessage(msg); });

const state = {
  runs: [],
  selected: null,
  key: null,
  table: null,
  vocabulary: [],
  stopping: false,
  score: { hands: 0, total: 0 },
  running: false,
  nextId: 1,
};

function seriesColor(algorithm) {
  return css(`--series-${ALGORITHMS.indexOf(algorithm) + 1}`);
}

function mixHex(a, b, t) {
  const p = (h) => [1, 3, 5].map((i) => parseInt(h.slice(i, i + 2), 16));
  const [x, y] = [p(a), p(b)];
  return "#" + x.map((v, i) => Math.round(v + (y[i] - v) * t).toString(16).padStart(2, "0")).join("");
}

function actionColor(name) {
  const v = state.vocabulary;
  const i = v.indexOf(name);
  if (v.length <= 7) return css(`--series-${i + 1}`);
  return mixHex(css("--seq-light"), css("--seq-dark"), v.length === 1 ? 1 : i / (v.length - 1));
}

const fmt = (x) => {
  if (x === 0) return "0";
  const a = Math.abs(x);
  if (a >= 1000 || a < 0.001) return x.toExponential(2);
  return x.toPrecision(3);
};
const fmtInt = (n) => n.toLocaleString("en-US");

function el(tag, attrs = {}, parent) {
  const node = document.createElementNS(SVG, tag);
  for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
  if (parent) parent.appendChild(node);
  return node;
}

function niceTicks(lo, hi, count) {
  const span = hi - lo || 1;
  const step0 = Math.pow(10, Math.floor(Math.log10(span / count)));
  const step = [1, 2, 5, 10].map((m) => m * step0).find((s) => span / s <= count) || step0 * 10;
  const ticks = [];
  for (let t = Math.ceil(lo / step) * step; t <= hi + step * 1e-9; t += step) ticks.push(+t.toPrecision(12));
  return ticks;
}

function decades(lo, hi) {
  const ticks = [];
  for (let e = Math.floor(Math.log10(lo)); e <= Math.ceil(Math.log10(hi)); e++) ticks.push(Math.pow(10, e));
  const inside = ticks.filter((t) => t >= lo / 1.0001 && t <= hi * 1.0001);
  const k = Math.ceil(inside.length / 8);
  return inside.filter((t) => Math.round(Math.log10(t)) % k === 0);
}

function plot(container, series, opts) {
  container.textContent = "";
  const width = Math.max(280, container.clientWidth);
  const height = opts.height || 280;
  const m = { top: 10, right: 16, bottom: 40, left: 62 };
  const svg = el("svg", { viewBox: `0 0 ${width} ${height}`, role: "img", "aria-label": opts.label });
  container.appendChild(svg);
  const pts = series.flatMap((s) => s.points).filter(([x, y]) => x > 0 && (!opts.logY || y > 0));
  if (!pts.length) {
    el("text", { x: width / 2, y: height / 2, "text-anchor": "middle", class: "empty" }, svg).textContent = opts.empty || "No data yet";
    return;
  }
  const xs = pts.map((p) => p[0]), ys = pts.map((p) => p[1]);
  const x0 = 1, x1 = Math.max(10, ...xs);
  let y0 = opts.yDomain ? opts.yDomain[0] : Math.min(...ys);
  let y1 = opts.yDomain ? opts.yDomain[1] : Math.max(...ys);
  if (opts.logY) {
    y0 = Math.pow(10, Math.floor(Math.log10(y0)));
    y1 = Math.pow(10, Math.ceil(Math.log10(y1)));
  } else if (!opts.yDomain) {
    if (y0 === y1) { y0 -= 1; y1 += 1; }
    const pad = (y1 - y0) * 0.05;
    y0 = Math.min(0, y0 - pad); y1 = Math.max(0, y1 + pad);
  }
  const W = width - m.left - m.right, H = height - m.top - m.bottom;
  const sx = (x) => m.left + (Math.log10(x) - Math.log10(x0)) / (Math.log10(x1) - Math.log10(x0)) * W;
  const sy = opts.logY
    ? (y) => m.top + H - (Math.log10(y) - Math.log10(y0)) / (Math.log10(y1) - Math.log10(y0)) * H
    : (y) => m.top + H - (y - y0) / (y1 - y0) * H;

  const yt = opts.logY ? decades(y0, y1) : niceTicks(y0, y1, 5);
  for (const t of yt) {
    el("line", { x1: m.left, x2: m.left + W, y1: sy(t), y2: sy(t), class: t === 0 && !opts.logY ? "baseline" : "grid" }, svg);
    el("text", { x: m.left - 8, y: sy(t) + 4, "text-anchor": "end" }, svg).textContent =
      opts.logY ? `1e${Math.round(Math.log10(t))}` : String(+t.toPrecision(3));
  }
  for (const t of decades(x0, x1)) {
    el("line", { x1: sx(t), x2: sx(t), y1: m.top, y2: m.top + H, class: "grid" }, svg);
    el("text", { x: sx(t), y: m.top + H + 16, "text-anchor": "middle" }, svg).textContent = fmtInt(t);
  }
  el("line", { x1: m.left, x2: m.left + W, y1: m.top + H, y2: m.top + H, class: "baseline" }, svg);
  el("text", { x: m.left + W / 2, y: height - 4, "text-anchor": "middle", class: "axis-label" }, svg).textContent = opts.xLabel;
  el("text", { x: 14, y: m.top + H / 2, "text-anchor": "middle", class: "axis-label", transform: `rotate(-90 14 ${m.top + H / 2})` }, svg).textContent = opts.yLabel;

  const clip = `clip-${Math.random().toString(36).slice(2)}`;
  el("rect", { x: m.left, y: m.top - 2, width: W, height: H + 4 }, el("clipPath", { id: clip }, svg));
  const lines = el("g", { "clip-path": `url(#${clip})` }, svg);
  const drawn = series.map((s) => {
    const p = s.points.filter(([x, y]) => x > 0 && (!opts.logY || y > 0));
    el("path", {
      d: p.map(([x, y], i) => `${i ? "L" : "M"}${sx(x).toFixed(1)},${sy(y).toFixed(1)}`).join(""),
      fill: "none", stroke: s.color, "stroke-width": 2, "stroke-dasharray": s.dash ? "5 4" : "none",
      "stroke-linejoin": "round", "stroke-linecap": "round",
    }, lines);
    return { ...s, points: p };
  });

  const cross = el("line", { y1: m.top, y2: m.top + H, class: "cross", visibility: "hidden" }, svg);
  const dots = el("g", { visibility: "hidden" }, svg);
  const tip = document.createElement("div");
  tip.className = "tooltip";
  tip.style.visibility = "hidden";
  container.appendChild(tip);
  const hit = el("rect", { x: m.left, y: m.top, width: W, height: H, fill: "transparent" }, svg);

  const hide = () => { cross.setAttribute("visibility", "hidden"); dots.setAttribute("visibility", "hidden"); tip.style.visibility = "hidden"; };
  hit.addEventListener("mouseleave", hide);
  hit.addEventListener("mousemove", (ev) => {
    const box = svg.getBoundingClientRect();
    const px = (ev.clientX - box.left) * (width / box.width);
    const lx = Math.log10(x0) + (px - m.left) / W * (Math.log10(x1) - Math.log10(x0));
    const rows = [];
    dots.textContent = "";
    let at = null;
    for (const s of drawn) {
      if (!s.points.length) continue;
      let best = s.points[0];
      for (const p of s.points) if (Math.abs(Math.log10(p[0]) - lx) < Math.abs(Math.log10(best[0]) - lx)) best = p;
      at = at ?? best[0];
      el("circle", { cx: sx(best[0]), cy: sy(best[1]), r: 4, fill: s.color, stroke: css("--surface-1"), "stroke-width": 2 }, dots);
      rows.push({ s, best });
    }
    if (!rows.length) return hide();
    cross.setAttribute("x1", sx(at)); cross.setAttribute("x2", sx(at));
    cross.setAttribute("visibility", "visible"); dots.setAttribute("visibility", "visible");
    tip.textContent = "";
    const head = document.createElement("div");
    head.textContent = `iteration ${fmtInt(at)}`;
    tip.appendChild(head);
    for (const { s, best } of rows) {
      const row = document.createElement("div");
      row.className = "row";
      const sw = document.createElement("span");
      sw.className = "swatch";
      sw.style.background = s.color;
      row.append(sw, `${s.name} ${fmt(best[1])}`);
      if (best[0] !== at) row.append(` (at ${fmtInt(best[0])})`);
      tip.appendChild(row);
    }
    tip.style.visibility = "visible";
    const scale = box.width / width;
    const left = sx(at) * scale + 12;
    tip.style.left = `${Math.min(left, box.width - tip.offsetWidth - 4)}px`;
    tip.style.top = `${m.top * scale}px`;
  });
}

function drawChart() {
  const game = $("game").value;
  const series = [];
  for (const r of state.runs.filter((r) => r.game === game)) {
    const color = seriesColor(r.algorithm);
    series.push({ name: `${LABELS[r.algorithm]} average`, color, points: r.points.map((p) => [p.iteration, p.average]) });
    if (r.last) series.push({ name: `${LABELS[r.algorithm]} last iterate`, color, dash: true, points: r.points.map((p) => [p.iteration, p.current]) });
  }
  plot($("chart"), series, {
    logY: true, xLabel: "iterations", yLabel: "exploitability", label: "Exploitability against iterations",
    empty: "Press Run to start the solver",
  });
}

function drawLegend() {
  const box = $("legend");
  box.textContent = "";
  for (const r of state.runs) {
    const b = document.createElement("button");
    b.type = "button";
    b.setAttribute("aria-pressed", String(r === state.selected));
    const sw = document.createElement("span");
    sw.className = "swatch";
    sw.style.background = seriesColor(r.algorithm);
    const last = r.points[r.points.length - 1];
    const num = document.createElement("span");
    num.className = "num";
    num.textContent = `${fmtInt(r.iteration)} it, ${last ? fmt(last.average) : "-"}, ${(r.ms / 1000).toFixed(2)} s`;
    b.append(sw, LABELS[r.algorithm], num);
    b.addEventListener("click", () => select(r));
    box.appendChild(b);
  }
}

function drawActions() {
  const box = $("actions");
  box.textContent = "";
  const v = state.vocabulary;
  const shown = v.length <= 7 ? v : [v[0], v[v.length - 1]];
  shown.forEach((name, i) => {
    const s = document.createElement("span");
    const chip = document.createElement("span");
    chip.className = "chip";
    chip.style.background = actionColor(name);
    s.append(chip, name);
    if (v.length > 7 && i === 0) s.append(" ... ");
    box.appendChild(s);
  });
  if (v.length > 7) {
    const s = document.createElement("span");
    s.textContent = `(${v.length} actions, shaded in order)`;
    box.appendChild(s);
  }
}

function drawTable() {
  const box = $("table");
  box.textContent = "";
  const t = state.table;
  if (!t) { $("table-note").textContent = ""; return; }
  const q = $("filter").value.trim();
  const seat = $("seat").value;
  const rows = t.infosets.filter((i) => (seat === "all" || String(i.player) === seat) && i.key.includes(q));
  const limit = 400;
  $("table-note").textContent =
    `Average strategy after ${fmtInt(t.iteration)} iterations of ${LABELS[state.selected.algorithm]}. ` +
    (rows.length > limit ? `Showing ${limit} of ${fmtInt(rows.length)} infosets; filter to narrow.` : `${fmtInt(rows.length)} infosets.`);
  for (const i of rows.slice(0, limit)) {
    const b = document.createElement("button");
    b.type = "button";
    b.className = "item";
    b.setAttribute("role", "listitem");
    b.setAttribute("aria-current", String(i.key === state.key));
    const key = document.createElement("span");
    key.className = "key";
    const s = document.createElement("span");
    s.className = "seat";
    s.textContent = `P${i.player}`;
    key.append(s, i.key || "(root)");
    const bar = document.createElement("span");
    bar.className = "bar";
    i.average.forEach((p, a) => {
      if (p < 0.005) return;
      const seg = document.createElement("span");
      seg.style.flex = `${p} 1 0`;
      seg.style.background = actionColor(i.actions[a]);
      seg.title = `${i.actions[a]} ${(100 * p).toFixed(1)}%`;
      bar.appendChild(seg);
    });
    b.setAttribute("aria-label", `${i.key}: ` + i.actions.map((a, k) => `${a} ${(100 * i.average[k]).toFixed(0)}%`).join(", "));
    b.append(key, bar);
    b.addEventListener("click", () => { state.key = i.key; drawTable(); refreshDetail(); });
    box.appendChild(b);
  }
}

function drawDetail(info, history) {
  const box = $("detail");
  box.textContent = "";
  $("detail-note").textContent =
    `${info.key || "(root)"}, player ${info.player}. The average strategy is what CFR converges to; the current one is regret matching on the cumulative regrets below.`;
  const table = document.createElement("table");
  table.className = "mix";
  table.innerHTML = "<thead><tr><th>action</th><th>average</th><th>current</th><th>regret</th></tr></thead>";
  const body = document.createElement("tbody");
  info.actions.forEach((a, k) => {
    const tr = document.createElement("tr");
    for (const v of [a, (100 * info.average[k]).toFixed(1) + "%", (100 * info.current[k]).toFixed(1) + "%", fmt(info.regret[k])]) {
      const td = document.createElement("td");
      td.textContent = v;
      tr.appendChild(td);
    }
    body.appendChild(tr);
  });
  table.appendChild(body);
  box.appendChild(table);

  const rows = history.rows.concat(history.rows.length && history.rows[history.rows.length - 1].iteration === state.table.iteration
    ? [] : [{ iteration: state.table.iteration, average: info.average, current: info.current, regret: info.regret }]);
  const part = (title, field, yDomain) => {
    const h = document.createElement("h3");
    h.textContent = title;
    const div = document.createElement("div");
    div.className = "chart";
    box.append(h, div);
    plot(div, info.actions.map((a, k) => ({
      name: a, color: actionColor(a), points: rows.map((r) => [r.iteration, r[field][k]]),
    })), { xLabel: "iterations", yLabel: field === "regret" ? "regret" : "probability", yDomain, height: 220, label: title });
  };
  part("Average strategy", "average", [0, 1]);
  part("Cumulative regret", "regret");
}

async function refreshTable() {
  const r = state.selected;
  if (!r) return;
  const t = await call({ kind: "table", id: r.id });
  if (t.type !== "table" || state.selected !== r) return;
  state.table = t;
  const seen = new Set();
  state.vocabulary = [];
  for (const i of t.infosets) for (const a of i.actions) if (!seen.has(a)) { seen.add(a); state.vocabulary.push(a); }
  drawActions();
  drawTable();
}

async function refreshDetail() {
  const r = state.selected;
  if (!r || !state.key || !state.table) { $("detail").textContent = ""; return; }
  const info = state.table.infosets.find((i) => i.key === state.key);
  if (!info) { state.key = null; $("detail").textContent = ""; $("detail-note").textContent = "Click a row in the strategy list."; return; }
  const h = await call({ kind: "history", id: r.id, key: state.key });
  if (h.type === "history" && state.selected === r && state.key === info.key) drawDetail(info, h);
}

function drawHand(h) {
  const box = $("play");
  box.textContent = "";
  const r = state.selected;
  if (h.value !== undefined) {
    $("play-note").textContent =
      `Against ${LABELS[r.algorithm]} after ${fmtInt(r.iteration)} iterations. From your seat the bot's own strategy earns ` +
      `${fmt(h.value)} per hand against itself, and a best response earns ${fmt(h.edge)}.`;
  }
  const moves = document.createElement("div");
  moves.className = "moves";
  for (const m of h.moves) {
    const span = document.createElement("span");
    span.className = m.player === h.seat ? "move you" : "move";
    span.textContent = `${m.player === h.seat ? "you" : "bot"}: ${m.action}` + (m.infoset !== undefined ? ` (saw ${m.infoset || "root"})` : "");
    moves.appendChild(span);
  }
  if (h.moves.length) box.appendChild(moves);
  if (h.prompt) {
    const view = document.createElement("div");
    view.className = "key";
    view.textContent = `your infoset: ${h.prompt.infoset || "(root)"}`;
    const choices = document.createElement("div");
    choices.className = "choices";
    for (const a of h.prompt.actions) {
      const b = document.createElement("button");
      b.type = "button";
      b.textContent = a;
      b.addEventListener("click", async () => {
        if (state.selected !== r) return;
        const next = await call({ kind: "act", id: r.id, action: a });
        if (next.type === "hand") drawHand(next);
      });
      choices.appendChild(b);
    }
    box.append(view, choices);
  } else {
    state.score.hands += 1;
    state.score.total += h.payoff;
    const res = document.createElement("div");
    res.className = "result";
    const outcome = h.payoff === 0 ? "Split" : `You ${h.payoff > 0 ? "won" : "lost"} ${fmt(Math.abs(h.payoff))}`;
    res.textContent = `${outcome}. ` +
      `Total after ${state.score.hands} hand${state.score.hands === 1 ? "" : "s"}: ${fmt(state.score.total)} ` +
      `(${fmt(state.score.total / state.score.hands)} per hand).`;
    box.appendChild(res);
  }
}

async function deal() {
  const r = state.selected;
  if (!r || state.running) return;
  const seat = +$("play-seat").value;
  const h = await call({ kind: "deal", id: r.id, seat, seed: Math.floor(Math.random() * 2 ** 30) });
  if (h.type === "hand" && state.selected === r) drawHand(h);
}

function resetPlay() {
  state.score = { hands: 0, total: 0 };
  $("play").textContent = "";
  $("deal").disabled = !state.selected || state.running;
  $("play-note").textContent = state.selected ? "Press Deal to start a hand." : "Run a solver first.";
}

async function select(r) {
  state.selected = r;
  drawLegend();
  resetPlay();
  await refreshTable();
  await refreshDetail();
}

async function run() {
  const game = $("game").value, algorithm = $("algorithm").value;
  const target = Math.max(1, Math.min(1e6, Math.floor(+$("iterations").value || 1)));
  const last = $("last").checked;
  const old = state.runs.find((r) => r.game === game && r.algorithm === algorithm);
  if (old) {
    state.runs = state.runs.filter((r) => r !== old);
    await call({ kind: "drop", id: old.id });
  }
  const r = { id: state.nextId++, game, algorithm, last, points: [], iteration: 0, ms: 0 };
  const created = await call({ kind: "create", id: r.id, game, algorithm, last });
  if (created.type !== "created") { $("table-note").textContent = created.message; return; }
  state.runs.push(r);
  state.runs.sort((a, b) => ALGORITHMS.indexOf(a.algorithm) - ALGORITHMS.indexOf(b.algorithm));
  state.selected = r;
  state.running = true;
  state.stopping = false;
  resetPlay();
  $("run").disabled = true;
  $("stop").disabled = false;
  const small = created.infosets <= 3000;
  let shown = 0;
  while (!state.stopping) {
    const t0 = performance.now();
    const p = await call({ kind: "step", id: r.id, budget: 60, target });
    r.ms += performance.now() - t0;
    if (p.type !== "progress") break;
    r.points.push(...p.points);
    r.iteration = p.iteration;
    drawChart();
    drawLegend();
    if (small && performance.now() - shown > 1000) {
      shown = performance.now();
      await refreshTable();
    }
    if (p.done) break;
  }
  state.running = false;
  $("run").disabled = false;
  $("stop").disabled = true;
  resetPlay();
  if (state.selected === r) {
    await refreshTable();
    await refreshDetail();
  }
}

async function changeGame() {
  if (state.running) state.stopping = true;
  while (state.running) await new Promise((res) => setTimeout(res, 20));
  for (const r of state.runs) await call({ kind: "drop", id: r.id });
  const g = GAMES[$("game").value];
  state.runs = [];
  state.selected = null;
  state.key = null;
  state.table = null;
  state.vocabulary = [];
  $("iterations").value = g.iterations;
  $("last").checked = g.last;
  $("notation").textContent = g.notation;
  $("detail").textContent = "";
  $("detail-note").textContent = "Click a row in the strategy list.";
  drawActions();
  drawLegend();
  drawTable();
  drawChart();
  resetPlay();
}

$("run").addEventListener("click", run);
$("stop").addEventListener("click", () => { state.stopping = true; });
$("deal").addEventListener("click", deal);
$("play-seat").addEventListener("change", resetPlay);
$("game").addEventListener("change", changeGame);
$("filter").addEventListener("input", drawTable);
$("seat").addEventListener("change", drawTable);
let resizeTimer;
window.addEventListener("resize", () => {
  clearTimeout(resizeTimer);
  resizeTimer = setTimeout(() => { drawChart(); if (state.key) refreshDetail(); }, 150);
});
window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => {
  drawChart(); drawActions(); drawTable(); if (state.key) refreshDetail();
});
changeGame();
