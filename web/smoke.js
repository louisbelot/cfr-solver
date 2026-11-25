"use strict";
const assert = require("node:assert");

const replies = [];
globalThis.onmessage = null;
globalThis.postMessage = (s) => replies.push(JSON.parse(s));
require(require("node:path").resolve(process.argv[2]));
const send = (m) => {
  globalThis.onmessage({ data: m });
  return replies[replies.length - 1];
};

assert.equal(send({ kind: "create", id: 1, game: "chess", algorithm: "cfr", sampling: "full", seed: 1, last: true }).type, "error");
assert.equal(send({ kind: "create", id: 1, game: "kuhn", algorithm: "cfr+", sampling: "full", seed: 1, last: true }).infosets, 12);
const p = send({ kind: "step", id: 1, budget: 1e9, target: 1000 });
assert.ok(p.done);
const end = p.points[p.points.length - 1];
assert.ok(end.average < 1e-3 && end.current < 0.1, JSON.stringify(end));
const t = send({ kind: "table", id: 1 });
assert.equal(t.infosets.length, 12);
assert.equal(send({ kind: "history", id: 1, key: "Qpb" }).rows.length, p.points.length);
assert.equal(send({ kind: "create", id: 2, game: "leduc", algorithm: "pdcfr+", sampling: "full", seed: 1, last: false }).infosets, 288);
assert.equal(send({ kind: "create", id: 3, game: "leduc", algorithm: "cfr", sampling: "outcome", seed: 9, last: false }).infosets, 288);
const os = send({ kind: "step", id: 3, budget: 1e9, target: 5000 });
assert.ok(os.done && os.touched < 5000 * 400, JSON.stringify(os.touched));
console.log(`kuhn cfr+ ${end.iteration}: ${end.average}, leduc os ${os.touched} nodes in 5000 iterations`);
