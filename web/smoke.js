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

assert.equal(send({ kind: "create", id: 1, game: "chess", algorithm: "cfr", last: true }).type, "error");
assert.equal(send({ kind: "create", id: 1, game: "kuhn", algorithm: "cfr+", last: true }).infosets, 12);
const p = send({ kind: "step", id: 1, budget: 1e9, target: 1000 });
assert.ok(p.done);
const end = p.points[p.points.length - 1];
assert.ok(end.average < 1e-3 && end.current < 0.1, JSON.stringify(end));
const t = send({ kind: "table", id: 1 });
assert.equal(t.infosets.length, 12);
assert.equal(send({ kind: "history", id: 1, key: "Qpb" }).rows.length, p.points.length);
assert.equal(send({ kind: "create", id: 2, game: "leduc", algorithm: "pdcfr+", last: false }).infosets, 288);
console.log(`kuhn cfr+ ${end.iteration}: ${end.average}, leduc created`);
