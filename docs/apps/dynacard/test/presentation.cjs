"use strict";
const assert = require("node:assert/strict");
const {formatValue, pumpCsv} = require("../presentation.js");
for (const [value, unit, expected] of [
  [12.3, "in", "12.30"], [12345.6, "lbf", "12,346"],
  [-1234.6, "lbf", "-1,235"], [83, "%", "83.0"],
  [100, "psi", "100.0"], [58.6513153, "lb/ft³", "58.651"],
  [58.6513153, "lb/ft3", "58.651"], [0.406987492, "psi/ft", "0.4070"],
  [56, "bbl/day", "56.0"], [11535.7275, "ft·lbf", "11,536"],
  [24, "h/day", "24"], [0, "in", "0.00"],
  [100, "% · reference assumption", "100.0"],
  [null, "psi", "Not established"], ["card", "reference provenance", "card"],
]) assert.equal(formatValue(value, unit), expected);
const card = {position:[-1.123456789123, 2.987654321987], load:[1234.56789123, -98.7654321987]};
const before = JSON.stringify(card);
const csv = pumpCsv(card);
assert.equal(csv.split("\n")[0], "position_in,load_lbf");
assert.deepEqual(csv.trim().split("\n").slice(1).map(row => row.split(",").map(Number)),
  card.position.map((value, i) => [value, card.load[i]]));
formatValue(card.position[0], "in");
assert.equal(JSON.stringify(card), before);
console.log("PASS: unit formatting and full-precision pump CSV; inputs unchanged.");
