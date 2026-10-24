/* Offline DOM contract check; execute from the repository root with Node. */
const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const path = require("node:path");
const root = path.resolve(__dirname, "../..");
const report = fs.readFileSync(path.join(root, "docs/reports/modules/orcaflex/report.html"), "utf8");

class Element {
  constructor(tag = "div") {
    this.tag = tag;
    this.attributes = {};
    this.children = [];
    this.events = {};
    this.textContent = "";
    this.value = "";
  }
  setAttribute(key, value) { this.attributes[key] = value; }
  appendChild(child) { this.children.push(child); return child; }
  append(...children) { this.children.push(...children); }
  replaceChildren() { this.children = []; }
  addEventListener(name, handler) { this.events[name] = handler; }
  click() { if (this.events.click) return this.events.click(); }
}

const elements = new Map();
for (const match of report.matchAll(/\bid="([^"]+)"/g)) {
  elements.set(match[1], new Element());
}
for (const match of report.matchAll(/<h2 id="([^"]+)">([^<]+)<\/h2>/g)) {
  const heading = elements.get(match[1]);
  heading.id = match[1];
  heading.textContent = match[2];
}
for (const name of ["evidence", "change-data"]) {
  elements.get(name).textContent = report.match(
    new RegExp(`<script id="${name}" type="application/json">([\\s\\S]*?)</script>`))[1];
}
const document = {
  getElementById(id) { assert(elements.has(id), `Missing element: ${id}`); return elements.get(id); },
  createElementNS(namespace, tag) { return new Element(tag); },
  createElement(tag) { return new Element(tag); },
  createTextNode(text) { const node = new Element("text"); node.textContent = text; return node; },
  querySelectorAll(selector) {
    assert.equal(selector, "h2");
    return [...elements.values()].filter(element => element.id);
  },
};
let saved = null;
let closed = false;
const window = {async showSaveFilePicker(options) {
  assert.equal(options.suggestedName, "report.comments.json");
  return {async createWritable() { return {
    async write(content) { saved = JSON.parse(content); },
    async close() { closed = true; },
  }; }};
}};
const script = report.match(/<script>([\s\S]*?)<\/script>/)[1];
const context = {document, window, setTimeout, URL, Blob};
vm.runInNewContext(script, context, {timeout: 1000});

function checkDifference(marker, expected) {
  const detail = marker.attributes["aria-label"];
  assert(detail.endsWith(`difference: ${expected} % of reference.`));
  assert.equal(marker.children.find(node => node.tag === "title").textContent, detail);
  for (const event of ["mouseenter", "focus", "click"]) {
    marker.events[event]();
    assert.equal(elements.get("chart-readout").textContent, detail);
  }
}

async function check() {
  const data = JSON.parse(elements.get("evidence").textContent);
  const markers = elements.get("tension-chart").children.filter(node => node.tag === "circle");
  assert.equal(markers.length, data.chart.length);
  markers.forEach((marker, index) => {
    marker.events.focus();
    assert(elements.get("chart-readout").textContent.includes(data.chart[index].tension.toFixed(1)));
    assert(elements.get("chart-readout").textContent.includes("limit: not established"));
    checkDifference(marker, "0.0");
  });
  const percentageCases = [
    [100, "0.0"], [100.04, "0.0"], [99.96, "0.0"],
    [101.26, "1.3"], [98.74, "−1.3"], [100.14, "0.1"], [99.86, "−0.1"],
  ];
  const chart = elements.get("tension-chart");
  chart.replaceChildren();
  const chartData = percentageCases.map(([tension]) => ({angle: 10, tension, reference: 100}));
  vm.runInNewContext(`evidence.chart = ${JSON.stringify(chartData)}; drawChart();`, context, {timeout: 1000});
  const percentageMarkers = chart.children.filter(node => node.tag === "circle");
  assert.equal(percentageMarkers.length, percentageCases.length);
  percentageMarkers.forEach((marker, index) => checkDifference(marker, percentageCases[index][1]));
  assert.equal(elements.get("comment-section").children.length, 10);
  elements.get("comment-section").value = "s5";
  elements.get("comment-text").value = "Check the <support> capacity.";
  await elements.get("add-comment").click();
  await elements.get("save-comments").click();
  assert(closed);
  assert.deepEqual(saved.comments, [{section: "s5", text: "Check the <support> capacity."}]);
  assert.equal(saved.revision, "B");
  const load = elements.get("load-comments").events.change;
  await load({target: {files: [{async text() { return JSON.stringify(saved); }}]}});
  assert.equal(elements.get("comments").children.length, 2);
  await load({target: {files: [{async text() { return '{"revision":"invalid","comments":[]}'; }}]}});
  assert(elements.get("comment-status").textContent.includes("not loaded"));
  assert.equal(elements.get("comments").children.length, 2);
  window.showSaveFilePicker = async () => { throw new Error("Cancelled"); };
  await elements.get("save-comments").click();
  assert(elements.get("comment-status").textContent.includes("not saved"));
  console.log("PASS: chart markers, percentage rounding, hover, comment save/readback, invalid input and cancellation");
}
check().catch(error => { console.error(error); process.exitCode = 1; });
