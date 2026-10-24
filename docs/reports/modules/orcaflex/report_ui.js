/* Offline chart interaction and explicitly saved document comments. */
"use strict";
const evidence = JSON.parse(document.getElementById("evidence").textContent);
const chart = document.getElementById("tension-chart");
const svgNamespace = "http://www.w3.org/2000/svg";
function shape(tag, attributes, text = "") {
  const node = document.createElementNS(svgNamespace, tag);
  Object.entries(attributes).forEach(([key, value]) => node.setAttribute(key, value));
  node.textContent = text;
  chart.appendChild(node);
  return node;
}
function drawChart() {
  const maxTension = Math.max(...evidence.chart.map(row => row.tension)) * 1.12;
  const x = angle => 80 + angle / evidence.angle_limit * 640;
  const y = tension => 300 - tension / maxTension * 240;
  shape("path", {d: "M80 45 V300 H750", fill: "none", stroke: "var(--ink)"});
  shape("text", {x: 240, y: 365}, "Hang-off angle from vertical (degrees)");
  shape("text", {x: 80, y: 25}, "Top tension (kN); engineering limit not established");
  shape("line", {x1: x(evidence.angle_limit), x2: x(evidence.angle_limit), y1: 45, y2: 300,
    stroke: "var(--caution)", "stroke-dasharray": "6 4"});
  shape("text", {x: 440, y: 47}, "Software angle bound");
  evidence.chart.forEach(row => {
    shape("text", {x: x(row.angle) - 10, y: 323}, row.angle.toFixed(1));
    shape("line", {x1: 80, x2: x(row.angle), y1: y(row.tension), y2: y(row.tension),
      stroke: "var(--edge)", "stroke-dasharray": "3 3"});
    shape("text", {x: 6, y: y(row.tension) + 5}, row.tension.toFixed(1));
    const difference = (row.tension - row.reference) / row.reference * 100;
    const roundedDifference = difference.toFixed(1);
    const displayedDifference = roundedDifference === "-0.0" ? "0.0" : roundedDifference.replace("-", "−");
    const detail = `Case: still water at ${row.angle.toFixed(1)} degrees; component: upper support; ` +
      `demand: ${row.tension.toFixed(1)} kN; tension limit: not established; ` +
      `software angle limit: ${evidence.angle_limit.toFixed(1)} degrees; ` +
      `force-balance reference: ${row.reference.toFixed(1)} kN; difference: ` +
      `${displayedDifference} % of reference.`;
    const marker = shape("circle", {cx: x(row.angle), cy: y(row.tension), r: 8,
      fill: "white", stroke: "var(--brand)", "stroke-width": 2, tabindex: 0,
      role: "button", "aria-label": detail});
    const title = document.createElementNS(svgNamespace, "title");
    title.textContent = detail;
    marker.appendChild(title);
    ["mouseenter", "focus", "click"].forEach(event => marker.addEventListener(event, () => {
      document.getElementById("chart-readout").textContent = detail;
    }));
    shape("path", {d: `M${x(row.angle)-5} ${y(row.reference)-5} l10 10 m-10 0 l10 -10`,
      stroke: "var(--ink)", fill: "none", "pointer-events": "none"});
  });
}
drawChart();
const changes = JSON.parse(document.getElementById("change-data").textContent);
changes.changes.forEach(change => {
  const item = document.createElement("li");
  item.textContent = `${change.section}: ${change.description}`;
  document.getElementById("change-panel").appendChild(item);
});
const sectionSelect = document.getElementById("comment-section");
document.querySelectorAll("h2").forEach(heading => {
  const option = document.createElement("option");
  option.value = heading.id;
  option.textContent = heading.textContent;
  sectionSelect.appendChild(option);
});
let comments = [];
const statusText = document.getElementById("comment-status");
function renderComments() {
  const list = document.getElementById("comments");
  list.replaceChildren();
  comments.forEach(comment => {
    const item = document.createElement("li");
    const link = document.createElement("a");
    link.href = `#${comment.section}`;
    link.textContent = document.getElementById(comment.section).textContent;
    item.append(link, document.createTextNode(`: ${comment.text}`));
    list.appendChild(item);
  });
}
document.getElementById("add-comment").addEventListener("click", () => {
  const field = document.getElementById("comment-text");
  if (!field.value.trim()) return;
  comments.push({section: sectionSelect.value, text: field.value.trim()});
  field.value = "";
  renderComments();
  statusText.textContent = "Comment added; save comments to retain it.";
});
document.getElementById("save-comments").addEventListener("click", async () => {
  const content = JSON.stringify({revision: changes.revision, comments}, null, 2);
  try {
    if (window.showSaveFilePicker) {
      const handle = await window.showSaveFilePicker({suggestedName: "report.comments.json",
        types: [{description: "Document comments", accept: {"application/json": [".json"]}}]});
      const writable = await handle.createWritable();
      await writable.write(content);
      await writable.close();
      statusText.textContent = "Comments saved to the selected file.";
    } else {
      const link = document.createElement("a");
      link.href = URL.createObjectURL(new Blob([content], {type: "application/json"}));
      link.download = "report.comments.json";
      link.click();
      setTimeout(() => URL.revokeObjectURL(link.href), 1000);
      statusText.textContent = "Download requested; place report.comments.json beside this report.";
    }
  } catch (error) {
    statusText.textContent = "Comments were not saved; retain this page and try saving again.";
  }
});
document.getElementById("load-comments").addEventListener("change", async event => {
  try {
    const file = event.target.files[0];
    if (!file) return;
    const loaded = JSON.parse(await file.text());
    if (loaded.revision !== changes.revision || !Array.isArray(loaded.comments) ||
        !loaded.comments.every(c => /^s(?:[1-9]|10)$/.test(c.section) && typeof c.text === "string")) {
      throw new Error("Invalid comment record");
    }
    comments = comments.concat(loaded.comments);
    renderComments();
    statusText.textContent = "Comments loaded and appended; save to retain the combined record.";
  } catch (error) {
    statusText.textContent = "Comments were not loaded: the file format or revision does not match.";
  }
});
