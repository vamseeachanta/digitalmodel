// Offline charts and explicit comment-file export; no network or solver access.
(() => {
  "use strict";
  const evidence = JSON.parse(document.getElementById("seismic-evidence").textContent);
  const changes = JSON.parse(document.getElementById("seismic-changes").textContent);
  const ns = "http://www.w3.org/2000/svg";
  const format = value => {
    const shown = value !== 0 && Math.abs(value) < 0.0005
      ? value.toExponential(2) : value.toFixed(3);
    return shown.replaceAll("-", "−");
  };
  function svgNode(tag, attributes, text) {
    const node = document.createElementNS(ns, tag);
    Object.entries(attributes).forEach(([key, value]) => node.setAttribute(key, value));
    if (text !== undefined) node.textContent = text;
    return node;
  }
  function drawAxes(svg, box, isFilter, xMin, xMax, yMin, ySpan, px, py) {
    const font = {fill: "var(--ink)", "font-size": 12, "font-family": "Arial,sans-serif"};
    svg.append(svgNode("path", {d: "M65 30V215H775", stroke: "var(--ink)", fill: "none"}));
    for (let tick = 0; tick <= 4; tick++) {
      const fraction = tick / 4;
      const xValue = xMin + fraction * (xMax - xMin);
      const yValue = yMin + fraction * ySpan;
      svg.append(svgNode("text", {...font, x: px(xValue), y: 235, "text-anchor": "middle"}, format(xValue)));
      svg.append(svgNode("text", {...font, x: 60, y: py(yValue) + 4, "text-anchor": "end"}, format(yValue)));
    }
    svg.append(svgNode("text", {...font, x: 420, y: 257, "text-anchor": "middle"}, isFilter ? "Frequency (Hz)" : "Time (s)"));
    svg.append(svgNode("text", {...font, x: 65, y: 18}, `${box.dataset.label} (${box.dataset.unit})`));
  }
  function drawFilterLimits(svg, box, metrics, px) {
    [[metrics.low_hz, "var(--caution)", "Effective lower edge"],
      [metrics.high_hz, "var(--caution)", "Effective upper edge"],
      [metrics.nyquist_hz, "var(--nogo)", "Nyquist sampling boundary"]].forEach(([frequency, color, label]) => {
      const line = svgNode("line", {x1: px(frequency), x2: px(frequency), y1: 30, y2: 215,
        stroke: color, "stroke-width": 2, "stroke-dasharray": "5 4", "data-limit": label});
      line.append(svgNode("title", {}, `${label}: ${format(frequency)} Hz`));
      svg.append(line);
    });
    const legend = document.createElement("p");
    legend.textContent = `Amber dashed lines mark the effective edges at ${format(metrics.low_hz)} and ${format(metrics.high_hz)} Hz. The red dashed line marks the Nyquist sampling boundary at ${format(metrics.nyquist_hz)} Hz. The blue curve is filter gain.`;
    box.append(legend);
  }
  function makeChart(box) {
    const isFilter = box.dataset.chart === "filter";
    const x = isFilter ? evidence.filter_response.frequency_hz : evidence.timeseries.time_s;
    const y = isFilter ? evidence.filter_response.gain : evidence.timeseries[box.dataset.column];
    const metrics = evidence.metrics;
    const xMin = x[0], xMax = isFilter ? metrics.nyquist_hz : x[x.length - 1];
    const yMin = Math.min(0, ...y), yMax = Math.max(...y);
    const ySpan = yMax - yMin || 1;
    const px = value => 65 + (value - xMin) / (xMax - xMin) * 710;
    const py = value => 215 - (value - yMin) / ySpan * 180;
    const svg = svgNode("svg", {viewBox: "0 0 800 265", role: "img", "aria-label": box.dataset.label});
    svg.append(svgNode("title", {}, `${box.dataset.label} against ${isFilter ? "frequency" : "time"}`));
    drawAxes(svg, box, isFilter, xMin, xMax, yMin, ySpan, px, py);
    svg.append(svgNode("polyline", {
      points: x.map((value, i) => `${px(value)},${py(y[i])}`).join(" "),
      fill: "none", stroke: "var(--brand)", "stroke-width": 1.5
    }));
    if (isFilter) drawFilterLimits(svg, box, metrics, px);
    const marker = svgNode("circle", {r: 4, fill: "var(--brand)"});
    svg.append(marker);
    const slider = document.createElement("input");
    slider.type = "range";
    slider.min = "0";
    slider.max = String(x.length - 1);
    slider.step = "1";
    slider.value = "0";
    slider.setAttribute("aria-label", `${box.dataset.label}: select ${isFilter ? "frequency" : "time"} sample`);
    const output = document.createElement("output");
    output.setAttribute("aria-live", "polite");
    function show(index) {
      const i = Math.max(0, Math.min(y.length - 1, index));
      marker.setAttribute("cx", px(x[i]));
      marker.setAttribute("cy", py(y[i]));
      slider.value = String(i);
      const limit = isFilter
        ? `Sampling limit: ${format(metrics.nyquist_hz)} Hz; effective band: ${format(metrics.low_hz)}–${format(metrics.high_hz)} Hz.`
        : "Demand acceptance limit: not supplied.";
      output.textContent = `Case: ${evidence.case}. Component: ${box.dataset.label}. ${isFilter ? "Frequency" : "Time"}: ${format(x[i])} ${isFilter ? "Hz" : "s"}. Demand: ${format(y[i])} ${box.dataset.unit}. ${limit}`;
    }
    slider.addEventListener("input", () => show(Number(slider.value)));
    svg.addEventListener("pointermove", event => {
      const bounds = svg.getBoundingClientRect();
      const fraction = Math.max(0, Math.min(1, ((event.clientX - bounds.left) / bounds.width * 800 - 65) / 710));
      const target = xMin + fraction * (xMax - xMin);
      const index = Math.round((target - x[0]) / (x[1] - x[0]));
      show(index);
    });
    box.append(svg, slider, output);
    show(0);
  }
  document.querySelectorAll("[data-chart]").forEach(makeChart);

  const changeList = document.getElementById("change-list");
  changes.revisions.forEach(revision => {
    const paragraph = document.createElement("p");
    paragraph.textContent = `Revision ${revision.revision} (${revision.date}): ${revision.changes.join(" ")}`;
    changeList.append(paragraph);
  });

  let notes = [], saved = true, generation = 0;
  // "00" identifies the comments-file schema, independently of report revision.
  const status = document.getElementById("comments-status");
  const text = document.getElementById("comment-text");
  const section = document.getElementById("comment-section");
  const allowedSections = new Set(Array.from(section.options, option => option.value));
  text.addEventListener("input", () => { generation += 1; });
  function renderNotes() {
    const list = document.getElementById("comments-list");
    list.replaceChildren();
    notes.forEach(note => {
      const item = document.createElement("li");
      const link = document.createElement("a");
      link.href = `#${note.section}`;
      link.textContent = document.querySelector(`#${note.section} h2`).textContent;
      item.append(link, document.createTextNode(`: ${note.text}`));
      list.append(item);
    });
  }
  document.getElementById("add-comment").addEventListener("click", () => {
    if (!text.value.trim()) {
      status.textContent = "Enter a review note before adding it.";
      return;
    }
    notes.push({section: section.value, text: text.value.trim()});
    generation += 1;
    text.value = "";
    saved = false;
    renderNotes();
    status.textContent = "The note has been added. Save comments to retain it.";
  });
  document.getElementById("save-comments").addEventListener("click", async () => {
    if (text.value.trim()) {
      status.textContent = "Add the pending note before saving comments.";
      return;
    }
    const exportGeneration = generation;
    const content = JSON.stringify({document: changes.document, revision: "00", notes}, null, 2) + "\n";
    const name = "seismic-analysis.comments.json";
    try {
      if (window.showSaveFilePicker) {
        const handle = await window.showSaveFilePicker({suggestedName: name,
          types: [{description: "Review notes", accept: {"application/json": [".json"]}}]});
        const stream = await handle.createWritable();
        await stream.write(content);
        await stream.close();
        status.textContent = "Comments were saved to the selected location. Keep the file beside the report.";
      } else {
        const url = URL.createObjectURL(new Blob([content], {type: "application/json"}));
        const link = document.createElement("a");
        link.href = url;
        link.download = name;
        link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        status.textContent = "A comments download was requested. Keep the downloaded file beside the report.";
      }
      if (generation === exportGeneration) {
        saved = true;
      } else {
        saved = false;
        status.textContent = "Comments changed during export. Save the current comments to retain those edits.";
      }
    } catch (error) {
      status.textContent = error.name === "AbortError"
        ? "Saving was cancelled. Comments remain unsaved."
        : "Comments were not saved. Export again before closing.";
    }
  });
  document.getElementById("load-comments").addEventListener("change", async event => {
    try {
      const file = event.target.files[0];
      if (!file) return;
      if (!saved || text.value.trim()) throw new Error("pending notes");
      if (file.size > 1000000) throw new Error("size");
      const loadGeneration = generation;
      const value = JSON.parse(await file.text());
      if (generation !== loadGeneration || !saved || text.value.trim()) throw new Error("intervening edits");
      if (value.document !== changes.document || value.revision !== "00" || !Array.isArray(value.notes)
          || value.notes.length > 1000 || !value.notes.every(note => note && allowedSections.has(note.section)
          && typeof note.text === "string" && note.text.length <= 10000)) throw new Error("format");
      notes = value.notes.map(note => ({section: note.section, text: note.text}));
      generation += 1;
      saved = true;
      renderNotes();
      status.textContent = "Saved comments were loaded.";
    } catch (error) {
      status.textContent = "Comments were not loaded. Save pending notes first and select a matching comments file.";
    }
    event.target.value = "";
  });
  window.addEventListener("beforeunload", event => {
    if (!saved || text.value.trim()) {
      event.preventDefault();
      event.returnValue = "";
    }
  });
})();
