/* Browser UI only; all calculations remain in the DOM-free core. */
"use strict";
(() => {
  const byId = id => document.getElementById(id);
  const fields = ["spm", "pump-diameter", "pump-depth", "fluid-density", "tubing-pressure"];
  const rodFields = ["diameter", "length", "weight_per_foot", "modulus_of_elasticity"];
  const rodLabels = ["diameter (in)", "length (ft)", "weight (lbf/ft)", "modulus (psi)"];
  let card = null, cardName = "", loadVersion = 0, loadedExample = null;
  let currentResult = null;
  const format = DynacardPresentation.formatValue;

  function element(tag, text, className) {
    const node = document.createElement(tag);
    if (text !== undefined) node.textContent = text;
    if (className) node.className = className;
    return node;
  }

  function invalidate() {
    currentResult = null;
    byId("results").hidden = true;
    byId("error").hidden = true;
  }

  function showError(error) {
    currentResult = null;
    byId("results").hidden = true;
    byId("error").textContent = error.message || String(error);
    byId("error").hidden = false;
  }

  function renumberRods() {
    const rows = Array.from(byId("rod-rows").children);
    rows.forEach((row, index) => {
      row.querySelectorAll("input").forEach((input, column) => {
        input.setAttribute("aria-label", `Taper ${index + 1} ${rodLabels[column]}`);
      });
      const button = row.querySelector("button");
      button.disabled = rows.length === 1;
      button.setAttribute("aria-label", `Remove taper ${index + 1}`);
    });
  }

  function addRod(rod) {
    const row = element("tr");
    rodFields.forEach(field => {
      const input = element("input");
      input.type = "number"; input.min = "0.000001"; input.step = "any";
      input.required = true; input.value = rod[field]; input.dataset.field = field;
      const cell = element("td"); cell.append(input); row.append(cell);
    });
    const button = element("button", "Remove"); button.type = "button";
    button.addEventListener("click", () => {row.remove(); renumberRods(); invalidate();});
    const cell = element("td"); cell.append(button); row.append(cell);
    byId("rod-rows").append(row); renumberRods();
  }

  function setExampleWell() {
    const well = DynacardExamples.well;
    [well.spm, well.pump.diameter, well.pump.depth, well.fluid_density,
      well.input_params.tubing_pressure].forEach((value, index) => {byId(fields[index]).value = value;});
    byId("rod-rows").replaceChildren();
    well.rod_string.forEach(addRod);
  }

  function loadExample() {
    loadVersion++; invalidate(); setExampleWell();
    const example = DynacardExamples.examples[Number(byId("example-select").value)];
    loadedExample = example;
    card = {position: example.card.position.slice(), load: example.card.load.slice()};
    cardName = example.name;
    byId("card-file").value = "";
    byId("card-status").textContent = `Loaded ${cardName}: ${card.position.length} samples. Example well restored.`;
  }

  async function loadFile() {
    const version = ++loadVersion;
    card = null; loadedExample = null; invalidate();
    const file = byId("card-file").files[0];
    if (!file) {byId("card-status").textContent = "No card loaded."; return;}
    byId("card-status").textContent = "Reading local CSV…";
    try {
      const text = await file.text();
      if (version !== loadVersion) return;
      card = DynacardCore.parseCardCsv(text); cardName = file.name;
      byId("card-status").textContent = `Loaded ${cardName}: ${card.position.length} samples.`;
    } catch (error) {
      if (version !== loadVersion) return;
      card = null; byId("card-status").textContent = "CSV rejected; no card loaded.";
      showError(error);
    }
  }

  function numberInput(id) {
    const value = byId(id).valueAsNumber;
    if (!Number.isFinite(value) || value <= 0) throw new Error(`${byId(id).labels[0].textContent} must be a finite positive number.`);
    return value;
  }

  function readWell() {
    const [spm, diameter, depth, fluidDensity, tubingPressure] = fields.map(numberInput);
    const rods = Array.from(byId("rod-rows").children).map((row, index) => {
      const rod = {};
      row.querySelectorAll("input").forEach(input => {
        const value = input.valueAsNumber;
        if (!Number.isFinite(value) || value <= 0) throw new Error(`Taper ${index + 1}: ${input.dataset.field} must be finite and positive.`);
        rod[input.dataset.field] = value;
      });
      return rod;
    });
    return {api14:"BROWSER-CARD", spm, pump:{diameter, depth}, fluid_density:fluidDensity,
      rod_string:rods, input_params:{strokes_per_minute:spm, tubing_pressure:tubingPressure, runtime:24}};
  }

  function svgElement(tag, attributes, text) {
    const node = document.createElementNS("http://www.w3.org/2000/svg", tag);
    Object.entries(attributes).forEach(([key, value]) => node.setAttribute(key, value));
    if (text !== undefined) node.textContent = text;
    return node;
  }

  function plot(id, data, result) {
    const svg = byId(id); svg.replaceChildren();
    if (result) {
      const origin = Math.min(...data.position);
      data = {position:data.position.map(value => value - origin), load:data.load};
      svg.dataset.positionOrigin = "stroke-bottom";
      svg.dataset.positionMinimum = Math.min(...data.position);
    }
    const xmin = Math.min(...data.position), xmax = Math.max(...data.position);
    const ymin = Math.min(...data.load), ymax = Math.max(...data.load);
    const xp = value => 72 + (value - xmin) / (xmax - xmin || 1) * 500;
    const yp = value => 276 - (value - ymin) / (ymax - ymin || 1) * 246;
    for (let i = 0; i <= 4; i++) {
      const x = xmin + (xmax - xmin) * i / 4, y = ymin + (ymax - ymin) * i / 4;
      svg.append(svgElement("line", {x1:xp(x),x2:xp(x),y1:30,y2:276,class:"grid-line"}));
      svg.append(svgElement("line", {x1:72,x2:572,y1:yp(y),y2:yp(y),class:"grid-line"}));
      svg.append(svgElement("text", {x:xp(x),y:298,"text-anchor":"middle"}, `${x.toFixed(1)} in`));
      svg.append(svgElement("text", {x:68,y:yp(y)+4,"text-anchor":"end"}, `${y.toFixed(0)} lbf`));
    }
    const points = data.position.map((value, index) => `${xp(value)},${yp(data.load[index])}`);
    points.push(points[0]);
    svg.append(svgElement("polyline", {points:points.join(" "),class:"card-line"}));
    svg.append(svgElement("text", {x:322,y:327,"text-anchor":"middle",class:"axis-title"}, "Position (in)"));
    svg.append(svgElement("text", {x:72,y:18,class:"axis-title"}, "Load (lbf)"));
    if (result) pumpAnnotations(svg, result, xp, yp);
  }

  function pumpAnnotations(svg, result, xp, yp) {
    const fluid = result.fluid_load, fillage = result.fillage;
    for (const phase of ["upstroke", "downstroke"]) {
      const value = fluid[`${phase}_load`];
      svg.append(svgElement("line", {x1:72,x2:572,y1:yp(value),y2:yp(value),
        class:`mean-line ${phase}-mean`,"data-load":value}));
    }
    const top = yp(fluid.upstroke_load), bottom = yp(fluid.downstroke_load);
    svg.append(svgElement("path", {d:`M580 ${top} h10 M585 ${top} V${bottom} M580 ${bottom} h10`,class:"travel-line"}));
    for (const [travel, y, kind] of [[fillage.gross_stroke, 266, "gross"], [fillage.net_stroke, 274, "effective"]]) {
      svg.append(svgElement("line", {x1:xp(0),x2:xp(travel),y1:y,y2:y,class:`travel-line ${kind}-travel`}));
    }
    const fixed = value => value.toFixed(2);
    const labels = [
      `Pump stroke: ${fixed(fillage.gross_stroke)} in · Effective travel: ${fixed(fillage.net_stroke)} in`,
      `Fillage: ${fixed(fillage.fillage)}% · Fluid load: ${fixed(fluid.fluid_load)} lbf`,
      `Upstroke mean: ${fixed(fluid.upstroke_load)} lbf (teal dashed)`,
      `Downstroke mean: ${fixed(fluid.downstroke_load)} lbf (purple dashed)`];
    labels.forEach((label, i) => svg.append(svgElement("text", {x:72,y:352+i*22,class:"plot-annotation"}, label)));
  }

  function resultRows(result) {
    const rows = [["Surface stroke",result.surface_stroke,"in"],
      ["Peak polished-rod load",result.peak_polished_rod_load,"lbf"],
      ["Minimum polished-rod load",result.minimum_polished_rod_load,"lbf"],
      ["Pump gross stroke",result.fillage.gross_stroke,"in"],
      ["Pump net stroke",result.fillage.net_stroke,"in"], ["Pump fillage",result.fillage.fillage,"%"],
      ["Fluid load",result.fluid_load.fluid_load,"lbf"],
      ["Upstroke load",result.fluid_load.upstroke_load,"lbf"],
      ["Downstroke load",result.fluid_load.downstroke_load,"lbf"],
      ["CPIP: pump intake pressure",result.cpip.pump_intake_pressure,"psi"],
      ["CPIP: pump discharge pressure",result.cpip.pump_discharge_pressure,"psi"],
      ["CPIP fluid density",result.cpip.fluid_density,"lb/ft³ · separate mixture basis"],
      ["CPIP tubing gradient",result.cpip.tubing_gradient,"psi/ft"],
      ["Gross displacement",result.production.gross_displacement,"bbl/day"],
      ["Net displacement",result.production.net_displacement,"bbl/day"],
      ["Theoretical production (displacement)",result.production.theoretical_production,"bbl/day · not measured production"],
      ["Pump efficiency",result.production.pump_efficiency,"% · reference assumption"],
      ["Runtime",result.production.runtime_hours,"h/day · assumed in this page"],
      ["Runtime source",result.production.runtime_source,"reference provenance"],
      ["Pump-card maximum load",result.pump_card_max_load,"lbf"],
      ["Pump-card minimum load",result.pump_card_min_load,"lbf"],
      ["Polished-rod work per stroke (ft·lbf)",result.polished_rod_work_per_stroke,"ft·lbf · surface card area"],
      ["Pump-card work per stroke (ft·lbf)",result.pump_work_per_stroke,"ft·lbf · computed pump card area"],
      ["Corner indices: BL, TL, TR, BR",result.fillage.corners.join(", "),"zero-based sample indices"],
      ["Buckling threshold flag",result.buckling_detected ? "Triggered" : "Not triggered","minimum pump load < −500 lbf"]];
    byId("analysis-values").replaceChildren(...rows.map(([label, value, unit]) => {
      const row = element("tr");
      row.append(element("td", label), element("td", format(value, unit)), element("td", unit));
      return row;
    }));
  }

  function diagnosis(title, result) {
    const block = element("article", undefined, "diagnosis");
    block.append(element("h4", title), element("p", "HYPOTHESIS", "hypothesis"),
      element("p", result.classification.replaceAll("_", " "), "classification"),
      element("p", `Model confidence: ${(result.confidence * 100).toFixed(2)}%`));
    const list = element("ol");
    result.differential.forEach(item => list.append(element("li", `HYPOTHESIS: ${item.mode.replaceAll("_", " ")} — ${(item.probability * 100).toFixed(2)}%`)));
    block.append(list);
    const details = element("details"); details.append(element("summary", "Full model probabilities"));
    const full = element("ul");
    Object.entries(result.probabilities).forEach(([label, value]) => {
      full.append(element("li", `${label.replaceAll("_", " ")}: ${(value * 100).toFixed(6)}%`));
    });
    details.append(full); block.append(details); return block;
  }

  function render(result) {
    plot("surface-plot", card); plot("pump-plot", result.downhole_card, result); resultRows(result);
    byId("result-basis").textContent = `${cardName} · ${card.position.length} samples · ${result.solver_method}`;
    const guards = [...result.solver_flags];
    if (!guards.length) guards.push("No reference solver guard annotations. This does not establish physical validity.");
    guards.push(`Recorded maximum inclination: ${result.max_inclination_deg === null ? "not supplied; vertical assumed" : format(result.max_inclination_deg) + "°"}.`);
    byId("guard-messages").replaceChildren(...guards.map(text => element("li", text)));
    byId("diagnoses").replaceChildren(diagnosis("Shape-only model (computed pump card)", result.shape_diagnosis),
      diagnosis("Shipped model call (19 features)", result.diagnosis));
    renderExampleSource();
    currentResult = result;
    byId("results").hidden = false;
  }

  function renderExampleSource() {
    const source = byId("example-source"); source.replaceChildren();
    source.hidden = !loadedExample;
    if (!loadedExample) return;
    const link = element("a", `DOI ${loadedExample.source_doi}`); link.href = loadedExample.source;
    const worked = element("a", `Worked example ${loadedExample.index}: ${loadedExample.english_gloss}`, "worked-example-link");
    worked.href = `../../domains/artificial_lift/dynacard-examples/README.md#example-${loadedExample.index}-${loadedExample.slug}`;
    source.append(element("span", `Plate condition: ${loadedExample.name} (${loadedExample.source_label}). Crosswalk: ${loadedExample.crosswalk_mode} (${loadedExample.crosswalk_status}). `), link,
      element("span", " · "), worked,
      element("span", ". The plate condition describes the source; the hypotheses use the computed pump card with the current well inputs."));
  }

  function downloadResult(kind) {
    if (!currentResult) return;
    const csv = kind === "csv";
    const text = csv ? DynacardPresentation.pumpCsv(currentResult.downhole_card) : JSON.stringify(currentResult, null, 2) + "\n";
    const blob = new Blob([text], {type:csv ? "text/csv;charset=utf-8" : "application/json"});
    const url = URL.createObjectURL(blob), link = element("a");
    link.href = url; link.download = csv ? "pump-card.csv" : "dynacard-results.json";
    document.body.append(link); link.click(); link.remove();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }

  function runAnalysis(event) {
    event?.preventDefault(); invalidate();
    try {
      if (!card) throw new Error("Load a valid surface CSV or an example before analysis.");
      render(DynacardCore.analyse(card, readWell()));
    } catch (error) {showError(error);}
  }

  function initialize() {
    byId("download-results").addEventListener("click", () => downloadResult("json"));
    byId("download-pump").addEventListener("click", () => downloadResult("csv"));
    DynacardExamples.examples.forEach((example, index) => {
      const option = element("option", `${index + 1}. ${example.name}`); option.value = index;
      byId("example-select").append(option);
    });
    byId("load-example").addEventListener("click", loadExample);
    byId("card-file").addEventListener("change", loadFile);
    byId("well-form").addEventListener("input", invalidate);
    byId("well-form").addEventListener("submit", runAnalysis);
    byId("add-rod").addEventListener("click", () => {
      addRod({diameter:.75,length:100,weight_per_foot:1.634,modulus_of_elasticity:30.5e6}); invalidate();
    });
    loadExample(); runAnalysis(); document.body.dataset.appState = "ready";
  }

  try {initialize();} catch (error) {showError(error); document.body.dataset.appState = "error";}
})();
