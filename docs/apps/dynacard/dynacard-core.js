/* Default DynacardWorkflow port. Oilfield API; SI finite-difference kernel. */
(function (root, factory) {
  if (typeof module === "object" && module.exports) {
    module.exports = factory(require("./analysis.js"), require("./models.js"));
  } else { root.DynacardCore = factory(root.DynacardAnalysis, root.DynacardModels); }
}(typeof globalThis !== "undefined" ? globalThis : this, function (analysis, models) {
  "use strict";
  const sum = a => a.reduce((s, v) => s + v, 0);
  const min = a => a.reduce((s, v) => Math.min(s, v), Infinity);
  const max = a => a.reduce((s, v) => Math.max(s, v), -Infinity);
  const span = a => max(a) - min(a);
  const IN2M = 25.4 / 1000, FT2M = 1 / (1000 / (12 * 25.4));
  const N2LB = 0.000224809 * 1000, PSI2PA = 6894.7572931783;
  function positive(value, name) {
    if (typeof value !== "number" || !Number.isFinite(value) || value <= 0) {
      throw new Error(name + " must be a finite positive number.");
    }
    return value;
  }
  function validateCard(card) {
    if (!card || !Array.isArray(card.position) || !Array.isArray(card.load)) {
      throw new Error("Card position and load must be arrays.");
    }
    if (card.position.length !== card.load.length) {
      throw new Error("Position and load must have equal lengths.");
    }
    if (card.position.length < 4) { throw new Error("Card needs at least 4 samples."); }
    for (const axis of [card.position, card.load]) {
      if (!axis.every(v => typeof v === "number" && Number.isFinite(v))) {
        throw new Error("Card cells must be numeric finite values.");
      }
    }
    if (span(card.position) <= 0) { throw new Error("Card has zero stroke."); }
  }
  function parseCardCsv(text) {
    if (typeof text !== "string") { throw new Error("CSV input must be text."); }
    const lines = text.replace(/^\uFEFF/, "").split(/\r?\n/)
      .map(v => v.trim()).filter(v => v && !v.startsWith("#"));
    const header = (lines.shift() || "").split(",").map(v => v.trim());
    let pi = header.indexOf("position_in"), li = header.indexOf("load_lbf");
    if (pi < 0 || li < 0) {
      pi = header.indexOf("surface_position_in"); li = header.indexOf("surface_load_lbf");
    }
    if (pi < 0 || li < 0 || new Set(header).size !== header.length) {
      throw new Error("CSV header requires position_in,load_lbf (or surface_position_in,surface_load_lbf).");
    }
    const card = {position: [], load: []};
    const numeric = /^[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?$/;
    lines.forEach((line, i) => {
      const cells = line.split(",").map(v => v.trim());
      if (cells.length !== header.length) { throw new Error("CSV row " + (i + 2) + " has mismatched columns."); }
      if (!cells.every(cell => numeric.test(cell) && Number.isFinite(Number(cell)))) {
        throw new Error("CSV row " + (i + 2) + " has a nonnumeric or nonfinite cell.");
      }
      card.position.push(Number(cells[pi])); card.load.push(Number(cells[li]));
    });
    validateCard(card);
    return closeCycle(card);
  }
  // The periodic stencil reads the last sample as phase zero repeated. A CSV
  // without an exact closing row holds unique phases; repeat its first row so
  // dropping or keeping the closing row gives the same pump card.
  function closeCycle(card) {
    const last = card.position.length - 1;
    if (card.position[last] === card.position[0] && card.load[last] === card.load[0]) {
      return card;
    }
    return {position: [...card.position, card.position[0]], load: [...card.load, card.load[0]]};
  }
  // Repeatable start for uploaded cards. The reference solver applies upstroke
  // damping to the samples before the top-of-stroke index and downstroke
  // damping after it (everitt_jennings/solver.py, build_coefficients), which
  // follows the motion only when the cycle begins at the bottom of the stroke;
  // the corner and fluid-load phases also follow the stored order. The cycle
  // is therefore rotated to its minimum surface position. Ties choose the
  // lexicographically smallest (position, load) rotation, so every rotation of
  // the same closed cycle gives the same card. Bundled examples keep their
  // stored origin for parity with the Python records.
  function startAtStrokeBottom(card) {
    validateCard(card);
    const closed = closeCycle(card), n = closed.position.length - 1;
    const p = closed.position.slice(0, n), l = closed.load.slice(0, n), low = min(p);
    const less = (a, b) => {
      for (let k = 0; k < n; k++) {
        const i = (a + k) % n, j = (b + k) % n;
        if (p[i] !== p[j]) return p[i] < p[j];
        if (l[i] !== l[j]) return l[i] < l[j];
      }
      return false;
    };
    let start = -1;
    for (let i = 0; i < n; i++) {
      if (p[i] === low && (start < 0 || less(i, start))) start = i;
    }
    const position = [...p.slice(start), ...p.slice(0, start)];
    const load = [...l.slice(start), ...l.slice(0, start)];
    return {card: {position: [...position, position[0]], load: [...load, load[0]]}, rotation: start};
  }
  function validateUnsupported(well) {
    if (well.survey != null) {
      const angles = well.survey.inclination;
      const axes = [well.survey.measured_depth, angles, well.survey.azimuth];
      if (!axes.every(axis => Array.isArray(axis) && axis.length === angles?.length &&
          axis.length >= 2 && axis.every(v => typeof v === "number" && Number.isFinite(v)))) {
        throw new Error("Survey arrays must have equal lengths and at least two finite samples.");
      }
      if (!angles.every(v => v === 0)) {
        throw new Error("Deviation survey is not supported; the app assumes a vertical well.");
      }
      if (well.survey.measured_depth.some((v, i, a) => v < 0 || (i > 0 && v <= a[i - 1]))) {
        throw new Error("Survey measured depths must be nonnegative and strictly increasing.");
      }
    }
    const anchors = [well.tubing_anchored, well.anchored, well.tubing?.anchored, well.pump?.tubing_anchored];
    if (anchors.some(value => value !== undefined && typeof value !== "boolean")) {
      throw new Error("Tubing anchored declaration must be a boolean; unanchored tubing is not supported.");
    }
    if (anchors.some(value => value === false)) {
      throw new Error("Unanchored tubing is not supported; anchored tubing is assumed.");
    }
    if (well.solver_method && well.solver_method !== "everitt_jennings") {
      throw new Error("Only the default everitt_jennings solver is supported.");
    }
    if (well.use_diagnostic_card || ["damping", "tubing_id_in", "viscosity_cp", "n_nodes"].some(key => key in well)) {
      throw new Error("Custom solver settings are not supported; the default workflow is required.");
    }
    if (well.calc_params?.use_si_units) { throw new Error("Only oilfield input units are supported."); }
    if (well.calc_params && "coulomb_friction_coefficient" in well.calc_params) {
      const friction = well.calc_params.coulomb_friction_coefficient;
      if (typeof friction !== "number" || !Number.isFinite(friction) || friction < 0) {
        throw new Error("Coulomb friction coefficient must be a finite nonnegative number.");
      }
    }
  }
  function normalizeRod(rod, i) {
    const name = "Rod taper " + (i + 1);
    const density = rod.density ?? 490;
    if ((rod.material && rod.material.toLowerCase() !== "steel") || density !== 490 ||
        /fiberglass|composite|carbon|alumin/i.test(rod.grade || "")) {
      throw new Error(name + ": non-steel rod material is not supported.");
    }
    const diameter = positive(rod.diameter, name + " diameter");
    const length = positive(rod.length, name + " length");
    const modulus = positive(rod.modulus_of_elasticity ?? 30.5e6, name + " modulus");
    const weight = rod.weight_per_foot ?? 0;
    const weightPerFoot = weight === 0 ? Math.PI * (diameter / 2) ** 2 * density / 144 : weight;
    positive(weightPerFoot, name + " weight per foot");
    if (diameter >= 2.441 || 1.5 * diameter / 2.441 <= 0.381) {
      throw new Error(name + ": rod/tubing geometry is outside the damping correlation (coupling ratio > 0.381 required).");
    }
    return {...rod, diameter, length, density, weight_per_foot: weightPerFoot,
      modulus_of_elasticity: modulus};
  }
  function normalizeWell(well) {
    if (!well || typeof well !== "object") { throw new Error("Well inputs are required."); }
    validateUnsupported(well);
    positive(well.spm, "SPM"); positive(well.pump?.diameter, "Pump diameter");
    positive(well.pump?.depth, "Pump depth");
    const density = positive(well.fluid_density ?? 62.4, "Fluid density");
    if (!Array.isArray(well.rod_string) || !well.rod_string.length) {
      throw new Error("Rod string needs at least one taper.");
    }
    positive(well.input_params?.tubing_pressure, "Tubing pressure (the fallback is not supported)");
    for (const block of [well.input_params, well.well_test]) {
      if (block?.runtime !== undefined) {
        positive(block.runtime, "Runtime");
        if (block.runtime > 24) { throw new Error("Runtime cannot exceed 24 hours/day."); }
      }
    }
    if (well.well_test) {
      const wc = well.well_test.water_cut ?? 0;
      if (!Number.isFinite(wc) || wc < 0 || wc > 100) { throw new Error("Water cut must be between 0 and 100 percent."); }
      for (const key of ["oil_rate", "water_rate"]) {
        const rate = well.well_test[key] ?? 0;
        if (!Number.isFinite(rate) || rate < 0) { throw new Error(key + " must be finite and nonnegative."); }
      }
    }
    const efficiency = well.pump.efficiency ?? 1;
    if (!Number.isFinite(efficiency) || efficiency < 0 || efficiency > 1) {
      throw new Error("Pump efficiency must be between 0 and 1.");
    }
    return {...well, fluid_density: density, rod_string: well.rod_string.map(normalizeRod)};
  }
  // damping.py: concentric-annulus drag, default viscosity 0.01 Pa-s.
  function damping(rod, pumpDiameter) {
    const od = rod.diameter, bore = 2.441 * IN2M, mu = 0.01;
    const m = bore / od, m2 = pumpDiameter / od + 1e-6;
    const aRod = Math.PI * od ** 2 / 4, aWell = Math.PI * bore ** 2 / 4;
    const b1 = (m ** 2 - 1) / (2 * Math.log(m)) - 1;
    const b2 = m ** 4 - 1 - (m ** 2 - 1) ** 2 / Math.log(m), b3 = 1 / Math.log(m);
    const base = 1.3e4 * mu * (1.5 * od / bore - 0.381) ** 2.57 / (bore / 2 - od / 2);
    const k1 = base * (2.77 - 1.69 * (m ** 2 - 1) / (m2 ** 2 - 1));
    const k2 = base * (2.77 + 1.69 * (m ** 2 - 1));
    const n1 = Math.PI * mu * (b3 + 4 * b1 ** 2 / b2) +
      4 * mu * Math.PI * (m2 ** 2 - 1) ** 2 / b2 - 8 * Math.PI * mu * (m2 ** 2 - 1) * b1 / b2;
    const n2 = Math.PI * mu * (b3 + 4 * b1 ** 2 / b2) + 4 * mu * Math.PI / b2 + 8 * Math.PI * mu * b1 / b2;
    const scale = (aWell - aRod) / (2 * (m ** 2 - 1) ** 2 * rod.length);
    return [(n1 + scale * (m2 ** 2 - 1) ** 2 * k1) / (rod.density * aRod),
      (n2 + scale * k2) / (rod.density * aRod)];
  }
  // Stability of the depth march (derived here; the Python module codes no
  // limit, issue #4159). Without damping, marchRow is
  //   u[i+1,j] = 2u[i,j] - u[i-1,j] + r^2 (u[i,j+1] - 2u[i,j] + u[i,j-1]),
  //   r^2 = (rho A / EA) (dx/dt)^2 = (dx / (c dt))^2,  c = sqrt(E / rho).
  // A periodic mode exp(i theta j) grows by g per depth step, where
  // g + 1/g = 2 - 4 r^2 sin^2(theta/2); |g| = 1 for every theta only when
  // r <= 1. This is the Courant-Friedrichs-Lewy condition (Courant,
  // Friedrichs and Lewy, Math. Ann. 100, 1928) with depth as the marching
  // axis: the sampling interval must be no shorter than the time a stress
  // wave takes to cross one depth cell, dt >= dx / c, in every taper. Above
  // the limit the shortest-period content grows by about |g|^199 at the pump,
  // which stays finite and so passes the finite-value check.
  function checkSampling(rods, dx, dt, intervals, spm) {
    const slowest = min(rods.map(r => Math.sqrt(r.modulus / r.density)));
    const crossing = dx / slowest, courant = crossing / dt;
    if (courant > 1) {
      const limit = Math.floor((60 / spm) / crossing);
      throw new Error(`Card sampling is too fine for this rod string: ${intervals} intervals per cycle at ` +
        `${spm} strokes/min give a ${(dt * 1000).toPrecision(5)} ms time step, shorter than the ` +
        `${(crossing * 1000).toPrecision(5)} ms a stress wave takes to cross one of the 199 depth cells ` +
        `(Courant number ${courant.toFixed(4)} > 1). The depth march is unstable above this limit. ` +
        `Resample the card to at most ${limit} intervals per cycle.`);
    }
  }
  function simulation(card, well) {
    const rods = well.rod_string.map(r => ({diameter: r.diameter * IN2M,
      length: r.length * FT2M, modulus: r.modulus_of_elasticity * PSI2PA,
      density: r.weight_per_foot * 576 / (Math.PI * r.diameter ** 2) * 16.0185}));
    rods.forEach(r => {
      r.area = Math.PI * (r.diameter / 2) ** 2;
      r.damping = damping(r, well.pump.diameter * IN2M);
    });
    const length = sum(rods.map(r => r.length)), dx = length / 199;
    const dt = (60 / well.spm) / (card.position.length - 1);
    checkSampling(rods, dx, dt, card.position.length - 1, well.spm);
    const volume = sum(rods.map(r => r.length * r.area));
    const weight = sum(rods.map(r => r.density * r.area * r.length));
    const buoyantWeight = (weight - volume * well.fluid_density * 16.0185) * 9.80665;
    const position = card.position.map(v => -v * IN2M), lowest = min(position);
    const turns = position.map((v, i) => v === lowest ? i : -1).filter(i => i >= 0);
    const nodes = Array.from({length: 200}, (_, i) => {
      const x = i === 199 ? length : i * dx;
      let cut = 0, selected;
      for (const rod of rods) {
        if (cut <= x && x <= cut + rod.length) { selected = rod; }
        cut += rod.length;
      }
      if (!selected) { throw new Error("Rod grid could not be resolved."); }
      return selected;
    });
    return {nodes, dx, dt, position,
      upDn: Math.trunc(sum(turns) / turns.length),
      boundary: card.load.map(v => v / N2LB - buoyantWeight)};
  }
  function marchRow(previous, current, next, i, sim) {
    const {nodes, dx, dt, upDn} = sim, n = current.length;
    const ea = r => r.modulus * r.area;
    const plus = (ea(nodes[i + 1]) + ea(nodes[i])) / 2;
    const minus = (ea(nodes[i - 1]) + ea(nodes[i])) / 2;
    const mass = nodes[i].density * nodes[i].area;
    const c1 = (mass / plus) * (dx / dt) ** 2;
    for (let j = 0; j < n - 1; j++) {
      const left = j === 0 ? n - 2 : j - 1;
      const c2 = (mass * nodes[i].damping[j < upDn ? 0 : 1] / plus) * dx ** 2 / dt;
      next[j] = current[j] + (minus / plus) * current[j] - (minus / plus) * previous[j] +
        c1 * (current[j + 1] - 2 * current[j] + current[left]) +
        c2 * (current[j + 1] - current[left]) / 2;
    }
    next[n - 1] = next[0];
  }
  // Polynomial least-squares projection, degree 5. A scaled QR basis avoids
  // normal-equation conditioning; edge rows evaluate the first/last whole fit
  // exactly as scipy.signal.savgol_filter(mode='interp'), without padding.
  function smoothingWeights(window) {
    const half = (window - 1) / 2, q = [];
    for (let degree = 0; degree <= 5; degree++) {
      const column = Array.from({length: window}, (_, j) => ((j - half) / half) ** degree);
      for (const basis of q) {
        const dot = sum(column.map((v, j) => v * basis[j]));
        for (let j = 0; j < window; j++) { column[j] -= dot * basis[j]; }
      }
      const norm = Math.sqrt(sum(column.map(v => v * v)));
      q.push(column.map(v => v / norm));
    }
    return Array.from({length: window}, (_, i) =>
      Array.from({length: window}, (_, j) => sum(q.map(basis => basis[i] * basis[j]))));
  }
  function smooth(load) {
    let window = Math.min(21, load.length);
    if (window % 2 === 0) { window--; }
    if (window <= 5) { return load.slice(); }
    const half = (window - 1) / 2, weights = smoothingWeights(window);
    return load.map((_, i) => {
      const start = Math.max(0, Math.min(i - half, load.length - window));
      return sum(weights[i - start].map((w, j) => w * load[start + j]));
    });
  }
  function solve(card, well) {
    const sim = simulation(card, well), n = card.position.length;
    let previous = sim.position, current = previous.map((v, j) => v +
      sim.boundary[j] / (sim.nodes[0].modulus * sim.nodes[0].area / sim.dx));
    let beforePrevious;
    for (let i = 1; i < 199; i++) {
      const next = new Array(n);
      marchRow(previous, current, next, i, sim);
      beforePrevious = previous; previous = current; current = next;
    }
    const rod = sim.nodes[199], factor = rod.modulus * rod.area / sim.dx;
    const load = current.map((v, j) => factor * (v - previous[j]));
    load[0] = (rod.modulus * rod.area / (2 * sim.dx)) *
      (3 * current[0] - 4 * previous[0] + beforePrevious[0]);
    load[n - 1] = load[0];
    const filtered = smooth(load);
    filtered[n - 1] = filtered[0];
    const result = {position: current.map(v => -v * (1 / IN2M)), load: filtered.map(v => v * N2LB)};
    validateCard(result);
    return result;
  }
  function workPerStroke(card) {
    let area = 0;
    for (let i = 0; i < card.position.length; i++) {
      const j = (i + 1) % card.position.length;
      area += (card.position[j] - card.position[i]) * (card.load[j] + card.load[i]);
    }
    return Math.abs(area) / 2 / 12;
  }
  function validateFiniteResult(value, path = "result") {
    if (typeof value === "number" && !Number.isFinite(value)) {
      throw new Error("Analysis produced a non-finite value at " + path + "; check input magnitudes for overflow.");
    }
    if (value !== null && typeof value === "object") {
      for (const [key, child] of Object.entries(value)) {
        validateFiniteResult(child, path + "." + key);
      }
    }
  }
  function analyse(card, well) {
    validateCard(card);
    // Mirrors "fluid_density" in model_fields_set: the CPIP density basis
    // depends on whether the caller supplied a density, not on its value.
    const densitySupplied = well != null && well.fluid_density != null;
    well = normalizeWell(well);
    if (!analysis || !models?.context || !models?.shape) {
      throw new Error("Shipped analysis/model assets are missing; no fallback is supported.");
    }
    const pumpCard = solve(card, well), p1 = analysis.calculate(pumpCard, well, densitySupplied);
    const result = {...p1, downhole_card: pumpCard, surface_stroke: span(card.position),
      peak_polished_rod_load: max(card.load), minimum_polished_rod_load: min(card.load),
      pump_card_max_load: max(pumpCard.load), pump_card_min_load: min(pumpCard.load),
      polished_rod_work_per_stroke: workPerStroke(card),
      pump_work_per_stroke: workPerStroke(pumpCard), pump_fillage: p1.fillage.fillage,
      inferred_production: p1.production.theoretical_production,
      buckling_detected: min(pumpCard.load) < -500, solver_method: "everitt_jennings",
      solver_flags: [], max_inclination_deg: well.survey?.inclination?.length ? 0 : null,
      diagnosis: analysis.diagnose(pumpCard, models.context),
      shape_diagnosis: analysis.diagnose(pumpCard, models.shape, true)};
    validateFiniteResult(result);
    return result;
  }
  return {parseCardCsv, startAtStrokeBottom, analyse};
}));
