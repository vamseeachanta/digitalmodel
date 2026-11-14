/* Exact default P1 and classifier arithmetic from the dynacard Python package.
 * Copyright (c) Digital Model Project. MIT license; see repository LICENSE. */
(function (root, factory) {
  "use strict";
  if (typeof module === "object" && module.exports) module.exports = factory();
  else root.DynacardAnalysis = factory();
}(typeof globalThis !== "undefined" ? globalThis : this, function () {
  "use strict";
  const sum = values => values.reduce((a, b) => a + b, 0);
  const minimum = values => values.reduce((a, b) => Math.min(a, b), Infinity);
  const maximum = values => values.reduce((a, b) => Math.max(a, b), -Infinity);
  const span = values => maximum(values) - minimum(values);
  const clip = (value, low, high) => Math.max(low, Math.min(high, value));
  const indices = n => Array.from({length: n}, (_, i) => i);
  const modulo = (value, n) => ((value % n) + n) % n;

  function extremumIndex(values, largest = true) {
    let best = 0;
    for (let i = 1; i < values.length; i += 1) {
      if (largest ? values[i] > values[best] : values[i] < values[best]) best = i;
    }
    return best;
  }

  function signedArea(card) {
    const {position: x, load: y} = card;
    return 0.5 * sum(x.map((value, i) => {
      const next = (i + 1) % x.length;
      return value * y[next] - x[next] * y[i];
    }));
  }

  function clockwise(card) {
    const source = indices(card.position.length);
    if (signedArea(card) > 0) source.reverse();
    return {position: source.map(i => card.position[i]),
      load: source.map(i => card.load[i]), source};
  }

  function cross(card, a, b, c) {
    const x = card.position, y = card.load;
    return (x[b] - x[a]) * (y[c] - y[a]) - (y[b] - y[a]) * (x[c] - x[a]);
  }

  function mergeCoplanar(card, hull) {
    // Qhull's 2-D distance roundoff and default visibility bound. A strict
    // cross-product test retains floating-point dents on nominally straight
    // edges, changing the vertex percentiles used by corners.py.
    const x = card.position, y = card.load;
    const maxX = maximum(x.map(Math.abs)), maxY = maximum(y.map(Math.abs));
    const maxAbs = Math.max(maxX, maxY);
    const maxDistanceSum = Math.min(Math.sqrt(2) * maxAbs, maxX + maxY);
    const distanceRoundoff = Number.EPSILON * (2 * maxDistanceSum * 1.01 + maxAbs);
    let changed = true;
    while (changed && hull.length > 2) {
      changed = false;
      for (let i = 0; i < hull.length; i += 1) {
        const a = hull[modulo(i - 1, hull.length)], b = hull[i], c = hull[(i + 1) % hull.length];
        const edge = Math.hypot(x[c] - x[a], y[c] - y[a]);
        if (cross(card, a, b, c) <= 2 * distanceRoundoff * edge) {
          hull.splice(i, 1); changed = true; break;
        }
      }
    }
    return hull;
  }

  function hullIndices(card) {
    // Monotone chain returns the same 2-D extreme vertices as ConvexHull.
    // Repeated closure samples retain the first source index.
    const x = card.position, y = card.load;
    const ordered = indices(x.length).sort((a, b) => x[a] - x[b] || y[a] - y[b] || a - b);
    const unique = ordered.filter((i, k) => k === 0 || x[i] !== x[ordered[k - 1]] || y[i] !== y[ordered[k - 1]]);
    const half = items => {
      const result = [];
      for (const i of items) {
        while (result.length > 1 && cross(card, result[result.length - 2], result[result.length - 1], i) <= 0) result.pop();
        result.push(i);
      }
      return result.slice(0, -1);
    };
    const hull = mergeCoplanar(card, half(unique).concat(half(unique.slice().reverse())));
    if (hull.length < 3) throw new Error("Degenerate pump-card convex hull; extrema fallback is not supported.");
    return hull;
  }

  function percentile(values, fraction) {
    const sorted = values.slice().sort((a, b) => a - b);
    const at = (sorted.length - 1) * fraction;
    const left = Math.floor(at), right = Math.ceil(at);
    return sorted[left] + (sorted[right] - sorted[left]) * (at - left);
  }

  function cyclicIndices(start, stop, n, includeStop = false) {
    return indices(modulo(stop - start, n) + (includeStop ? 1 : 0)).map(i => (start + i) % n);
  }

  function bottomRight(card) {
    const x = card.position, y = card.load;
    const xMin = minimum(x), xMax = maximum(x), yMin = minimum(y);
    const positionSpan = xMax - xMin, loadSpan = span(y);
    const top = indices(x.length).filter(i => x[i] === xMax);
    const bottom = indices(x.length).filter(i => x[i] === xMin);
    const topIndex = top[extremumIndex(top.map(i => y[i]))];
    const bottomIndex = bottom[extremumIndex(bottom.map(i => y[i]), false)];
    const down = cyclicIndices(topIndex, bottomIndex, x.length, true);
    if (down.length < 2 || positionSpan <= Number.EPSILON || loadSpan <= Number.EPSILON) return topIndex;
    const scores = down.map(i => (x[i] - xMin) / positionSpan - (y[i] - yMin) / loadSpan);
    let knee = extremumIndex(scores);
    const loads = down.map(i => y[i]), availableDrop = loads[0] - minimum(loads);
    if (availableDrop > Number.EPSILON) {
      const transferred = loads.map(value => (loads[0] - value) / availableDrop);
      if (transferred[knee] < 0.35) {
        const eligible = indices(down.length).filter(i => transferred[i] >= 0.35);
        if (eligible.length) knee = eligible[extremumIndex(eligible.map(i => scores[i]))];
      }
    }
    if (knee + 1 < down.length && y[down[knee]] - y[down[knee + 1]] > 0.01 * loadSpan) knee += 1;
    return down[knee];
  }

  function corners(card) {
    const canonical = clockwise(card), x = canonical.position, y = canonical.load;
    const hull = hullIndices(canonical), hullPositions = hull.map(i => x[i]);
    const lower = percentile(hullPositions, 0.25), upper = percentile(hullPositions, 0.75);
    const left = hull.filter(i => x[i] <= lower), right = hull.filter(i => x[i] >= upper);
    const bl = left[extremumIndex(left.map(i => y[i]), false)];
    const tl = left[extremumIndex(left.map(i => y[i]))];
    const tr = right[extremumIndex(right.map(i => y[i]))];
    const result = [bl, tl, tr, bottomRight(canonical)];
    result.sort((a, b) => modulo(a - bl, x.length) - modulo(b - bl, x.length));
    return result.map(i => canonical.source[i]);
  }

  function fluidLoad(card) {
    const canonical = clockwise(card), found = corners(canonical);
    if (new Set(found).size < 4) throw new Error("Fluid load requires distinct corner phases.");
    const meanPhase = (start, stop) => {
      const phase = cyclicIndices(start, stop, canonical.load.length);
      if (!phase.length) throw new Error("Fluid load requires distinct corner phases.");
      return sum(phase.map(i => canonical.load[i])) / phase.length;
    };
    const up = meanPhase(found[1], found[2]), down = meanPhase(found[3], found[0]);
    return {fluid_load: up - down, upstroke_load: up, downstroke_load: down};
  }

  function cpip(well, fluid) {
    const waterCut = well.well_test ? (well.well_test.water_cut ?? 0) : 50;
    const tubingPressure = well.input_params && well.input_params.tubing_pressure;
    if (!tubingPressure) throw new Error("A positive tubing pressure is required; the 100 psi fallback is not supported.");
    const oilSg = 141.5 / (35 + 131.5), wc = waterCut / 100;
    const mixedSg = 1.03 * wc + oilSg * (1 - wc);
    const gradient = 0.433 * mixedSg, density = mixedSg * 62.4;
    const discharge = tubingPressure + gradient * well.pump.depth;
    const pumpArea = Math.PI * well.pump.diameter ** 2 / 4;
    return {pump_intake_pressure: discharge - fluid.fluid_load / pumpArea,
      pump_discharge_pressure: discharge, fluid_density: density, tubing_gradient: gradient};
  }

  function production(well, fillage) {
    const area = Math.PI * well.pump.diameter ** 2 / 4;
    const gross = area * fillage.gross_stroke * well.spm * 1440 / 9702;
    const net = gross * (fillage.fillage / 100);
    const cardRuntime = well.input_params ? (well.input_params.runtime ?? 24) : null;
    const testRuntime = well.well_test ? (well.well_test.runtime ?? 24) : null;
    const runtime = cardRuntime || testRuntime || 24;
    const source = cardRuntime ? "card" : testRuntime ? "well_test" : "assumed_24h";
    const theoretical = net * (runtime / 24);
    const measured = well.well_test ? (well.well_test.oil_rate ?? 0) + (well.well_test.water_rate ?? 0) : 0;
    const efficiency = measured > 0 ? (theoretical > 0 ? measured / theoretical * 100 : 0) : (well.pump.efficiency ?? 1) * 100;
    return {gross_displacement: gross, net_displacement: net, theoretical_production: theoretical,
      pump_efficiency: efficiency, runtime_hours: runtime, runtime_source: source};
  }

  function calculate(card, well) {
    const found = corners(card), gross = span(card.position);
    const net = card.position[found[3]] - minimum(card.position);
    const fillage = {gross_stroke: gross, net_stroke: net,
      fillage: clip(gross > 0 ? net / gross * 100 : 0, 0, 100), corners: found};
    const fluid = fluidLoad(card);
    return {fluid_load: fluid, fillage, cpip: cpip(well, fluid),
      production: production(well, fillage), corners: found};
  }

  function binHalf(position, load, start, end) {
    return indices(8).map(bin => {
      const members = indices(end - start).map(i => i + start).filter(i =>
        position[i] >= bin / 8 && (bin === 7 ? position[i] <= 1 : position[i] < (bin + 1) / 8));
      return members.length ? sum(members.map(i => load[i])) / members.length : 0;
    });
  }

  function projections(card) {
    const x = card.position, y = card.load, n = x.length;
    const xSpan = span(x), ySpan = span(y);
    if (n < 4 || xSpan < 1e-10 || ySpan < 1e-10) throw new Error("A shape requires at least four points and nondegenerate position and load spans.");
    const xMin = minimum(x), yMin = minimum(y);
    const px = x.map(value => (value - xMin) / xSpan), py = y.map(value => (value - yMin) / ySpan);
    const peak = extremumIndex(x), midpoint = Math.floor(n / 2);
    const atEdge = peak < 2 || peak >= n - 2;
    return binHalf(px, py, 0, atEdge ? midpoint : peak + 1)
      .concat(binHalf(px, py, atEdge ? midpoint : peak, n));
  }

  function normalizedFeatures(card, model, shape) {
    let values = projections(card);
    if (!shape) values = values.concat([span(card.position), span(card.load), Math.abs(signedArea(card))]);
    if (!model || !model.scaling || model.scaling.min.length !== values.length) {
      throw new Error("Classifier model is missing or its feature count does not match; legacy classification is not supported.");
    }
    return values.map((value, i) => {
      const range = model.scaling.max[i] - model.scaling.min[i];
      return range < 1e-10 ? 0 : clip((value - model.scaling.min[i]) / range, 0, 1);
    });
  }

  function treeValue(features, tree) {
    let node = 0;
    while (tree.feature[node] >= 0) {
      node = features[tree.feature[node]] <= tree.threshold[node] ? tree.children_left[node] : tree.children_right[node];
    }
    return tree.value[node];
  }

  function diagnose(card, model, shape = false) {
    const features = normalizedFeatures(card, model, shape);
    const scores = model.init_value ? model.init_value.slice() : Array(model.n_classes).fill(0);
    for (const stage of model.trees) {
      stage.forEach((tree, i) => { scores[i] += model.learning_rate * treeValue(features, tree); });
    }
    const maxScore = maximum(scores), exponents = scores.map(value => Math.exp(value - maxScore));
    const total = sum(exponents), probabilities = exponents.map(value => value / total);
    const ranked = indices(probabilities.length).sort((a, b) => probabilities[b] - probabilities[a] || b - a);
    const rounded = value => Number(value.toFixed(4));
    return {classification: model.class_labels[extremumIndex(probabilities)],
      confidence: rounded(probabilities[ranked[0]]),
      differential: ranked.slice(0, 3).map(i => ({mode: model.class_labels[i], probability: rounded(probabilities[i])})),
      probabilities: Object.fromEntries(model.class_labels.map((label, i) => [label, probabilities[i]])),
      model_version: model.model_version || "1.0"};
  }

  return {calculate, diagnose, corners};
}));
