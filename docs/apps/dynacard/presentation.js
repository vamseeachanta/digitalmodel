/* Display precision never changes the analysis or exported samples. */
(function(root) {
  "use strict";
  const decimals = {in:2, lbf:0, "%":1, psi:1, "lb/ft³":3, "lb/ft3":3,
    "psi/ft":4, "bbl/day":1, "ft·lbf":0, "h/day":0};

  function formatValue(value, unit = "") {
    if (typeof value !== "number") return value === null ? "Not established" : String(value);
    const places = decimals[unit.split(" · ")[0]];
    if (places === undefined) return String(value);
    return value.toLocaleString("en-US", {minimumFractionDigits:places, maximumFractionDigits:places});
  }

  function pumpCsv(card) {
    return "position_in,load_lbf\n" + card.position.map((value, index) =>
      `${value},${card.load[index]}\n`).join("");
  }

  // The page supplies runtime 24 h/day. The reference labels any
  // input_params runtime "card", including its 24 h default, so the export
  // restates the source as assumed and keeps the reference label beside it.
  function exportRecord(result, input) {
    const upload = input && input.source === "upload";
    return {...result,
      production: {...result.production, runtime_source: "assumed_24h"},
      page_inputs: {
        runtime: {hours_per_day: result.production.runtime_hours, source: "assumed_24h",
          basis: "Not read from the card or a well test; this page supplies 24 h/day.",
          reference_runtime_source: result.production.runtime_source},
        card_start: upload
          ? {source: "uploaded CSV", origin: "stroke_bottom", rotation_samples: input.rotation,
            basis: "Rotated to the minimum surface position before analysis; every start phase of the same cycle gives the same result."}
          : {source: "bundled example", origin: "stored", rotation_samples: 0,
            basis: "Stored sample origin retained for parity with the Python records; CPIP and fluid load depend on the start phase."}}};
  }

  const api = {formatValue, pumpCsv, exportRecord};
  root.DynacardPresentation = api;
  if (typeof module === "object" && module.exports) module.exports = api;
}(typeof globalThis !== "undefined" ? globalThis : this));
