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

  const api = {formatValue, pumpCsv};
  root.DynacardPresentation = api;
  if (typeof module === "object" && module.exports) module.exports = api;
}(typeof globalThis !== "undefined" ? globalThis : this));
