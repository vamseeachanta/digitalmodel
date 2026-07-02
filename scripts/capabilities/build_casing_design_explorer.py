# ABOUTME: Build the production casing design explorer (capabilities page).
# ABOUTME: Eight candidate products checked against one reference frac well.
"""Build the **casing design explorer** — a self-contained, light-themed page
running the production-casing design checks (burst / collapse / tension /
triaxial) for eight candidate API 5CT products against one reference well,
published on the capabilities page under the "Well construction" section.

Every number is computed by :mod:`digitalmodel.well.tubulars.casing` (API 5C3
ratings) and :mod:`digitalmodel.well.tubulars.casing_design` (load cases,
design factors, frac allowable, NACE sour screen, connection classes) — the
page's JavaScript only reads the frozen JSON embedded in the page; no physics
is re-implemented client-side.

Outputs (committed frozen artifacts; served as static files under docs/api):

    docs/api/well/casing-design-explorer.html   # explorer page
    docs/api/well/casing-design-explorer.json   # frozen case data

The JSON is the drift guard: tests/well/tubulars/test_casing_design_explorer.py
re-runs every product through the live ``check_production_casing`` /
``max_frac_surface_pressure`` and fails if a published margin no longer matches.

Source: Hansen (Devon Energy), EPA Hydraulic Fracturing Technical Workshop —
Session 2: Well Design, 2011; ratings per API 5C3 / API 5CT; sour service per
NACE MR0175 / ISO 15156.

Run:
    uv run python scripts/capabilities/build_casing_design_explorer.py
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np

from digitalmodel.well.tubulars.casing import casing
from digitalmodel.well.tubulars.casing_design import (
    CONNECTION_CLASSES,
    DesignFactors,
    H2S_PARTIAL_PRESSURE_THRESHOLD_PSIA,
    NACE_MIN_SERVICE_TEMP_F,
    ProductionCasingWell,
    SOUR_TOTAL_PRESSURE_GAS_PSIA,
    SOUR_TOTAL_PRESSURE_OIL_PSIA,
    api_round_force_lbf,
    api_round_pressure_psi,
    burst_external_profile,
    check_production_casing,
    injection_internal_profile,
    max_frac_surface_pressure,
    shut_in_tubing_pressure,
    sour_service_screen,
    tubing_leak_internal_profile,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "well"

_SOURCE = (
    "Hansen (Devon Energy), EPA Hydraulic Fracturing Technical Workshop — "
    "Session 2: Well Design, 2011"
)
_STANDARDS = "API 5C3 · API 5CT · NACE MR0175"

# Reference production-casing well for the study (a typical frac'd gas well).
_WELL = dict(
    td_ft=12_000.0,
    mud_ppg=10.0,
    toc_ft=4_000.0,
    outer_shoe_ft=8_000.0,
    reservoir_pressure_psi=8_500.0,
    packer_fluid_ppg=9.0,
    frac_surface_pressure_psi=9_000.0,
    frac_fluid_ppg=8.6,
)

# Candidate products: (grade, OD in, nominal weight ppf) — chosen to span
# clear fails (14# J55) through comfortable passes (23# P110 / Q125).
_PRODUCTS: list[tuple[str, float, float]] = [
    ("J55", 5.5, 14.0),
    ("N80", 5.5, 17.0),
    ("P110", 5.5, 17.0),
    ("N80", 5.5, 20.0),
    ("P110", 5.5, 20.0),
    ("P110", 5.5, 23.0),
    ("Q125", 5.5, 23.0),
    ("P110", 7.0, 26.0),
]

_FRAC_ALLOWABLE_DF = 1.25  # burst DF used for the max-frac-pressure allowable

# Connection-family framing facts from the source presentation.
_CONNECTION_FACTS = [
    "Connections are less than 3% of the string length",
    "…but account for more than 90% of pipe failures",
    "…and 10–50% of the tubular cost",
    "API connection ratings (STC / LTC / BTC) are ultimate-strength based; "
    "premium connection ratings are yield based — not directly comparable",
]


def _label(od_in: float, weight_ppf: float, grade: str) -> str:
    size = {5.5: "5-1/2\"", 7.0: "7\""}.get(od_in, f"{od_in:g}\"")
    return f"{size} {weight_ppf:g}# {grade}"


def _round_check(res) -> dict:
    d = res.as_dict()
    d["rating"] = round(float(d["rating"]), 1)
    d["max_load"] = round(float(d["max_load"]), 1)
    d["min_design_factor"] = round(float(d["min_design_factor"]), 4)
    d["governing_depth_ft"] = round(float(d["governing_depth_ft"]), 1)
    d["required_design_factor"] = float(d["required_design_factor"])
    d["passes"] = bool(d["passes"])
    return d


def build_profiles(well: ProductionCasingWell) -> dict:
    """Shared (product-independent) pressure profiles on a fixed depth grid."""
    depths = np.linspace(0.0, well.td_ft, 61)  # step 200 ft; anchors on-grid
    leak = tubing_leak_internal_profile(
        well.reservoir_pressure_psi, well.td_ft, well.packer_fluid_ppg,
        well.td_ft, gas_gradient_psi_ft=well.gas_gradient_psi_ft)
    frac = injection_internal_profile(
        well.frac_surface_pressure_psi, well.frac_fluid_ppg, well.td_ft)
    external = burst_external_profile(
        well.mud_ppg, well.toc_ft, well.outer_shoe_ft, well.td_ft,
        mix_water_ppg=well.mix_water_ppg, pore_ppg=well.pore_ppg)
    leak_i = leak.at(depths)
    frac_i = frac.at(depths)
    ext = external.at(depths)

    def _r(a: np.ndarray) -> list[float]:
        return [round(float(v), 1) for v in a]

    return dict(
        depth_ft=_r(depths),
        tubing_leak_internal_psi=_r(leak_i),
        frac_internal_psi=_r(frac_i),
        external_psi=_r(ext),
        tubing_leak_net_psi=_r(leak_i - ext),
        frac_net_psi=_r(frac_i - ext),
    )


def build_study() -> dict:
    """Run the eight candidate products through the live design checks."""
    well = ProductionCasingWell(**_WELL)
    factors = DesignFactors()
    external = burst_external_profile(
        well.mud_ppg, well.toc_ft, well.outer_shoe_ft, well.td_ft,
        mix_water_ppg=well.mix_water_ppg, pore_ppg=well.pore_ppg)

    products = []
    for grade, od_in, weight_ppf in _PRODUCTS:
        product = casing(grade, od_in=od_in, weight_ppf=weight_ppf)
        checks = check_production_casing(product, weight_ppf, well)
        p_max = max_frac_surface_pressure(
            product, well.frac_fluid_ppg, external, well.td_ft,
            design_factor=_FRAC_ALLOWABLE_DF)
        products.append(dict(
            label=_label(od_in, weight_ppf, grade),
            grade=grade,
            od_in=od_in,
            weight_ppf=weight_ppf,
            wall_in=round(product.wt_in, 3),
            id_in=round(product.id_in, 3),
            d_over_t=round(product.d_over_t, 2),
            ratings=dict(
                burst_psi=api_round_pressure_psi(product.burst_psi),
                collapse_psi=api_round_pressure_psi(product.collapse_psi),
                body_yield_lbf=api_round_force_lbf(product.body_yield_lbf),
                collapse_regime=product.collapse_regime,
            ),
            checks={m: _round_check(r) for m, r in checks.items()},
            passes_all=bool(all(r.passes for r in checks.values())),
            max_frac_surface_pressure_psi=round(float(p_max), 1),
        ))

    # Golden Barlow worked example (5-1/2" 23# P110 -> 14,520 psi).
    barlow_product = casing("P110", od_in=5.5, weight_ppf=23.0)
    barlow = dict(
        label=_label(5.5, 23.0, "P110"),
        od_in=5.5,
        wall_in=round(barlow_product.wt_in, 3),
        min_yield_psi=barlow_product.grade.min_yield_psi,
        raw_psi=round(barlow_product.burst_psi, 1),
        rounded_psi=api_round_pressure_psi(barlow_product.burst_psi),
    )

    # NACE MR0175 sour-service screen + worked example.
    sour_example = sour_service_screen(100.0, 10_000.0, "gas")
    sour = dict(
        thresholds=dict(
            h2s_partial_psia=H2S_PARTIAL_PRESSURE_THRESHOLD_PSIA,
            total_pressure_gas_psia=SOUR_TOTAL_PRESSURE_GAS_PSIA,
            total_pressure_oil_psia=SOUR_TOTAL_PRESSURE_OIL_PSIA,
        ),
        min_service_temp_f={g: NACE_MIN_SERVICE_TEMP_F[g]
                            for g in sorted(NACE_MIN_SERVICE_TEMP_F)},
        example=dict(
            h2s_ppm=100.0,
            total_pressure_psia=10_000.0,
            well_type="gas",
            h2s_partial_psia=round(sour_example.h2s_partial_psia, 4),
            is_sour=bool(sour_example.is_sour),
            acceptable_grades=list(sour_example.acceptable_grades),
        ),
    )

    connections = [dict(
        name=c.name,
        full_name=c.full_name,
        threads_per_inch=c.threads_per_inch,
        tension_efficiency=list(c.tension_efficiency),
        strength_basis=c.strength_basis,
        notes=c.notes,
    ) for c in CONNECTION_CLASSES.values()]

    return dict(
        source=_SOURCE,
        standards=_STANDARDS,
        well=dict(_WELL),
        sitp_psi=round(shut_in_tubing_pressure(
            well.reservoir_pressure_psi, well.td_ft,
            well.gas_gradient_psi_ft), 1),
        design_factors=dict(
            burst=factors.burst,
            burst_low_sicp=factors.burst_low_sicp,
            sicp_threshold_psi=factors.sicp_threshold_psi,
            collapse=factors.collapse,
            tension=factors.tension,
            compression=factors.compression,
            triaxial=factors.triaxial,
        ),
        frac_allowable_df=_FRAC_ALLOWABLE_DF,
        profiles=build_profiles(well),
        products=products,
        barlow=barlow,
        sour=sour,
        connections=connections,
        connection_facts=_CONNECTION_FACTS,
    )


_PAGE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Casing design explorer — digitalmodel</title>
<script src="https://cdn.plot.ly/plotly-2.27.0.min.js" charset="utf-8"></script>
<style>
  :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#ffffff;--ink:#13233f;
        --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;
        --ok:#1f9d57;--bad:#b91c1c}
  *{box-sizing:border-box;margin:0;padding:0}
  body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
       color:var(--ink);line-height:1.5;padding:26px 22px 60px}
  main{max-width:1160px;margin:0 auto}
  a{color:var(--navy)}
  h1{font-size:26px;color:var(--navy);letter-spacing:-.3px}
  .sub{color:var(--muted);font-size:14.5px;margin:8px 0 4px;max-width:900px}
  .crumb{font-size:12.5px;margin-bottom:14px}
  .tabs{display:flex;gap:8px;flex-wrap:wrap;margin:20px 0 16px}
  .tab{display:flex;align-items:center;gap:8px;font-size:13px;font-weight:700;color:var(--ink);
       background:var(--panel);border:1px solid var(--line);border-radius:10px;
       padding:8px 13px;cursor:pointer}
  .tab:hover{border-color:var(--teal)}
  .tab.active{color:#fff;background:linear-gradient(135deg,var(--teal),var(--navy));border-color:transparent}
  .dot{width:9px;height:9px;border-radius:50%;flex:none}
  .tab.active .dot{outline:2px solid rgba(255,255,255,.7)}
  .layout{display:grid;grid-template-columns:minmax(0,7fr) minmax(0,5fr);gap:18px}
  @media(max-width:900px){.layout{grid-template-columns:1fr}}
  .panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;padding:16px 18px}
  .panel h2{font-size:12px;text-transform:uppercase;letter-spacing:1px;color:var(--muted);margin-bottom:10px}
  #chart{width:100%;height:470px}
  .verdict{display:flex;align-items:center;gap:10px;flex-wrap:wrap;margin-bottom:10px}
  .chip{display:inline-flex;align-items:center;gap:7px;font-size:12.5px;font-weight:700;color:#fff;
        border-radius:9px;padding:5px 11px}
  .metrics{display:grid;grid-template-columns:repeat(3,1fr);gap:10px;margin:12px 0}
  @media(max-width:520px){.metrics{grid-template-columns:repeat(2,1fr)}}
  .m{background:var(--soft);border:1px solid var(--line);border-radius:10px;padding:9px 11px}
  .m .v{font-size:16px;font-weight:800;color:var(--navy);font-variant-numeric:tabular-nums}
  .m .l{font-size:10.5px;color:var(--muted);margin-top:2px;text-transform:uppercase;letter-spacing:.4px}
  table{width:100%;border-collapse:collapse;font-size:13px;background:var(--panel);
        border:1px solid var(--line);border-radius:12px;overflow:hidden}
  th,td{padding:8px 11px;text-align:left;border-bottom:1px solid var(--line);white-space:nowrap}
  th{background:var(--soft);color:var(--muted);font-weight:700;font-size:11px;
     text-transform:uppercase;letter-spacing:.4px}
  tr:last-child td{border-bottom:none}
  td.num{font-variant-numeric:tabular-nums;text-align:right}
  .pass{color:var(--ok);font-weight:700}
  .fail{color:var(--bad);font-weight:700}
  .tablewrap{overflow-x:auto;margin-top:26px}
  .cols{display:grid;grid-template-columns:1fr 1fr;gap:18px;margin-top:26px}
  @media(max-width:900px){.cols{grid-template-columns:1fr}}
  .worked{background:var(--soft);border:1px solid var(--line);border-radius:10px;
          padding:11px 13px;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;
          font-size:12.5px;line-height:1.7;margin-top:8px}
  .facts li{font-size:13px;list-style:none;position:relative;padding:5px 0 5px 22px;
            border-bottom:1px solid var(--line)}
  .facts li:last-child{border-bottom:none}
  .facts li:before{content:"";position:absolute;left:3px;top:12px;width:8px;height:8px;
                   border-radius:50%;background:var(--teal)}
  .prov{margin-top:26px;background:var(--panel);border:1px solid var(--line);border-radius:14px;
        padding:15px 18px;font-size:12.5px;color:var(--muted)}
  .prov b{color:var(--ink)}
  code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;background:var(--soft);
       border:1px solid var(--line);padding:1px 5px;border-radius:5px;font-size:11.5px}
</style>
</head>
<body>
<main>
  <div class="crumb"><a href="../capabilities/">&larr; digitalmodel capabilities</a></div>
  <h1>Casing design explorer</h1>
  <p class="sub">Production-casing design study on a reference frac'd well — TD 12,000 ft TVD, 10.0 ppg mud,
  TOC 4,000 ft, outer shoe 8,000 ft, reservoir 8,500 psi, 9.0 ppg packer fluid, 9,000 psi frac surface
  pressure on 8.6 ppg fluid. Eight candidate API 5CT products are checked for burst (tubing leak + frac
  screen-out), full-evacuation collapse, buoyed-weight tension and Von Mises triaxial against the published
  operator design-factor minimums. Ratings per API 5C3; sour-service screen per NACE MR0175.
  Source: __SOURCE__.</p>

  <div class="tabs" id="tabs"></div>

  <div class="layout">
    <div class="panel">
      <h2>Pressure vs depth — load cases &amp; burst rating</h2>
      <div id="chart"></div>
    </div>
    <div class="panel">
      <h2>Design-factor verdict</h2>
      <div id="verdict"></div>
    </div>
  </div>

  <div class="tablewrap">
    <table>
      <thead><tr><th>Product</th><th>Verdict</th><th>Burst</th><th>Collapse (regime)</th>
        <th>Body yield</th><th>DF burst</th><th>DF collapse</th><th>DF tension</th>
        <th>DF triaxial</th><th>Max frac P<sub>surf</sub> @ DF __FRACDF__</th></tr></thead>
      <tbody id="tbody"></tbody>
    </table>
  </div>

  <div class="cols">
    <div class="panel">
      <h2>Golden worked example — Barlow burst, __BARLOW_LABEL__</h2>
      <p style="font-size:13px">API 5C3 internal yield with the 0.875 factor covering the 12.5%
      wall tolerance:</p>
      <div class="worked" id="barlow"></div>
      <p style="font-size:12.5px;color:var(--muted);margin-top:8px">Pressure ratings round to the nearest
      10 psi and axial ratings to the nearest 1,000 lbf (API published-rating convention).</p>
    </div>
    <div class="panel">
      <h2>Design-factor minimums (DF = rating / load)</h2>
      <table id="dftable"></table>
      <p style="font-size:12.5px;color:var(--muted);margin-top:8px">Published operator practice from the
      source presentation; every minimum is configurable in
      <code>DesignFactors</code>. This well's SICP is <span id="sitp"></span> psi, so the full
      burst minimum applies.</p>
    </div>
  </div>

  <div class="cols">
    <div class="panel">
      <h2>Sour-service screen — NACE MR0175 / ISO 15156</h2>
      <p style="font-size:13px">Sour service requires H<sub>2</sub>S partial pressure &gt;
      <b id="th-pp"></b> psia <i>and</i> total pressure &gt; <b id="th-gas"></b> psia (gas well) /
      <b id="th-oil"></b> psia (oil well).</p>
      <div class="worked" id="sour-example"></div>
      <table id="nacetable" style="margin-top:12px"></table>
    </div>
    <div class="panel">
      <h2>Connection classes — tension efficiency vs pipe body</h2>
      <table id="conntable"></table>
      <ul class="facts" id="connfacts" style="margin-top:12px"></ul>
    </div>
  </div>

  <div class="prov">
    <b>Provenance.</b> Ratings, load profiles and design factors are produced by
    <code>digitalmodel.well.tubulars.casing</code> (API 5C3 / API 5CT product catalog) and
    <code>digitalmodel.well.tubulars.casing_design</code> (load cases, design factors, frac allowable,
    NACE sour screen, connections) and frozen by
    <code>scripts/capabilities/build_casing_design_explorer.py</code>; a CI test re-runs every product
    through the live checks so a published margin cannot silently drift. This page's JavaScript only reads
    the frozen JSON — no physics is re-implemented client-side. Design-factor minimums, the Barlow worked
    example, the NACE thresholds / grade windows and the connection-class facts follow
    <b>__SOURCE__</b>; rating formulas per <b>API 5C3 / API 5CT</b>; sour limits per
    <b>NACE MR0175 / ISO 15156</b>.
    Source: <a href="https://github.com/vamseeachanta/digitalmodel/tree/main/src/digitalmodel/well/tubulars">well/tubulars module</a> ·
    data: <a href="casing-design-explorer.json">frozen JSON</a>.
  </div>
</main>

<script id="case-data" type="application/json">__DATA__</script>
<script>
(function(){
  "use strict";
  var DATA = JSON.parse(document.getElementById("case-data").textContent);
  var P = DATA.products, PR = DATA.profiles;
  var MODES = ["burst", "collapse", "tension", "triaxial"];
  var active = 0;

  function fmt(n, dp){
    n = Number(n);
    return n.toLocaleString("en-US", {minimumFractionDigits: dp || 0,
                                      maximumFractionDigits: dp || 0});
  }
  function esc(s){
    return String(s).replace(/[&<>"]/g, function(ch){
      return {"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;"}[ch];
    });
  }
  function verdictHtml(ok){
    return ok ? '<span class="pass">PASS</span>' : '<span class="fail">FAIL</span>';
  }

  // ---- chart (Plotly; traces come straight from the frozen profiles) ----
  function ratingTrace(p){
    return {
      x: [p.ratings.burst_psi, p.ratings.burst_psi],
      y: [PR.depth_ft[0], PR.depth_ft[PR.depth_ft.length - 1]],
      mode: "lines", name: "Burst rating " + p.label,
      line: {color: "#b91c1c", width: 2.5, dash: "dash"},
      hovertemplate: "burst rating %{x:,.0f} psi<extra></extra>"
    };
  }
  function drawChart(idx){
    var p = P[idx];
    var traces = [
      {x: PR.tubing_leak_internal_psi, y: PR.depth_ft, mode: "lines",
       name: "Internal — tubing leak", line: {color: "#2563C4", width: 2.5},
       hovertemplate: "%{x:,.0f} psi @ %{y:,.0f} ft<extra>tubing leak internal</extra>"},
      {x: PR.frac_internal_psi, y: PR.depth_ft, mode: "lines",
       name: "Internal — frac screen-out", line: {color: "#7c3aed", width: 2.5},
       hovertemplate: "%{x:,.0f} psi @ %{y:,.0f} ft<extra>frac internal</extra>"},
      {x: PR.external_psi, y: PR.depth_ft, mode: "lines",
       name: "External backup", line: {color: "#0B8F80", width: 2},
       hovertemplate: "%{x:,.0f} psi @ %{y:,.0f} ft<extra>external</extra>"},
      {x: PR.tubing_leak_net_psi, y: PR.depth_ft, mode: "lines",
       name: "Net — tubing leak", line: {color: "#2563C4", width: 1.5, dash: "dot"},
       hovertemplate: "%{x:,.0f} psi @ %{y:,.0f} ft<extra>tubing leak net</extra>"},
      {x: PR.frac_net_psi, y: PR.depth_ft, mode: "lines",
       name: "Net — frac screen-out", line: {color: "#7c3aed", width: 1.5, dash: "dot"},
       hovertemplate: "%{x:,.0f} psi @ %{y:,.0f} ft<extra>frac net</extra>"},
      ratingTrace(p)
    ];
    var layout = {
      margin: {t: 8, r: 14, b: 46, l: 64},
      xaxis: {title: {text: "Pressure (psi)", font: {size: 12}}, gridcolor: "#e5ecf6",
              zerolinecolor: "#c9d6e8", tickformat: ",.0f"},
      yaxis: {title: {text: "Depth (ft TVD)", font: {size: 12}}, autorange: "reversed",
              gridcolor: "#e5ecf6", tickformat: ",.0f"},
      legend: {orientation: "h", y: -0.16, font: {size: 11}},
      paper_bgcolor: "#ffffff", plot_bgcolor: "#ffffff",
      font: {family: '-apple-system,"Segoe UI",Roboto,Arial,sans-serif', color: "#13233f"}
    };
    Plotly.react("chart", traces, layout,
                 {displayModeBar: false, responsive: true});
  }

  // ---- verdict panel ----
  function drawVerdict(idx){
    var p = P[idx];
    var rows = MODES.map(function(m){
      var c = p.checks[m];
      return "<tr><td>" + esc(m) + "</td>" +
        '<td class="num">' + fmt(c.rating) + (m === "tension" ? " lbf" :
          m === "triaxial" ? " psi" : " psi") + "</td>" +
        '<td class="num">' + fmt(c.max_load) + "</td>" +
        '<td class="num">' + c.min_design_factor.toFixed(3) + "</td>" +
        '<td class="num">' + fmt(c.governing_depth_ft) + " ft</td>" +
        '<td class="num">' + c.required_design_factor.toFixed(2) + "</td>" +
        "<td>" + verdictHtml(c.passes) + "</td></tr>";
    }).join("");
    document.getElementById("verdict").innerHTML =
      '<div class="verdict"><span class="chip" style="background:' +
        (p.passes_all ? "var(--ok)" : "var(--bad)") + '">' +
        (p.passes_all ? "\\u2713 PASSES ALL MODES" : "\\u2715 FAILS") + "</span>" +
        "<b>" + esc(p.label) + "</b></div>" +
      '<div class="metrics">' +
        '<div class="m"><div class="v">' + fmt(p.ratings.burst_psi) + '</div><div class="l">Burst (psi)</div></div>' +
        '<div class="m"><div class="v">' + fmt(p.ratings.collapse_psi) + '</div><div class="l">Collapse (psi, ' + esc(p.ratings.collapse_regime) + ')</div></div>' +
        '<div class="m"><div class="v">' + fmt(p.ratings.body_yield_lbf) + '</div><div class="l">Body yield (lbf)</div></div>' +
        '<div class="m"><div class="v">' + p.id_in.toFixed(3) + '"</div><div class="l">ID</div></div>' +
        '<div class="m"><div class="v">' + p.d_over_t.toFixed(2) + '</div><div class="l">D/t</div></div>' +
        '<div class="m"><div class="v">' + fmt(p.max_frac_surface_pressure_psi) + '</div><div class="l">Max frac P (psi) @ DF ' + DATA.frac_allowable_df.toFixed(2) + '</div></div>' +
      "</div>" +
      "<table><thead><tr><th>Mode</th><th>Rating</th><th>Max load</th><th>Min DF</th>" +
      "<th>Governing depth</th><th>Req'd</th><th></th></tr></thead><tbody>" + rows +
      "</tbody></table>" +
      '<p style="font-size:12px;color:var(--muted);margin-top:8px">Governing burst case: ' +
      esc(p.checks.burst.load_case) + ". Collapse assumes full evacuation against a " +
      DATA.well.mud_ppg.toFixed(1) + " ppg external column.</p>";
  }

  function select(idx){
    active = idx;
    Array.prototype.forEach.call(document.querySelectorAll(".tab"), function(t, i){
      t.classList.toggle("active", i === idx);
    });
    drawChart(idx); drawVerdict(idx);
  }

  // ---- tabs ----
  var tabs = document.getElementById("tabs");
  P.forEach(function(p, i){
    var b = document.createElement("button");
    b.className = "tab"; b.type = "button";
    b.innerHTML = '<span class="dot" style="background:' +
      (p.passes_all ? "var(--ok)" : "var(--bad)") + '"></span>' + esc(p.label);
    b.addEventListener("click", function(){ select(i); });
    tabs.appendChild(b);
  });

  // ---- summary table ----
  var tbody = document.getElementById("tbody");
  P.forEach(function(p, i){
    var tr = document.createElement("tr");
    tr.innerHTML =
      "<td><a href=\\"#\\" data-i=\\"" + i + "\\">" + esc(p.label) + "</a></td>" +
      "<td>" + verdictHtml(p.passes_all) + "</td>" +
      '<td class="num">' + fmt(p.ratings.burst_psi) + " psi</td>" +
      '<td class="num">' + fmt(p.ratings.collapse_psi) + " psi (" + esc(p.ratings.collapse_regime) + ")</td>" +
      '<td class="num">' + fmt(p.ratings.body_yield_lbf) + " lbf</td>" +
      MODES.map(function(m){
        var c = p.checks[m];
        return '<td class="num ' + (c.passes ? "pass" : "fail") + '">' +
               c.min_design_factor.toFixed(2) + "</td>";
      }).join("") +
      '<td class="num">' + fmt(p.max_frac_surface_pressure_psi) + " psi</td>";
    tr.querySelector("a").addEventListener("click", function(ev){
      ev.preventDefault(); select(i); window.scrollTo({top: 0, behavior: "smooth"});
    });
    tbody.appendChild(tr);
  });

  // ---- Barlow worked example ----
  var B = DATA.barlow;
  document.getElementById("barlow").innerHTML =
    "P<sub>iy</sub> = 0.875 &times; 2 &times; Y<sub>p</sub> &times; t / D<br>" +
    "&nbsp;&nbsp;&nbsp;&nbsp;= 0.875 &times; 2 &times; " + fmt(B.min_yield_psi) +
    " &times; " + B.wall_in.toFixed(3) + " / " + B.od_in.toFixed(1) + "<br>" +
    "&nbsp;&nbsp;&nbsp;&nbsp;= " + fmt(B.raw_psi) +
    " psi &nbsp;&rarr;&nbsp; <b>" + fmt(B.rounded_psi) +
    " psi</b> (API rounding to the nearest 10 psi)";

  // ---- design-factor minimums ----
  var DF = DATA.design_factors;
  document.getElementById("dftable").innerHTML =
    "<thead><tr><th>Mode</th><th>Minimum DF</th></tr></thead><tbody>" +
    '<tr><td>Burst</td><td class="num">' + DF.burst.toFixed(2) + "</td></tr>" +
    '<tr><td>Burst (SICP &lt; ' + fmt(DF.sicp_threshold_psi) + ' psi)</td><td class="num">' +
      DF.burst_low_sicp.toFixed(2) + "</td></tr>" +
    '<tr><td>Collapse</td><td class="num">' + DF.collapse.toFixed(2) + "</td></tr>" +
    '<tr><td>Tension (on yield)</td><td class="num">' + DF.tension.toFixed(2) + "</td></tr>" +
    '<tr><td>Compression</td><td class="num">' + DF.compression.toFixed(2) + "</td></tr>" +
    '<tr><td>Triaxial (Von Mises)</td><td class="num">' + DF.triaxial.toFixed(2) + "</td></tr>" +
    "</tbody>";
  document.getElementById("sitp").textContent = fmt(DATA.sitp_psi);

  // ---- NACE sour panel ----
  var S = DATA.sour;
  document.getElementById("th-pp").textContent = S.thresholds.h2s_partial_psia;
  document.getElementById("th-gas").textContent = fmt(S.thresholds.total_pressure_gas_psia);
  document.getElementById("th-oil").textContent = fmt(S.thresholds.total_pressure_oil_psia);
  var E = S.example;
  document.getElementById("sour-example").innerHTML =
    "Example: " + fmt(E.h2s_ppm) + " ppm H<sub>2</sub>S at " + fmt(E.total_pressure_psia) +
    " psia (" + esc(E.well_type) + " well)<br>" +
    "p<sub>H2S</sub> = " + fmt(E.h2s_ppm) + " &times; " + fmt(E.total_pressure_psia) +
    " / 10<sup>6</sup> = " + E.h2s_partial_psia.toFixed(1) + " psia &gt; " +
    S.thresholds.h2s_partial_psia + " psia &rarr; <b>" +
    (E.is_sour ? "SOUR" : "not sour") + "</b><br>" +
    "SSC-acceptable grades (all temperatures): " + E.acceptable_grades.join(", ");
  var naceRows = Object.keys(S.min_service_temp_f).map(function(g){
    var t = S.min_service_temp_f[g];
    return "<tr><td>" + esc(g) + '</td><td class="num">' +
      (t === 0 ? "all temperatures" : "&ge; " + fmt(t) + " &deg;F") + "</td></tr>";
  }).join("");
  document.getElementById("nacetable").innerHTML =
    "<thead><tr><th>Grade</th><th>SSC-acceptable service temperature (NACE MR0175 Table A.3)</th></tr></thead>" +
    "<tbody>" + naceRows + "</tbody>";

  // ---- connections ----
  var connRows = DATA.connections.map(function(c){
    return "<tr><td><b>" + esc(c.name) + "</b> — " + esc(c.full_name) + "</td>" +
      '<td class="num">' + (c.threads_per_inch === null ? "—" : c.threads_per_inch) + "</td>" +
      '<td class="num">' + Math.round(c.tension_efficiency[0] * 100) + "–" +
        Math.round(c.tension_efficiency[1] * 100) + "%</td>" +
      "<td>" + esc(c.strength_basis) + "</td></tr>";
  }).join("");
  document.getElementById("conntable").innerHTML =
    "<thead><tr><th>Connection</th><th>Threads/in</th><th>Tension efficiency</th>" +
    "<th>Rating basis</th></tr></thead><tbody>" + connRows + "</tbody>";
  document.getElementById("connfacts").innerHTML =
    DATA.connection_facts.map(function(f){ return "<li>" + esc(f) + "</li>"; }).join("");

  select(5); // open on the worked-example product, 5-1/2" 23# P110
})();
</script>
</body>
</html>
"""


def render_page(study: dict) -> str:
    data = json.dumps(study, separators=(",", ":"))
    # </script> can never appear inside the JSON block.
    data = data.replace("</", "<\\/")
    return (_PAGE
            .replace("__SOURCE__", _SOURCE)
            .replace("__FRACDF__", f"{_FRAC_ALLOWABLE_DF:.2f}")
            .replace("__BARLOW_LABEL__", "5-1/2&quot; 23# P110")
            .replace("__DATA__", data))


def main() -> None:
    study = build_study()
    labels = [p["label"] for p in study["products"]]
    if len(set(labels)) != len(labels):
        raise SystemExit(f"Refusing to publish: duplicate product labels: {labels}")
    if not any(p["passes_all"] for p in study["products"]):
        raise SystemExit("Refusing to publish: no passing product in the study")
    if all(p["passes_all"] for p in study["products"]):
        raise SystemExit("Refusing to publish: no failing product in the study")
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    (_OUT_DIR / "casing-design-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    (_OUT_DIR / "casing-design-explorer.html").write_text(
        render_page(study), encoding="utf-8")
    print(
        f"Wrote {len(labels)} product cases -> "
        f"{(_OUT_DIR / 'casing-design-explorer.html').relative_to(_REPO)} (+ .json)"
    )
    for p in study["products"]:
        dfs = "  ".join(
            f"{m} {p['checks'][m]['min_design_factor']:.2f}"
            f"{'✓' if p['checks'][m]['passes'] else '✗'}"
            for m in ("burst", "collapse", "tension", "triaxial"))
        print(f"  {p['label']:18s} {'PASS' if p['passes_all'] else 'FAIL':4s}  {dfs}  "
              f"max frac {p['max_frac_surface_pressure_psi']:8.0f} psi")


if __name__ == "__main__":
    main()
