# ABOUTME: Build the dynacard troubleshooting use-case explorer (capabilities page).
# ABOUTME: Seven seeded rod-pump failure cases: card shape -> ML diagnosis -> field action.
"""Build the **dynacard troubleshooting explorer** — a self-contained, light-themed
page of practical sucker-rod-pump troubleshooting use cases, published on the
capabilities page under the "Artificial lift" section.

Each use case is generated deterministically from the dynacard module's own
18-mode synthetic card library (pinned seed), classified by the shipped
GradientBoosting model, and quantified with the P1 calculations (fluid load,
pump fillage, theoretical production). Classification is presented on the pump
(downhole) card — the classifier's documented contract
(``PumpDiagnostics.classify_card(downhole_card)``) and its training domain.

Outputs (committed frozen artifacts; served as static files under docs/api):

    docs/api/artificial-lift/dynacard-troubleshooting.html   # explorer page
    docs/api/artificial-lift/dynacard-troubleshooting.json   # frozen case data

The JSON is the drift guard: tests/marine_ops/artificial_lift/dynacard/
test_troubleshooting_usecases.py re-runs every case against the live classifier
and fails if a published diagnosis no longer matches.

Run:
    uv run python scripts/capabilities/build_dynacard_troubleshooting.py
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import (
    run_p1_calculations,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    PumpDiagnostics,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    AnalysisResults,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
    SurfaceUnit,
)
from digitalmodel.marine_ops.artificial_lift.field_health import (
    CRITICAL_CLASSIFICATIONS,
    FAILURE_CLASSIFICATIONS,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "artificial-lift"
_SEED = 711  # same seed as examples/workflows/dynacard-diagnostics/input.yml

# Well configuration mirrors the registered dynacard-diagnostics workflow input.
_WELL = dict(
    rod=dict(diameter=1.0, length=5000.0),
    pump=dict(diameter=1.75, depth=5000.0),
    surface_unit=dict(
        manufacturer="Sim",
        unit_type="Test",
        stroke_length=144.0,
        gear_box_rating=640000.0,
    ),
    spm=10.0,
)

# The practical troubleshooting use cases. Symptom = what the operator sees at
# the wellsite; actions = standard rod-pump dynamometer-analysis responses.
# Every mode here must classify back to itself (the drift test enforces it).
USE_CASES: list[dict] = [
    dict(
        mode="NORMAL",
        title="Healthy baseline",
        symptom=(
            "Full, smooth parallelogram-shaped card. Steady production, loads "
            "well inside rod and gearbox ratings. This is the reference shape "
            "every other case is compared against."
        ),
        actions=[
            "No intervention — record the card as the well's healthy reference",
            "Trend fillage and peak/minimum load for early drift detection",
        ],
    ),
    dict(
        mode="GAS_INTERFERENCE",
        title="Gas interference",
        symptom=(
            "Rounded lower-right corner: the traveling valve opens late on the "
            "downstroke because free gas in the barrel must compress before the "
            "load can transfer. Production is down while the pump card looks "
            "“spongy”."
        ),
        actions=[
            "Lower the pump intake below the perforations if the completion allows",
            "Install or service a downhole gas separator (gas anchor)",
            "Reduce SPM to give the annulus time to separate gas",
        ],
    ),
    dict(
        mode="FLUID_POUND",
        title="Fluid pound (pumped-off well)",
        symptom=(
            "Abrupt load drop partway down the downstroke: the plunger strikes "
            "the fluid level in a partially filled barrel. Audible pounding, "
            "shock loading on rods, tubing and gearbox."
        ),
        actions=[
            "Slow the unit or shorten the stroke to match pump displacement to inflow",
            "Install a pump-off controller or variable-speed drive",
            "Verify fluid level and inflow before resizing the pump",
        ],
    ),
    dict(
        mode="VALVE_LEAK_TV",
        title="Traveling valve leak (worn pump)",
        symptom=(
            "Load picks up slowly and decays on the upstroke: fluid slips back "
            "past the traveling valve or plunger fit, so the rods never carry "
            "the full fluid load. Gradual production decline."
        ),
        actions=[
            "Confirm with a standing-valve / traveling-valve dynamometer check",
            "Pull the pump; repair or replace the traveling valve, seat and plunger",
            "Review produced-solids and corrosion history before re-running",
        ],
    ),
    dict(
        mode="PUMP_TAGGING",
        title="Pump tagging (spacing fault)",
        symptom=(
            "Sharp load spike at the stroke end: the plunger strikes the "
            "standing valve or the top of the pump. Audible metallic tag; "
            "risk of valve, plunger and rod damage every stroke."
        ),
        actions=[
            "Stop and re-space the plunger off bottom",
            "Re-check spacing after the well reaches operating temperature",
            "Verify the surface stroke setting matches the pump design",
        ],
    ),
    dict(
        mode="ROD_PARTING",
        title="Parted rod string",
        symptom=(
            "Card collapses to a thin, low-load band with near-zero area: the "
            "pump no longer carries fluid load because the rod string is "
            "parted. Production stops; polished-rod load drops sharply."
        ),
        actions=[
            "Stop the unit immediately to avoid compounding downhole damage",
            "Fish the parted rod string and inspect the break face",
            "Review rod loading, corrosion program and service factor before re-running",
        ],
    ),
    dict(
        mode="GAS_LOCK",
        title="Gas-locked pump",
        symptom=(
            "Near-zero card area with a gas-cushion signature: gas trapped "
            "between the valves compresses and re-expands each stroke, so "
            "neither valve cycles and the pump stops delivering."
        ),
        actions=[
            "Re-space the pump to raise the compression ratio and break the lock",
            "Install a gas separator or relocate the intake below the perforations",
            "Consider a variable slippage or gas-lock-resistant pump design",
        ],
    ),
]


def _health_status(mode: str) -> str:
    if mode == "NORMAL":
        return "normal"
    if mode in FAILURE_CLASSIFICATIONS:
        return "failure"
    if mode in CRITICAL_CLASSIFICATIONS:
        return "critical"
    return "warning"


def _shoelace_area(position: list[float], load: list[float]) -> float:
    x = np.asarray(position)
    y = np.asarray(load)
    return float(abs(np.dot(x, np.roll(y, -1)) - np.dot(y, np.roll(x, -1))) / 2.0)


def _build_context(mode: str, card) -> DynacardAnalysisContext:
    return DynacardAnalysisContext(
        api14=f"SIM-{mode}-{_SEED}",
        surface_card=card,
        rod_string=[RodSection(**_WELL["rod"])],
        pump=PumpProperties(**_WELL["pump"]),
        surface_unit=SurfaceUnit(**_WELL["surface_unit"]),
        spm=_WELL["spm"],
    )


def build_cases() -> list[dict]:
    """Run every use case through the classifier and P1 calculations.

    Published metrics are the load-based quantities that genuinely
    discriminate between these card signatures (fluid load, peak/min load,
    card area vs the healthy reference). The P1 corner-detection fillage is
    not a reliable discriminator on the synthetic library shapes, so it is
    deliberately not published.
    """
    diagnostics = PumpDiagnostics()
    healthy_area = _shoelace_area(
        ALL_GENERATORS["NORMAL"](seed=_SEED).position,
        ALL_GENERATORS["NORMAL"](seed=_SEED).load,
    )
    cases = []
    for spec in USE_CASES:
        mode = spec["mode"]
        card = ALL_GENERATORS[mode](seed=_SEED)
        ctx = _build_context(mode, card)
        results = AnalysisResults(ctx=ctx, downhole_card=card)
        diag = diagnostics.classify_with_context(results)
        p1 = run_p1_calculations(ctx, card)
        area = _shoelace_area(card.position, card.load)
        cases.append(
            dict(
                mode=mode,
                seed=_SEED,
                title=spec["title"],
                severity=_health_status(mode),
                symptom=spec["symptom"],
                actions=spec["actions"],
                classification=diag.classification,
                confidence=round(diag.confidence, 4),
                differential=diag.differential,
                model_version=diag.model_version,
                metrics=dict(
                    fluid_load_lbs=round(p1["fluid_load"].fluid_load, 0),
                    peak_load_lbs=round(max(card.load), 0),
                    min_load_lbs=round(min(card.load), 0),
                    card_area_inlb=round(area, 0),
                    area_vs_healthy_pct=round(area / healthy_area * 100.0, 1),
                ),
                card=dict(
                    position=[round(p, 2) for p in card.position],
                    load=[round(v, 1) for v in card.load],
                ),
            )
        )
    return cases


_PAGE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Dynacard troubleshooting explorer — digitalmodel</title>
<style>
  :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#ffffff;--ink:#13233f;
        --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;
        --series-card:#2563C4;--series-ref:#0B8F80;
        --st-normal:#1f9d57;--st-warning:#b7791f;--st-critical:#c2410c;--st-failure:#b91c1c}
  *{box-sizing:border-box;margin:0;padding:0}
  body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
       color:var(--ink);line-height:1.5;padding:26px 22px 60px}
  main{max-width:1160px;margin:0 auto}
  a{color:var(--navy)}
  h1{font-size:26px;color:var(--navy);letter-spacing:-.3px}
  .sub{color:var(--muted);font-size:14.5px;margin:8px 0 4px;max-width:860px}
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
  .chartwrap{position:relative}
  svg{display:block;width:100%;height:auto}
  .legend{display:flex;gap:18px;margin-top:8px;font-size:12.5px;color:var(--muted)}
  .legend .sw{display:inline-block;width:16px;height:3px;border-radius:2px;vertical-align:middle;margin-right:6px}
  .tooltip{position:absolute;pointer-events:none;background:#0e1726;color:#e8eef9;font-size:11.5px;
           padding:6px 9px;border-radius:7px;white-space:nowrap;display:none;z-index:5}
  .verdict{display:flex;align-items:center;gap:10px;flex-wrap:wrap}
  .chip{display:inline-flex;align-items:center;gap:7px;font-size:12.5px;font-weight:700;color:#fff;
        border-radius:9px;padding:5px 11px}
  .conf{font-size:12.5px;color:var(--muted)}
  .confbar{height:6px;background:var(--soft);border:1px solid var(--line);border-radius:4px;
           overflow:hidden;margin:8px 0 2px}
  .confbar span{display:block;height:100%;background:linear-gradient(90deg,var(--teal),var(--navy))}
  .diff{margin-top:10px;font-size:12.5px;color:var(--muted)}
  .diff b{color:var(--ink)}
  .sym{font-size:13.5px;color:var(--ink);margin-top:12px}
  .metrics{display:grid;grid-template-columns:repeat(3,1fr);gap:10px;margin-top:14px}
  @media(max-width:520px){.metrics{grid-template-columns:repeat(2,1fr)}}
  .m{background:var(--soft);border:1px solid var(--line);border-radius:10px;padding:9px 11px}
  .m .v{font-size:17px;font-weight:800;color:var(--navy);font-variant-numeric:tabular-nums}
  .m .l{font-size:10.5px;color:var(--muted);margin-top:2px;text-transform:uppercase;letter-spacing:.4px}
  .actions{margin-top:14px}
  .actions li{font-size:13.5px;list-style:none;position:relative;padding:5px 0 5px 22px;
              border-bottom:1px solid var(--line)}
  .actions li:last-child{border-bottom:none}
  .actions li:before{content:"";position:absolute;left:3px;top:12px;width:8px;height:8px;
                     border-radius:50%;background:var(--teal)}
  .tablewrap{overflow-x:auto;margin-top:26px}
  table{width:100%;border-collapse:collapse;font-size:13px;background:var(--panel);
        border:1px solid var(--line);border-radius:12px;overflow:hidden}
  th,td{padding:8px 11px;text-align:left;border-bottom:1px solid var(--line);white-space:nowrap}
  th{background:var(--soft);color:var(--muted);font-weight:700;font-size:11px;
     text-transform:uppercase;letter-spacing:.4px}
  tr:last-child td{border-bottom:none}
  td.num{font-variant-numeric:tabular-nums;text-align:right}
  .stlabel{display:inline-flex;align-items:center;gap:6px;font-weight:700}
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
  <h1>Dynacard troubleshooting explorer</h1>
  <p class="sub">Seven practical sucker-rod-pump troubleshooting cases. Each pump card is generated
  deterministically from the dynacard module's 18-mode synthetic library (seed __SEED__), classified by the
  shipped GradientBoosting diagnostic model, and quantified with the module's fluid-load and card-shape
  calculations. Pick a case to see the card signature, the diagnosis and the recommended
  field response.</p>

  <div class="tabs" id="tabs"></div>

  <div class="layout">
    <div class="panel">
      <h2>Pump (downhole) card — load vs position</h2>
      <div class="chartwrap" id="chart"></div>
      <div class="legend">
        <span><span class="sw" style="background:var(--series-card)"></span>This case</span>
        <span><span class="sw" style="background:var(--series-ref)"></span>Healthy reference (NORMAL)</span>
      </div>
    </div>
    <div class="panel">
      <h2>Diagnosis &amp; recommended response</h2>
      <div id="verdict"></div>
    </div>
  </div>

  <div class="tablewrap">
    <table>
      <thead><tr><th>Case</th><th>Status</th><th>ML classification</th><th>Confidence</th>
        <th>Fluid load</th><th>Peak / min load</th><th>Card area</th><th>Area vs healthy</th></tr></thead>
      <tbody id="tbody"></tbody>
    </table>
  </div>

  <div class="prov">
    <b>Provenance.</b> Cards, diagnoses and metrics are produced by
    <code>digitalmodel.marine_ops.artificial_lift.dynacard</code> and frozen by
    <code>scripts/capabilities/build_dynacard_troubleshooting.py</code>; a CI test re-runs every case
    against the live classifier so a published diagnosis cannot silently drift.
    Classification operates on the pump (downhole) card — the classifier's documented contract — using a
    GradientBoosting model over 16 Bezerra vertical-projection features, trained on 5,400 synthetic cards
    (89.4% five-fold cross-validated accuracy on that synthetic set). Cards shown are synthetic training-library
    signatures, not field data; surface-to-downhole conversion (Gibbs frequency-domain and finite-difference
    solvers) is available in the same module. Severity follows the module's field-health convention.
    Source: <a href="https://github.com/vamseeachanta/digitalmodel/tree/main/src/digitalmodel/marine_ops/artificial_lift/dynacard">dynacard module</a> ·
    data: <a href="dynacard-troubleshooting.json">frozen JSON</a>.
  </div>
</main>

<script id="case-data" type="application/json">__DATA__</script>
<script>
(function(){
  "use strict";
  var DATA = JSON.parse(document.getElementById("case-data").textContent);
  var CASES = DATA.cases;
  var REF = null;
  CASES.forEach(function(c){ if(c.mode === "NORMAL") REF = c.card; });
  var ST = {normal:"var(--st-normal)", warning:"var(--st-warning)",
            critical:"var(--st-critical)", failure:"var(--st-failure)"};
  var STICON = {normal:"✓", warning:"⚠", critical:"⚠", failure:"✕"};
  var NS = "http://www.w3.org/2000/svg";
  var active = 0;

  function el(tag, attrs, parent){
    var e = document.createElementNS(NS, tag);
    for(var k in attrs) e.setAttribute(k, attrs[k]);
    if(parent) parent.appendChild(e);
    return e;
  }
  function fmt(n){ n = Number(n); if(Object.is(n, -0) || Math.abs(n) < 1e-9) n = 0;
    return n.toLocaleString("en-US"); }

  function ticks(lo, hi, n){
    var span = hi - lo, step = Math.pow(10, Math.floor(Math.log10(span/n)));
    var err = span/(n*step);
    if(err >= 7.5) step *= 10; else if(err >= 3.5) step *= 5; else if(err >= 1.5) step *= 2;
    var out = [], v = Math.ceil(lo/step)*step;
    for(; v <= hi + 1e-9; v += step) out.push(v);
    return out;
  }

  function drawChart(idx){
    var c = CASES[idx];
    var wrap = document.getElementById("chart");
    wrap.innerHTML = "";
    var W = 640, H = 420, m = {t:14, r:16, b:44, l:74};
    var xs = c.card.position.concat(REF.position), ys = c.card.load.concat(REF.load);
    var xlo = Math.min.apply(null, xs), xhi = Math.max.apply(null, xs);
    var ylo = Math.min.apply(null, ys), yhi = Math.max.apply(null, ys);
    var ypad = (yhi - ylo) * 0.07 || 1; ylo -= ypad; yhi += ypad;
    var xpad = (xhi - xlo) * 0.04 || 1; xlo -= xpad; xhi += xpad;
    function X(v){ return m.l + (v - xlo)/(xhi - xlo)*(W - m.l - m.r); }
    function Y(v){ return H - m.b - (v - ylo)/(yhi - ylo)*(H - m.t - m.b); }

    var svg = el("svg", {viewBox:"0 0 "+W+" "+H, role:"img",
      "aria-label":"Dynamometer card, load versus position, case "+c.title}, wrap);

    ticks(ylo, yhi, 6).forEach(function(v){
      el("line", {x1:m.l, x2:W-m.r, y1:Y(v), y2:Y(v), stroke:"#e5ecf6", "stroke-width":1}, svg);
      el("text", {x:m.l-8, y:Y(v)+4, "text-anchor":"end", "font-size":11, fill:"#5b6b86"}, svg)
        .textContent = fmt(v);
    });
    ticks(xlo, xhi, 7).forEach(function(v){
      el("text", {x:X(v), y:H-m.b+18, "text-anchor":"middle", "font-size":11, fill:"#5b6b86"}, svg)
        .textContent = fmt(v);
    });
    el("line", {x1:m.l, x2:W-m.r, y1:H-m.b, y2:H-m.b, stroke:"#c9d6e8", "stroke-width":1}, svg);
    el("text", {x:(m.l+W-m.r)/2, y:H-8, "text-anchor":"middle", "font-size":11.5, fill:"#5b6b86"}, svg)
      .textContent = "Plunger position (in)";
    var yl = el("text", {x:16, y:(m.t+H-m.b)/2, "font-size":11.5, fill:"#5b6b86",
      transform:"rotate(-90 16 "+((m.t+H-m.b)/2)+")", "text-anchor":"middle"}, svg);
    yl.textContent = "Load (lbs)";

    function loop(card){
      return card.position.map(function(p,i){
        return (i ? "L" : "M") + X(p).toFixed(1) + " " + Y(card.load[i]).toFixed(1);
      }).join("") + "Z";
    }
    if(c.mode !== "NORMAL"){
      el("path", {d:loop(REF), fill:"none", stroke:"#0B8F80", "stroke-width":2,
                  "stroke-dasharray":"6 5", opacity:.85}, svg);
    }
    el("path", {d:loop(c.card), fill:"rgba(37,99,196,.07)", stroke:"#2563C4", "stroke-width":2.25,
                "stroke-linejoin":"round"}, svg);

    var tip = document.createElement("div");
    tip.className = "tooltip"; wrap.appendChild(tip);
    var hoverDot = el("circle", {r:4.5, fill:"#2563C4", stroke:"#fff", "stroke-width":2,
                                 style:"display:none"}, svg);
    svg.addEventListener("mousemove", function(ev){
      var r = svg.getBoundingClientRect();
      var sx = (ev.clientX - r.left) * W / r.width, sy = (ev.clientY - r.top) * H / r.height;
      var best = -1, bd = 1e12;
      for(var i = 0; i < c.card.position.length; i++){
        var dx = X(c.card.position[i]) - sx, dy = Y(c.card.load[i]) - sy, d = dx*dx + dy*dy;
        if(d < bd){ bd = d; best = i; }
      }
      if(best < 0 || bd > 45*45){ tip.style.display = "none"; hoverDot.style.display = "none"; return; }
      var px = X(c.card.position[best]), py = Y(c.card.load[best]);
      hoverDot.setAttribute("cx", px); hoverDot.setAttribute("cy", py);
      hoverDot.style.display = "";
      tip.innerHTML = "<b>" + fmt(c.card.load[best]) + " lbs</b> @ " +
                      fmt(c.card.position[best]) + " in";
      tip.style.display = "block";
      var wr = wrap.getBoundingClientRect();
      tip.style.left = Math.min(px/W*wr.width + 12, wr.width - 130) + "px";
      tip.style.top = (py/H*wr.height - 34) + "px";
    });
    svg.addEventListener("mouseleave", function(){
      tip.style.display = "none"; hoverDot.style.display = "none";
    });
  }

  function esc(s){
    return String(s).replace(/[&<>"]/g, function(ch){
      return {"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;"}[ch];
    });
  }

  function drawVerdict(idx){
    var c = CASES[idx];
    var diff = c.differential.slice(0, 3).map(function(d, i){
      return (i+1) + ". <b>" + esc(d.mode) + "</b> " + (d.probability*100).toFixed(1) + "%";
    }).join(" &nbsp;&middot;&nbsp; ");
    var acts = c.actions.map(function(a){ return "<li>" + esc(a) + "</li>"; }).join("");
    var mt = c.metrics;
    document.getElementById("verdict").innerHTML =
      '<div class="verdict"><span class="chip" style="background:' + ST[c.severity] + '">' +
        STICON[c.severity] + " " + esc(c.severity.toUpperCase()) + "</span>" +
        "<b>" + esc(c.classification) + "</b>" +
        '<span class="conf">' + (c.confidence*100).toFixed(1) + "% confidence</span></div>" +
      '<div class="confbar"><span style="width:' + (c.confidence*100).toFixed(1) + '%"></span></div>' +
      '<div class="diff">Differential: ' + diff + "</div>" +
      '<p class="sym">' + esc(c.symptom) + "</p>" +
      '<div class="metrics">' +
        '<div class="m"><div class="v">' + fmt(mt.fluid_load_lbs) + '</div><div class="l">Fluid load (lbs)</div></div>' +
        '<div class="m"><div class="v">' + fmt(mt.peak_load_lbs) + '</div><div class="l">Peak load (lbs)</div></div>' +
        '<div class="m"><div class="v">' + fmt(mt.min_load_lbs) + '</div><div class="l">Min load (lbs)</div></div>' +
        '<div class="m"><div class="v">' + fmt(mt.card_area_inlb) + '</div><div class="l">Card area (in·lb)</div></div>' +
        '<div class="m"><div class="v">' + mt.area_vs_healthy_pct.toFixed(1) + '%</div><div class="l">Area vs healthy</div></div>' +
      "</div>" +
      '<ul class="actions">' + acts + "</ul>";
  }

  function select(idx){
    active = idx;
    Array.prototype.forEach.call(document.querySelectorAll(".tab"), function(t, i){
      t.classList.toggle("active", i === idx);
    });
    drawChart(idx); drawVerdict(idx);
  }

  var tabs = document.getElementById("tabs");
  CASES.forEach(function(c, i){
    var b = document.createElement("button");
    b.className = "tab"; b.type = "button";
    b.innerHTML = '<span class="dot" style="background:' + ST[c.severity] + '"></span>' + esc(c.title);
    b.addEventListener("click", function(){ select(i); });
    tabs.appendChild(b);
  });

  var tbody = document.getElementById("tbody");
  CASES.forEach(function(c, i){
    var tr = document.createElement("tr");
    tr.innerHTML =
      "<td><a href=\\"#\\" data-i=\\"" + i + "\\">" + esc(c.title) + "</a></td>" +
      '<td><span class="stlabel" style="color:' + ST[c.severity] + '">' + STICON[c.severity] + " " +
        esc(c.severity) + "</span></td>" +
      "<td>" + esc(c.classification) + "</td>" +
      '<td class="num">' + (c.confidence*100).toFixed(1) + "%</td>" +
      '<td class="num">' + fmt(c.metrics.fluid_load_lbs) + " lbs</td>" +
      '<td class="num">' + fmt(c.metrics.peak_load_lbs) + " / " + fmt(c.metrics.min_load_lbs) + "</td>" +
      '<td class="num">' + fmt(c.metrics.card_area_inlb) + " in·lb</td>" +
      '<td class="num">' + c.metrics.area_vs_healthy_pct.toFixed(1) + "%</td>";
    tr.querySelector("a").addEventListener("click", function(ev){
      ev.preventDefault(); select(i); window.scrollTo({top:0, behavior:"smooth"});
    });
    tbody.appendChild(tr);
  });

  select(1); // open on gas interference — the most common field case
})();
</script>
</body>
</html>
"""


def render_page(cases: list[dict]) -> str:
    data = json.dumps({"cases": cases}, separators=(",", ":"))
    # </script> can never appear inside the JSON block.
    data = data.replace("</", "<\\/")
    return _PAGE.replace("__SEED__", str(_SEED)).replace("__DATA__", data)


def main() -> None:
    cases = build_cases()
    mismatched = [
        (c["mode"], c["classification"]) for c in cases if c["classification"] != c["mode"]
    ]
    if mismatched:
        raise SystemExit(
            f"Refusing to publish: use-case diagnosis drifted from its mode: {mismatched}"
        )
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    slim = [
        {k: v for k, v in c.items() if k != "card"} | {"n_points": len(c["card"]["position"])}
        for c in cases
    ]
    (_OUT_DIR / "dynacard-troubleshooting.json").write_text(
        json.dumps({"seed": _SEED, "well": _WELL, "cases": slim}, indent=2) + "\n",
        encoding="utf-8",
    )
    (_OUT_DIR / "dynacard-troubleshooting.html").write_text(render_page(cases), encoding="utf-8")
    print(
        f"Wrote {len(cases)} use cases -> "
        f"{(_OUT_DIR / 'dynacard-troubleshooting.html').relative_to(_REPO)} (+ .json)"
    )
    for c in cases:
        print(
            f"  {c['mode']:18s} -> {c['classification']:18s} "
            f"conf {c['confidence']:.2f}  fluid load {c['metrics']['fluid_load_lbs']:7.0f} lbs  "
            f"area vs healthy {c['metrics']['area_vs_healthy_pct']:5.1f}%"
        )


if __name__ == "__main__":
    main()
