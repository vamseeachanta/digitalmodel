# ABOUTME: Generate the galvanic-compatibility heatmap explorer (docs/api/corrosion/)
# ABOUTME: — verdict matrices precomputed in Python, embedded as JSON; JS only draws.
"""Build the galvanic compatibility explorer (#1294).

All verdicts are computed by :mod:`digitalmodel.corrosion.galvanic_screening`
(MIL-STD-889 anodic-index method) and embedded as JSON — the page's JS only
selects an environment and draws, so the heatmap is the Python result by
construction.

Run:  uv run python scripts/corrosion/build_galvanic_explorer.py
"""

from __future__ import annotations

import json
from pathlib import Path

from digitalmodel.corrosion.galvanic_screening import (
    ANODIC_INDEX_V,
    ENVIRONMENTS,
    screen_couple,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT = _REPO / "docs" / "api" / "corrosion" / "galvanic-compatibility-explorer.html"

#: pretty axis labels for the module's metal keys
DISPLAY = {
    "gold": "Gold",
    "silver": "Silver",
    "nickel": "Nickel",
    "monel_nickel_copper": "Monel (Ni-Cu)",
    "copper": "Copper",
    "cupronickel": "Cupronickel",
    "brass": "Brass",
    "bronze": "Bronze",
    "stainless_316_passive": "316 SS (passive)",
    "stainless_304_passive": "304 SS (passive)",
    "carbon_steel": "Carbon steel",
    "cast_iron": "Cast iron",
    "aluminum_alloy": "Aluminum alloy",
    "cadmium_plated": "Cadmium (plated)",
    "zinc": "Zinc",
    "magnesium": "Magnesium",
}

ENV_LABEL = {
    "harsh_marine": "Harsh / marine (allowable ΔV = 0.15 V)",
    "normal_industrial": "Normal / industrial (allowable ΔV = 0.25 V)",
    "controlled_indoor": "Controlled / indoor (allowable ΔV = 0.50 V)",
}

_RANK = {"OK": 0, "MARGINAL": 1, "PROTECT": 2}


def build_data() -> dict:
    metals = list(ANODIC_INDEX_V)
    envs = {}
    for env, allow in ENVIRONMENTS.items():
        verdict, rank, delta, anodic = [], [], [], []
        for row in metals:
            v_r, k_r, d_r, a_r = [], [], [], []
            for col in metals:
                r = screen_couple(row, col, env)
                v_r.append(r.verdict)
                k_r.append(_RANK[r.verdict])
                d_r.append(round(r.delta_v, 2))
                a_r.append(DISPLAY[r.anodic] if r.anodic else "— (same index)")
            verdict.append(v_r)
            rank.append(k_r)
            delta.append(d_r)
            anodic.append(a_r)
        envs[env] = {
            "label": ENV_LABEL[env],
            "allowable_v": allow,
            "verdict": verdict,
            "rank": rank,
            "delta_v": delta,
            "anodic": anodic,
        }
    return {
        "metals": metals,
        "labels": [DISPLAY[m] for m in metals],
        "anodic_index_v": [ANODIC_INDEX_V[m] for m in metals],
        "environments": envs,
        "env_order": list(ENVIRONMENTS),
    }


_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Galvanic Compatibility Explorer — dissimilar-metal screening</title>
<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
<style>
  :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
        --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc}
  *{box-sizing:border-box;margin:0;padding:0}
  body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
       color:var(--ink);line-height:1.5;padding:26px 30px 60px;max-width:1180px;margin:0 auto}
  h1{font-size:24px;color:var(--navy);letter-spacing:-.3px}
  .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:940px}
  .controls{display:flex;flex-wrap:wrap;gap:14px;align-items:flex-end;background:var(--panel);
            border:1px solid var(--line);border-radius:12px;padding:12px 16px;margin-bottom:16px}
  .ctl label{display:block;font-size:11px;font-weight:700;color:var(--muted);
             text-transform:uppercase;letter-spacing:.4px;margin-bottom:3px}
  select{font-size:13.5px;padding:5px 8px;border:1px solid var(--line);border-radius:8px;
         background:var(--soft);color:var(--ink)}
  .legend{display:flex;gap:16px;font-size:12.5px;color:var(--muted);align-items:center}
  .chip{display:inline-block;width:13px;height:13px;border-radius:3px;margin-right:5px;
        vertical-align:-2px;border:1px solid var(--line)}
  .panel{background:var(--panel);border:1px solid var(--line);border-radius:12px;padding:10px}
  .panel h2{font-size:13px;color:var(--navy);padding:4px 8px 8px}
  .note{color:var(--muted);font-size:12px;margin-top:14px;max-width:960px}
  .note b{color:var(--navy)}
  a{color:var(--teal)}
  table.idx{border-collapse:collapse;font-size:12px;margin-top:10px}
  table.idx th,table.idx td{border:1px solid var(--line);padding:3px 9px;text-align:left}
  table.idx th{background:var(--soft);color:var(--navy)}
</style>
</head>
<body>
<h1>Galvanic Compatibility Explorer</h1>
<p class="sub">Dissimilar-metal couple screening by the published
<b>MIL-STD-889 anodic-index method</b>: a couple's risk statistic is the
difference ΔV between the two metals' anodic indices (V vs gold), judged
against an environment-dependent allowable. Every verdict is precomputed in
Python (<code>digitalmodel.corrosion.galvanic_screening</code>) — the page
only draws. Hover a cell for ΔV and the anodic (sacrificial) member.</p>

<div class="controls">
  <div class="ctl"><label>Environment</label><select id="env"></select></div>
  <div class="legend">
    <span><span class="chip" style="background:#d7efe1"></span>OK — ΔV ≤ allowable</span>
    <span><span class="chip" style="background:#fbe8c9"></span>MARGINAL — ≤ 0.05 V over
      (area-ratio / coating control)</span>
    <span><span class="chip" style="background:#f5d3ce"></span>PROTECT — insulate, coat
      cathode, or CP</span>
  </div>
</div>

<div class="panel"><h2 id="title">Couple verdicts</h2>
  <div id="chart" style="height:680px"></div></div>

<p class="note"><b>Basis (public sources only).</b> Anodic-index values are the
standard published MIL-STD-889 group values (gold 0.00 V &hellip; magnesium 1.75 V,
table below). Allowable-ΔV thresholds are the widely published rule of thumb:
<b>0.15&nbsp;V</b> harsh/marine, <b>0.25&nbsp;V</b> normal industrial,
<b>0.50&nbsp;V</b> controlled indoor environments. The metal with the larger
anodic index is anodic (sacrificial). <b>Area ratio:</b> a small anode coupled to
a large cathode is the dangerous configuration — anodic penetration rate scales
with the cathode/anode area ratio, so make fasteners the cathodic member.
<b>Practice caution:</b> series positions can invert in specific chemistries and
temperatures (e.g. zinc/steel polarity reversal above ~60&nbsp;°C in some waters);
this screen is a materials-selection first pass, not a substitute for
service-specific testing. No operator-proprietary matrix is used.</p>

<table class="idx"><tr><th>Metal / group</th><th>Anodic index (V)</th></tr>
__INDEX_ROWS__
</table>

<script>
const DATA = __DATA__;
const $ = id => document.getElementById(id);
$("env").innerHTML = DATA.env_order
  .map(e => `<option value="${e}">${DATA.environments[e].label}</option>`).join("");

// status tints (fills) + dark ink text in every cell (verdict word), so
// identity is never color-alone
const SCALE = [
  [0, "#d7efe1"], [1/3, "#d7efe1"],
  [1/3, "#fbe8c9"], [2/3, "#fbe8c9"],
  [2/3, "#f5d3ce"], [1, "#f5d3ce"],
];

function draw(){
  const env = $("env").value, E = DATA.environments[env];
  $("title").textContent = "Couple verdicts — " + E.label;
  const custom = E.delta_v.map((row,i) => row.map((dv,j) =>
    [dv.toFixed(2), E.anodic[i][j], E.verdict[i][j]]));
  Plotly.newPlot("chart", [{
    type: "heatmap",
    x: DATA.labels, y: DATA.labels,
    z: E.rank, zmin: -0.5, zmax: 2.5,
    colorscale: SCALE, showscale: false,
    xgap: 2, ygap: 2,
    text: E.verdict, texttemplate: "%{text}",
    textfont: {size: 9.5, color: "#13233f"},
    customdata: custom,
    hovertemplate: "%{y} + %{x}<br>ΔV = %{customdata[0]} V " +
      "(allowable " + E.allowable_v.toFixed(2) + " V)<br>" +
      "anodic member: %{customdata[1]}<br><b>%{customdata[2]}</b>" +
      "<extra></extra>",
  }], {
    margin: {l: 118, r: 10, t: 10, b: 100},
    paper_bgcolor: "#fff", plot_bgcolor: "#fff",
    xaxis: {tickangle: -40, tickfont: {size: 11}},
    yaxis: {autorange: "reversed", tickfont: {size: 11}},
    font: {size: 12, color: "#13233f"},
  }, {displayModeBar: false, responsive: true});
}
$("env").addEventListener("change", draw);
draw();
</script>
</body>
</html>
"""


def main() -> None:
    data = build_data()
    rows = "\n".join(
        f"<tr><td>{DISPLAY[m]}</td><td>{ANODIC_INDEX_V[m]:.2f}</td></tr>"
        for m in data["metals"]
    )
    html = (
        _TEMPLATE
        .replace("__INDEX_ROWS__", rows)
        .replace("__DATA__", json.dumps(data, separators=(",", ":")))
    )
    _OUT.parent.mkdir(parents=True, exist_ok=True)
    _OUT.write_text(html, encoding="utf-8")
    print(f"wrote {_OUT} ({_OUT.stat().st_size/1024:.1f} kB)")


if __name__ == "__main__":
    main()
