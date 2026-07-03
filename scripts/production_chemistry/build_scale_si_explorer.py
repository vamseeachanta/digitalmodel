# ABOUTME: Generate the mineral-scale SI explorer (docs/api/production/) —
# ABOUTME: SI trending + seawater-mixing curves precomputed in Python, embedded JSON.

"""Build the produced-water mineral-scale SI explorer (#1295).

All numbers are computed here by
:mod:`digitalmodel.production_chemistry.scale_si` and embedded as JSON —
the page's JS only draws, so the charts are the Python result by
construction (no chemistry duplicated in JS).

Demo data is synthetic: a made-up barium-rich formation water plus the
public standard-seawater major-ion composition (Millero et al. 2008).

Run:  uv run python scripts/production_chemistry/build_scale_si_explorer.py
"""

from __future__ import annotations

import json
from pathlib import Path

from digitalmodel.production_chemistry.scale_si import (
    SCALE_FAMILIES,
    BrineComposition,
    mixing_sweep,
    saturation_indices,
    si_profile,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT = _REPO / "docs" / "api" / "production" / "scale-si-explorer.html"

# --- Synthetic demo brines (no client data) --------------------------------

FORMATION_WATER = BrineComposition(
    na=31_000.0, k=400.0, mg=500.0, ca=2_000.0, sr=400.0, ba=250.0, fe=5.0,
    cl=53_000.0, so4=10.0, hco3=1_500.0, tds=89_000.0, ph=6.6,
    co2_mole_fraction_gas=0.02,
)

# Standard-seawater major ions — public reference composition
# (Millero, Feistel, Wright & McDougall, Deep-Sea Research I 55, 2008).
SEAWATER = BrineComposition(
    na=10_770.0, k=399.0, mg=1_290.0, ca=412.0, sr=7.9, ba=0.01, fe=0.002,
    cl=19_350.0, so4=2_712.0, hco3=142.0, tds=35_170.0, ph=8.1,
)

# Bottomhole -> wellhead path (synthetic demo well)
N_STEPS = 13
T_BH, T_WH = 250.0, 120.0     # degF
P_BH, P_WH = 9_000.0, 250.0   # psia
MIX_T_F, MIX_P_PSIA = 100.0, 150.0  # topside mixing state


def build_payload() -> dict:
    profile = [
        (
            T_BH + (T_WH - T_BH) * i / (N_STEPS - 1),
            P_BH + (P_WH - P_BH) * i / (N_STEPS - 1),
        )
        for i in range(N_STEPS)
    ]
    prof_df = si_profile(FORMATION_WATER, profile)

    fractions = [i / 40 for i in range(41)]
    mix_df = mixing_sweep(
        FORMATION_WATER, SEAWATER, fractions, t_f=MIX_T_F, p_psia=MIX_P_PSIA
    )

    wellhead = saturation_indices(FORMATION_WATER, T_WH, P_WH)

    def _round(values, nd=4):
        return [round(float(v), nd) for v in values]

    payload = {
        "families": list(SCALE_FAMILIES),
        "profile": {
            "step": [int(s) for s in prof_df["step"]],
            "t_f": _round(prof_df["t_f"], 1),
            "p_psia": _round(prof_df["p_psia"], 0),
            "si": {
                fam: _round(prof_df[f"si_{fam}"]) for fam in SCALE_FAMILIES
            },
            "dsi": {
                fam: _round(prof_df[f"dsi_{fam}"]) for fam in SCALE_FAMILIES
            },
        },
        "mixing": {
            "fraction_b": _round(mix_df["fraction_b"], 3),
            "si": {
                fam: _round(mix_df[f"si_{fam}"]) for fam in SCALE_FAMILIES
            },
            "t_f": MIX_T_F,
            "p_psia": MIX_P_PSIA,
        },
        "wellhead_basis": {fam: wellhead[fam].basis for fam in SCALE_FAMILIES},
        "brines": {
            "formation_water": {
                "label": "Synthetic formation water (Ba-rich, SO4-poor)",
                "tds_mg_l": FORMATION_WATER.tds_mg_l,
                "ba_mg_l": FORMATION_WATER.ba,
                "so4_mg_l": FORMATION_WATER.so4,
            },
            "seawater": {
                "label": "Standard seawater (Millero et al. 2008)",
                "tds_mg_l": SEAWATER.tds_mg_l,
                "ba_mg_l": SEAWATER.ba,
                "so4_mg_l": SEAWATER.so4,
            },
        },
    }
    return payload


HTML_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Mineral-Scale Saturation-Index Explorer</title>
<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
<style>
  :root {{
    --navy: #0B3D91;
    --teal: #0f8a7e;
    --bg: #eef3fa;
  }}
  * {{ box-sizing: border-box; }}
  body {{
    margin: 0; background: var(--bg); color: #1c2a3a;
    font-family: "Segoe UI", -apple-system, Roboto, Arial, sans-serif;
  }}
  header {{
    background: var(--navy); color: #fff; padding: 22px 28px;
  }}
  header h1 {{ margin: 0 0 6px; font-size: 1.45rem; }}
  header p {{ margin: 0; opacity: 0.85; font-size: 0.95rem; max-width: 70rem; }}
  .badge {{
    display: inline-block; background: var(--teal); color: #fff;
    border-radius: 4px; padding: 2px 8px; font-size: 0.75rem;
    margin-left: 8px; vertical-align: middle;
  }}
  main {{ max-width: 1180px; margin: 0 auto; padding: 20px 24px 40px; }}
  .panel {{
    background: #fff; border: 1px solid #d5e0ef; border-radius: 8px;
    padding: 18px 20px; margin-top: 20px;
    box-shadow: 0 1px 3px rgba(11, 61, 145, 0.08);
  }}
  .panel h2 {{ margin: 0 0 4px; color: var(--navy); font-size: 1.12rem; }}
  .panel p.sub {{ margin: 0 0 10px; color: #4a5a70; font-size: 0.88rem; }}
  .chart {{ width: 100%; min-height: 430px; }}
  .note {{
    background: #fff; border-left: 4px solid var(--teal);
    border-radius: 0 8px 8px 0; padding: 14px 18px; margin-top: 22px;
    font-size: 0.85rem; color: #33445c; line-height: 1.55;
    border-top: 1px solid #d5e0ef; border-right: 1px solid #d5e0ef;
    border-bottom: 1px solid #d5e0ef;
  }}
  .note strong {{ color: var(--navy); }}
  code {{ background: #eef3fa; padding: 1px 5px; border-radius: 3px; }}
</style>
</head>
<body>
<header>
  <h1>Produced-Water Mineral-Scale Saturation-Index Explorer
    <span class="badge">synthetic demo data</span></h1>
  <p>SI = log<sub>10</sub>(IAP / K<sub>sp,cond</sub>) per scale family in the
     Oddo&ndash;Tomson conditional-solubility framework &mdash; SI &gt; 0 means
     supersaturated (scaling tendency). Bottomhole&rarr;wellhead trending for a
     synthetic formation water, plus the classic seawater-injection mixing sweep.
     All values precomputed by
     <code>digitalmodel.production_chemistry.scale_si</code>; this page only draws.</p>
</header>
<main>
  <div class="panel">
    <h2>SI trending, bottomhole &rarr; wellhead</h2>
    <p class="sub">Synthetic demo brine (TDS 89&thinsp;g/L, Ba 250&thinsp;mg/L,
       HCO<sub>3</sub> 1500&thinsp;mg/L, 2&thinsp;mol% CO<sub>2</sub> in gas)
       produced from 250&thinsp;&deg;F / 9000&thinsp;psia to
       120&thinsp;&deg;F / 250&thinsp;psia. Calcite SI rises on drawdown as CO<sub>2</sub>
       leaves solution; barite SI rises as the brine cools.</p>
    <div id="profile-chart" class="chart"></div>
  </div>
  <div class="panel">
    <h2>Brine-mixing sweep (waterflood compatibility)</h2>
    <p class="sub">Synthetic Ba-rich formation water blended with standard
       seawater (Millero et al. 2008 reference composition) at
       100&thinsp;&deg;F / 150&thinsp;psia. Seawater sulfate meets formation-water
       barium: barite SI peaks at an interior mixing fraction &mdash; the classic
       incompatibility screen. Concentrations mix linearly by volume fraction
       (ideal mixing, no precipitation en route).</p>
    <div id="mixing-chart" class="chart"></div>
  </div>
  <div class="note">
    <strong>Basis &amp; honesty note.</strong> Framework: Oddo, J.E. &amp; Tomson, M.B.,
    &ldquo;Why Scale Forms in the Oil Field and Methods To Predict It&rdquo;,
    <em>SPE Production &amp; Facilities</em> 9(1), 1994 (SPE-21710-PA) &mdash; saturation
    indices from conditional solubility products correlated in temperature, pressure and
    ionic strength, with the calcite carbonate system handled through CO<sub>2</sub>
    partitioning (in-situ pH from the gas CO<sub>2</sub> fraction, not a surface pH).
    <strong>The coefficient sets used here are configurable, physically-consistent
    defaults</strong> anchored to standard 25&thinsp;&deg;C thermodynamic constants and
    validated against qualitative behavior tests; <strong>they are not the coefficient
    values printed in the 1994 paper</strong> &mdash; calibrate against a trusted dataset
    before quantitative use. Both brines are synthetic demo compositions (the seawater
    column uses the public standard-seawater reference composition of Millero, Feistel,
    Wright &amp; McDougall, <em>Deep-Sea Research I</em> 55, 2008). No proprietary
    (e.g. Pitzer-model workbook) data is used.
  </div>
</main>
<script>
const DATA = {payload_json};

const NAVY = "#0B3D91", TEAL = "#0f8a7e";
const FAMILY_COLORS = {{
  calcite: NAVY,
  barite: TEAL,
  celestite: "#c2571a",
  gypsum: "#7a2048",
  hemihydrate: "#b0779a",
  anhydrite: "#4a6fa5",
  halite: "#8a93a2",
}};
const FAMILY_DASH = {{ hemihydrate: "dot", anhydrite: "dash", halite: "dot" }};

const baseLayout = {{
  paper_bgcolor: "#ffffff",
  plot_bgcolor: "#ffffff",
  font: {{ family: "Segoe UI, Arial, sans-serif", color: "#1c2a3a", size: 13 }},
  margin: {{ l: 62, r: 20, t: 12, b: 58 }},
  legend: {{ orientation: "h", y: -0.18 }},
  shapes: [{{
    type: "line", xref: "paper", x0: 0, x1: 1, y0: 0, y1: 0,
    line: {{ color: "#9aa7b8", width: 1.5, dash: "dash" }},
  }}],
  annotations: [{{
    xref: "paper", x: 0.995, y: 0, yanchor: "bottom", xanchor: "right",
    text: "SI = 0 (saturation)", showarrow: false,
    font: {{ size: 11, color: "#66758a" }},
  }}],
}};

// Panel 1: SI vs depth step (bottomhole -> wellhead)
const prof = DATA.profile;
const profTraces = DATA.families.map((fam) => ({{
  x: prof.step,
  y: prof.si[fam],
  name: fam,
  mode: "lines+markers",
  line: {{ color: FAMILY_COLORS[fam], width: 2.4,
           dash: FAMILY_DASH[fam] || "solid" }},
  marker: {{ size: 5 }},
  customdata: prof.step.map((_, i) => [prof.t_f[i], prof.p_psia[i],
                                       prof.dsi[fam][i]]),
  hovertemplate:
    fam + " SI %{{y:.2f}} (&#916;SI %{{customdata[2]:.2f}})" +
    "<br>%{{customdata[0]:.0f}} &#176;F, %{{customdata[1]:.0f}} psia" +
    "<extra></extra>",
}}));
Plotly.newPlot("profile-chart", profTraces, Object.assign({{}}, baseLayout, {{
  xaxis: {{
    title: "flow-path step (0 = bottomhole, " + (prof.step.length - 1) +
           " = wellhead)",
    tickmode: "array",
    tickvals: prof.step,
    ticktext: prof.step.map((s, i) =>
      i === 0 ? "BH" : (i === prof.step.length - 1 ? "WH" : String(s))),
    gridcolor: "#e3eaf4",
  }},
  yaxis: {{ title: "saturation index, SI", gridcolor: "#e3eaf4",
            zeroline: false }},
}}), {{ responsive: true, displaylogo: false }});

// Panel 2: mixing sweep
const mix = DATA.mixing;
const mixTraces = DATA.families.map((fam) => ({{
  x: mix.fraction_b,
  y: mix.si[fam],
  name: fam,
  mode: "lines",
  line: {{ color: FAMILY_COLORS[fam], width: 2.4,
           dash: FAMILY_DASH[fam] || "solid" }},
  hovertemplate: fam + " SI %{{y:.2f}} at %{{x:.0%}} seawater<extra></extra>",
}}));
Plotly.newPlot("mixing-chart", mixTraces, Object.assign({{}}, baseLayout, {{
  xaxis: {{
    title: "seawater volume fraction (0 = formation water, 1 = seawater), " +
           "at " + mix.t_f + " \\u00B0F / " + mix.p_psia + " psia",
    tickformat: ".0%", gridcolor: "#e3eaf4",
  }},
  yaxis: {{ title: "saturation index, SI", gridcolor: "#e3eaf4",
            zeroline: false }},
}}), {{ responsive: true, displaylogo: false }});
</script>
</body>
</html>
"""


def main() -> None:
    payload = build_payload()
    html = HTML_TEMPLATE.format(payload_json=json.dumps(payload))
    _OUT.parent.mkdir(parents=True, exist_ok=True)
    _OUT.write_text(html, encoding="utf-8")
    print(f"wrote {_OUT.relative_to(_REPO)} ({_OUT.stat().st_size:,} bytes)")


if __name__ == "__main__":
    main()
