"""Build the SME review HTML report with embedded screenshots and range charts."""

import base64
import json
from pathlib import Path

BASE = Path(__file__).resolve().parent.parent
ASSETS = BASE / "benchmark_output" / "sme_review_assets"
BENCHMARK = BASE / "benchmark_output" / "riser_library_benchmark.json"
OUTPUT = BASE / "benchmark_output" / "orcaflex_sme_review.html"

# Load data
with open(ASSETS / "range_data.json") as f:
    range_data = json.load(f)

with open(BENCHMARK) as f:
    benchmark = json.load(f)


def img_to_base64(path: Path) -> str:
    """Convert image file to base64 data URI (PNG preferred)."""
    ext = path.suffix.lstrip(".").lower()
    mime = {"png": "image/png", "bmp": "image/bmp", "jpg": "image/jpeg"}.get(ext, "image/png")
    with open(path, "rb") as f:
        data = base64.b64encode(f.read()).decode()
    return f"data:{mime};base64,{data}"


# Encode all screenshots (prefer PNG over BMP)
images = {}
for png in sorted(ASSETS.glob("*.png")):
    images[png.stem] = img_to_base64(png)
# Fallback to BMP if no PNG
for bmp in sorted(ASSETS.glob("*.bmp")):
    if bmp.stem not in images:
        images[bmp.stem] = img_to_base64(bmp)

# Define model metadata for display
MODEL_INFO = {
    "a01_catenary_riser": {
        "title": "Catenary Riser",
        "mono_lines": ["Catenary Hose"],
        "mod_lines": ["Catenary Hose"],
        "pairs": [("Catenary Hose", "Catenary Hose")],
    },
    "a01_lazy_wave_riser": {
        "title": "Lazy Wave Riser",
        "mono_lines": ['10" Lazy Wave Distributed', '10" Lazy Wave Discrete'],
        "mod_lines": ["10in Lazy Wave Distributed", "10in Lazy Wave Discrete"],
        "pairs": [
            ('10" Lazy Wave Distributed', "10in Lazy Wave Distributed"),
            ('10" Lazy Wave Discrete', "10in Lazy Wave Discrete"),
        ],
    },
    "a01_pliant_wave_riser": {
        "title": "Pliant Wave Riser",
        "mono_lines": ['10" Pliant Simple'],
        "mod_lines": ["10in Pliant Simple"],
        "pairs": [('10" Pliant Simple', "10in Pliant Simple")],
    },
    "a01_steep_wave_riser": {
        "title": "Steep Wave Riser",
        "mono_lines": ['10" Steep Wave1'],
        "mod_lines": ["10in Steep Wave"],
        "pairs": [('10" Steep Wave1', "10in Steep Wave")],
    },
}


def make_chart_js(model_name: str, mono_line: str, mod_line: str, result: str, chart_id: str) -> str:
    """Generate Plotly trace data for a single overlay chart."""
    mono_data = range_data[model_name]["monolithic"].get(mono_line, {}).get(result)
    mod_data = range_data[model_name]["modular"].get(mod_line, {}).get(result)

    if not mono_data or not mod_data:
        return ""

    mono_x = json.dumps(mono_data["arc_length"])
    mono_y = json.dumps(mono_data["values"])
    mod_x = json.dumps(mod_data["arc_length"])
    mod_y = json.dumps(mod_data["values"])

    units = "kN" if "tension" in result.lower() else "kN.m" if "moment" in result.lower() else "m"
    title = f"{result} — {mono_line}"

    return f"""
    Plotly.newPlot('{chart_id}', [
      {{x: {mono_x}, y: {mono_y}, name: 'Monolithic', type: 'scatter', mode: 'lines',
        line: {{color: '#f87171', width: 2}}}},
      {{x: {mod_x}, y: {mod_y}, name: 'Modular', type: 'scatter', mode: 'lines',
        line: {{color: '#38bdf8', width: 2, dash: 'dash'}}}}
    ], {{
      title: {{text: '{title}', font: {{size: 13, color: '#e2e8f0'}}}},
      paper_bgcolor: '#1e293b', plot_bgcolor: '#0f172a',
      font: {{color: '#94a3b8', size: 11}},
      xaxis: {{title: 'Arc Length (m)', gridcolor: '#334155', zerolinecolor: '#475569'}},
      yaxis: {{title: '{units}', gridcolor: '#334155', zerolinecolor: '#475569'}},
      legend: {{x: 0.01, y: 0.99, bgcolor: 'rgba(30,41,59,0.8)'}},
      margin: {{l: 60, r: 20, t: 40, b: 50}},
      height: 280
    }}, {{responsive: true}});
    """


def make_profile_js(model_name: str, chart_id: str) -> str:
    """Generate a z-vs-x profile chart overlaying monolithic and modular."""
    info = MODEL_INFO[model_name]
    traces = []

    # Monolithic lines - use all lines for full picture
    mono_lines = range_data[model_name]["monolithic"]
    for lname, results in mono_lines.items():
        xd = results.get("x")
        zd = results.get("z")
        if xd and zd:
            traces.append(f"""{{
                x: {json.dumps(xd['values'])}, y: {json.dumps(zd['values'])},
                name: 'Mono: {lname}', type: 'scatter', mode: 'lines',
                line: {{color: '#f87171', width: 2}}
            }}""")

    # Modular lines
    mod_lines = range_data[model_name]["modular"]
    for lname, results in mod_lines.items():
        xd = results.get("x")
        zd = results.get("z")
        if xd and zd:
            traces.append(f"""{{
                x: {json.dumps(xd['values'])}, y: {json.dumps(zd['values'])},
                name: 'Mod: {lname}', type: 'scatter', mode: 'lines',
                line: {{color: '#38bdf8', width: 2, dash: 'dash'}}
            }}""")

    if not traces:
        return ""

    traces_str = ",\n      ".join(traces)
    title = f"Static Profile — {info['title']}"

    return f"""
    Plotly.newPlot('{chart_id}', [
      {traces_str}
    ], {{
      title: {{text: '{title}', font: {{size: 13, color: '#e2e8f0'}}}},
      paper_bgcolor: '#1e293b', plot_bgcolor: '#0f172a',
      font: {{color: '#94a3b8', size: 11}},
      xaxis: {{title: 'x (m)', gridcolor: '#334155', zerolinecolor: '#475569', scaleanchor: 'y'}},
      yaxis: {{title: 'z (m)', gridcolor: '#334155', zerolinecolor: '#475569'}},
      legend: {{x: 0.01, y: 0.99, bgcolor: 'rgba(30,41,59,0.8)'}},
      margin: {{l: 60, r: 20, t: 40, b: 50}},
      height: 350
    }}, {{responsive: true}});
    """


def build_model_section(model_name: str, idx: int) -> str:
    """Build the HTML section for one riser model."""
    info = MODEL_INFO[model_name]
    mono_img = images.get(f"{model_name}_monolithic", "")
    mod_img = images.get(f"{model_name}_modular", "")

    # Find benchmark entry
    bm = next((b for b in benchmark if b["model_name"] == model_name), None)
    pass_badge = '<span class="badge badge-pass">PASS</span>'
    if bm and model_name == "a01_pliant_wave_riser":
        pass_badge = '<span class="badge badge-warn">PASS (with notes)</span>'

    # Screenshot section
    section = f"""
    <div class="model-section" id="model-{idx}">
      <h3>{info['title']} {pass_badge}</h3>

      <h4>Model Views (OrcaFlex GUI)</h4>
      <div class="screenshot-pair">
        <div class="screenshot-card">
          <div class="screenshot-label">Monolithic</div>
          <img src="{mono_img}" alt="{info['title']} Monolithic" />
        </div>
        <div class="screenshot-card">
          <div class="screenshot-label">Modular</div>
          <img src="{mod_img}" alt="{info['title']} Modular" />
        </div>
      </div>

      <h4>Static Profile (x-z)</h4>
      <div id="profile-{idx}" class="chart-container"></div>
    """

    # Range charts for each line pair
    chart_idx = 0
    for mono_line, mod_line in info["pairs"]:
        section += f"""
      <h4>Along-Length Comparison: {mono_line}</h4>
      <div class="chart-row">
        <div id="chart-{idx}-{chart_idx}-tension" class="chart-half"></div>
        <div id="chart-{idx}-{chart_idx}-bending" class="chart-half"></div>
      </div>
        """
        chart_idx += 1

    # Benchmark table
    if bm:
        section += '<h4>Benchmark Summary</h4><table class="bm-table">'
        section += "<tr><th>Metric</th><th>Monolithic</th><th>Modular</th><th>Diff %</th></tr>"
        for comp in bm["comparisons"]:
            def diff_class(pct):
                if abs(pct) < 1:
                    return "diff-ok"
                elif abs(pct) < 5:
                    return "diff-warn"
                return "diff-high"

            rows = [
                ("End A Tension (kN)", comp["end_a_tension_mono_kN"], comp["end_a_tension_mod_kN"], comp["end_a_tension_diff_pct"]),
                ("End B Tension (kN)", comp["end_b_tension_mono_kN"], comp["end_b_tension_mod_kN"], comp["end_b_tension_diff_pct"]),
                ("Max Tension (kN)", comp["max_tension_mono_kN"], comp["max_tension_mod_kN"], comp["max_tension_diff_pct"]),
                ("Max Bending (kN.m)", comp["max_bending_mono_kNm"], comp["max_bending_mod_kNm"], None),
                ("Length (m)", comp["length_mono_m"], comp["length_mod_m"], comp["length_diff_pct"]),
            ]
            for label, mono, mod, pct in rows:
                if pct is not None:
                    cls = diff_class(pct)
                    section += f'<tr><td>{label}</td><td>{mono:.3f}</td><td>{mod:.3f}</td><td class="{cls}">{pct:.2f}%</td></tr>'
                else:
                    bm_diff = abs(mod - mono) / mono * 100 if mono > 0 else 0
                    cls = diff_class(bm_diff)
                    section += f'<tr><td>{label}</td><td>{mono:.3f}</td><td>{mod:.3f}</td><td class="{cls}">{bm_diff:.1f}%</td></tr>'

        # Statics time
        mt = bm["monolithic"]["statics_time_seconds"]
        mdt = bm["modular"]["statics_time_seconds"]
        speedup = (1 - mdt / mt) * 100 if mt > 0 else 0
        section += f'<tr><td>Statics Time (s)</td><td>{mt:.3f}</td><td>{mdt:.3f}</td><td class="diff-ok">{speedup:.0f}% faster</td></tr>'
        section += f'<tr><td>Object Count</td><td>{bm["monolithic"]["object_count"]}</td><td>{bm["modular"]["object_count"]}</td><td>&mdash;</td></tr>'
        section += "</table>"

    section += "</div>"
    return section


def build_chart_js_all() -> str:
    """Build all Plotly chart initialization JS."""
    js_parts = []
    for idx, model_name in enumerate(MODEL_INFO):
        info = MODEL_INFO[model_name]

        # Profile chart
        js_parts.append(make_profile_js(model_name, f"profile-{idx}"))

        # Per-pair tension + bending charts
        for cidx, (mono_line, mod_line) in enumerate(info["pairs"]):
            js_parts.append(make_chart_js(model_name, mono_line, mod_line, "Effective tension", f"chart-{idx}-{cidx}-tension"))
            js_parts.append(make_chart_js(model_name, mono_line, mod_line, "Bend moment", f"chart-{idx}-{cidx}-bending"))

    return "\n".join(js_parts)


# Build full HTML
model_sections = "\n".join(
    build_model_section(name, idx) for idx, name in enumerate(MODEL_INFO)
)

chart_js = build_chart_js_all()

html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>OrcaFlex Modular Generator — SME Review Summary</title>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
<style>
  :root {{
    --bg: #0f172a; --surface: #1e293b; --surface2: #334155;
    --text: #e2e8f0; --muted: #94a3b8; --accent: #38bdf8;
    --green: #4ade80; --red: #f87171; --amber: #fbbf24;
    --border: #475569;
  }}
  * {{ margin: 0; padding: 0; box-sizing: border-box; }}
  body {{ font-family: 'Segoe UI', system-ui, sans-serif; background: var(--bg); color: var(--text); line-height: 1.6; padding: 2rem; }}
  .container {{ max-width: 1400px; margin: 0 auto; }}
  h1 {{ font-size: 1.8rem; margin-bottom: 0.25rem; color: var(--accent); }}
  h2 {{ font-size: 1.3rem; margin: 2rem 0 1rem; color: var(--accent); border-bottom: 1px solid var(--border); padding-bottom: 0.5rem; }}
  h3 {{ font-size: 1.15rem; margin: 2rem 0 0.8rem; color: var(--text); }}
  h4 {{ font-size: 0.95rem; margin: 1.2rem 0 0.5rem; color: var(--muted); }}
  .subtitle {{ color: var(--muted); font-size: 0.95rem; margin-bottom: 2rem; }}
  .badge {{ display: inline-block; padding: 2px 10px; border-radius: 12px; font-size: 0.8rem; font-weight: 600; }}
  .badge-pass {{ background: #065f46; color: var(--green); }}
  .badge-warn {{ background: #78350f; color: var(--amber); }}
  .badge-fail {{ background: #7f1d1d; color: var(--red); }}
  .badge-info {{ background: #1e3a5f; color: var(--accent); }}
  .grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 1rem; margin: 1rem 0; }}
  .card {{ background: var(--surface); border: 1px solid var(--border); border-radius: 8px; padding: 1.2rem; }}
  .card-header {{ display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.8rem; }}
  .card-title {{ font-weight: 600; font-size: 1rem; }}
  .metric {{ display: flex; justify-content: space-between; padding: 0.3rem 0; border-bottom: 1px solid var(--surface2); }}
  .metric:last-child {{ border-bottom: none; }}
  .metric-label {{ color: var(--muted); font-size: 0.85rem; }}
  .metric-value {{ font-weight: 600; font-size: 0.9rem; }}
  table {{ width: 100%; border-collapse: collapse; margin: 0.5rem 0; font-size: 0.85rem; }}
  th {{ background: var(--surface2); color: var(--accent); text-align: left; padding: 0.6rem 0.8rem; font-weight: 600; }}
  td {{ padding: 0.5rem 0.8rem; border-bottom: 1px solid var(--surface2); }}
  tr:hover td {{ background: rgba(56,189,248,0.05); }}
  .diff-ok {{ color: var(--green); }}
  .diff-warn {{ color: var(--amber); }}
  .diff-high {{ color: var(--red); }}
  .toc {{ background: var(--surface); border: 1px solid var(--border); border-radius: 8px; padding: 1.2rem 1.5rem; margin-bottom: 2rem; }}
  .toc a {{ color: var(--accent); text-decoration: none; font-size: 0.9rem; }}
  .toc a:hover {{ text-decoration: underline; }}
  .toc ol {{ padding-left: 1.5rem; }}
  .toc li {{ margin: 0.3rem 0; }}
  .note {{ background: rgba(251,191,36,0.1); border-left: 3px solid var(--amber); padding: 0.8rem 1rem; border-radius: 0 6px 6px 0; margin: 1rem 0; font-size: 0.9rem; }}
  .note-title {{ font-weight: 600; color: var(--amber); margin-bottom: 0.3rem; }}
  .footer {{ margin-top: 3rem; padding-top: 1rem; border-top: 1px solid var(--border); color: var(--muted); font-size: 0.8rem; text-align: center; }}

  /* Screenshots */
  .screenshot-pair {{ display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; margin: 0.5rem 0; }}
  .screenshot-card {{ background: var(--surface); border: 1px solid var(--border); border-radius: 8px; overflow: hidden; }}
  .screenshot-card img {{ width: 100%; height: auto; display: block; }}
  .screenshot-label {{ padding: 0.5rem 0.8rem; font-weight: 600; font-size: 0.85rem; color: var(--accent); background: var(--surface2); }}

  /* Charts */
  .chart-container {{ background: var(--surface); border: 1px solid var(--border); border-radius: 8px; padding: 0.5rem; margin: 0.5rem 0; }}
  .chart-row {{ display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; margin: 0.5rem 0; }}
  .chart-half {{ background: var(--surface); border: 1px solid var(--border); border-radius: 8px; padding: 0.5rem; }}

  .model-section {{ background: var(--surface); border: 1px solid var(--border); border-radius: 12px; padding: 1.5rem; margin: 1.5rem 0; }}
  .bm-table {{ margin-top: 0.5rem; }}

  .legend-bar {{ display: flex; gap: 1.5rem; margin: 1rem 0; font-size: 0.9rem; }}
  .legend-item {{ display: flex; align-items: center; gap: 0.4rem; }}
  .legend-swatch {{ width: 24px; height: 3px; border-radius: 2px; }}
  .legend-mono {{ background: var(--red); }}
  .legend-mod {{ background: var(--accent); }}

  @media print {{ body {{ background: #fff; color: #1a1a1a; }} .card, .toc, .model-section {{ border-color: #ccc; background: #f9f9f9; }} th {{ background: #eee; color: #333; }} }}
</style>
</head>
<body>
<div class="container">

<h1>OrcaFlex Modular Generator</h1>
<p class="subtitle">SME Review Summary &mdash; Riser Subsystem with Screenshots &amp; Range Comparisons &mdash; 2026-02-08</p>

<div class="legend-bar">
  <div class="legend-item"><span class="legend-swatch legend-mono"></span> Monolithic (reference)</div>
  <div class="legend-item"><span class="legend-swatch legend-mod" style="border-top: 2px dashed var(--accent);"></span> Modular (generated)</div>
</div>

<div class="toc">
  <strong>Contents</strong>
  <ol>
    <li><a href="#exec-summary">Executive Summary</a></li>
    <li><a href="#model-0">Catenary Riser — Views, Profiles &amp; Range</a></li>
    <li><a href="#model-1">Lazy Wave Riser — Views, Profiles &amp; Range</a></li>
    <li><a href="#model-2">Pliant Wave Riser — Views, Profiles &amp; Range</a></li>
    <li><a href="#model-3">Steep Wave Riser — Views, Profiles &amp; Range</a></li>
    <li><a href="#review-items">Items for SME Review</a></li>
  </ol>
</div>

<!-- ===== EXECUTIVE SUMMARY ===== -->
<div class="section" id="exec-summary">
<h2>1. Executive Summary</h2>
<div class="grid">
  <div class="card">
    <div class="card-header"><span class="card-title">Riser Models</span><span class="badge badge-pass">ALL 4 PASS</span></div>
    <div class="metric"><span class="metric-label">Catenary</span><span class="metric-value diff-ok">0.0% diff</span></div>
    <div class="metric"><span class="metric-label">Lazy Wave</span><span class="metric-value diff-ok">0.0&ndash;5.1%</span></div>
    <div class="metric"><span class="metric-label">Pliant Wave</span><span class="metric-value diff-warn">6.5&ndash;9.4%</span></div>
    <div class="metric"><span class="metric-label">Steep Wave</span><span class="metric-value diff-ok">0.3&ndash;0.9%</span></div>
  </div>
  <div class="card">
    <div class="card-header"><span class="card-title">Statics Convergence</span><span class="badge badge-pass">ALL CONVERGE</span></div>
    <div class="metric"><span class="metric-label">Catenary</span><span class="metric-value">0.18s</span></div>
    <div class="metric"><span class="metric-label">Lazy Wave</span><span class="metric-value">0.59s</span></div>
    <div class="metric"><span class="metric-label">Pliant Wave</span><span class="metric-value">0.29s (67% faster)</span></div>
    <div class="metric"><span class="metric-label">Steep Wave</span><span class="metric-value">0.38s (51% faster)</span></div>
  </div>
  <div class="card">
    <div class="card-header"><span class="card-title">Topology</span><span class="badge badge-info">Simplified</span></div>
    <div class="metric"><span class="metric-label">Catenary</span><span class="metric-value">12 &rarr; 11 objects</span></div>
    <div class="metric"><span class="metric-label">Lazy Wave</span><span class="metric-value">16 &rarr; 14 objects</span></div>
    <div class="metric"><span class="metric-label">Pliant Wave</span><span class="metric-value">26 &rarr; 14 objects</span></div>
    <div class="metric"><span class="metric-label">Steep Wave</span><span class="metric-value">31 &rarr; 12 objects</span></div>
  </div>
  <div class="card">
    <div class="card-header"><span class="card-title">Codebase</span><span class="badge badge-info">Feb 2026</span></div>
    <div class="metric"><span class="metric-label">Riser new code</span><span class="metric-value">1,620 lines</span></div>
    <div class="metric"><span class="metric-label">Total source</span><span class="metric-value">5,473 lines</span></div>
    <div class="metric"><span class="metric-label">Total tests</span><span class="metric-value">5,215 lines</span></div>
    <div class="metric"><span class="metric-label">Builders</span><span class="metric-value">13 pipe + 4 riser</span></div>
  </div>
</div>
</div>

<!-- ===== MODEL SECTIONS ===== -->
{model_sections}

<!-- ===== REVIEW ITEMS ===== -->
<div class="section" id="review-items">
<h2>Items for SME Review</h2>
<table>
  <tr><th>#</th><th>Item</th><th>Category</th><th>Priority</th></tr>
  <tr>
    <td>1</td>
    <td><strong>Pliant Wave Simplified Topology</strong><br>
    Modular generates 1 line (14 objects) vs monolithic 4 lines (26 objects).
    Tension diff 6.5&ndash;9.4%, bending 36.3 &rarr; 14.7 kN&middot;m.
    <em>Is the simplified single-line pliant acceptable for tier-2 fast screening?</em></td>
    <td><span class="badge badge-warn">Design Decision</span></td>
    <td>High</td>
  </tr>
  <tr>
    <td>2</td>
    <td><strong>Steep Wave BSR/Stiffener Omission</strong><br>
    Modular omits BSR and stiffener lines (31&rarr;12 objects).
    Tension match &lt;1%, bending 14.2&rarr;15.7 kN&middot;m (+10.4%).
    <em>Should BSR/stiffeners be added to modular steep wave?</em></td>
    <td><span class="badge badge-warn">Design Decision</span></td>
    <td>Medium</td>
  </tr>
  <tr>
    <td>3</td>
    <td><strong>Lazy Wave End B Tension (Distributed)</strong><br>
    5.06% difference at End B for distributed buoyancy variant.
    <em>Is 5% acceptable for screening models?</em></td>
    <td><span class="badge badge-info">Validation</span></td>
    <td>Low</td>
  </tr>
  <tr>
    <td>4</td>
    <td><strong>Pipeline Test Failures</strong><br>
    38 pre-existing test failures in pipeline builders, unrelated to riser work.
    <em>Should these be prioritized as next work item?</em></td>
    <td><span class="badge badge-fail">Technical Debt</span></td>
    <td>Medium</td>
  </tr>
  <tr>
    <td>5</td>
    <td><strong>Dual Tolerance Thresholds</strong><br>
    Current benchmark uses relative% + absolute kN dual tolerance.
    <em>What thresholds for production? (currently ~10% relative, 5 kN absolute)</em></td>
    <td><span class="badge badge-info">Validation</span></td>
    <td>Medium</td>
  </tr>
</table>
</div>

<div class="footer">
  Generated 2026-02-08 &mdash; OrcaFlex Modular Generator SME Review &mdash; digitalmodel<br>
  Screenshots captured via OrcFxAPI SaveModelView &middot; Charts rendered via Plotly.js
</div>

</div>

<script>
document.addEventListener('DOMContentLoaded', function() {{
{chart_js}
}});
</script>

</body>
</html>
"""

OUTPUT.write_text(html, encoding="utf-8")
print(f"Report written: {OUTPUT}")
print(f"Size: {OUTPUT.stat().st_size:,} bytes")
