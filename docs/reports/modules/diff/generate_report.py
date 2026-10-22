# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the self-contained diffraction module report from public source tests.

Run with NumPy and SciPy available:
    python docs/reports/modules/diff/generate_report.py --cache-dir <scratch>

The first computation also needs --measure. Subsequent builds reuse the recorded
timing; they never present a new timing as the original measurement. Numerical
files are loaded directly so the optional licensed adapters are never imported.
"""

from __future__ import annotations

import argparse
import ast
from datetime import datetime, timezone
import hashlib
import html
import importlib.util
import json
import math
import os
import re
import sys
import time
from pathlib import Path


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
MODULE = ROOT / "src/digitalmodel/hydrodynamics/diffraction"
TESTS = ROOT / "tests/hydrodynamics/diffraction"
REVIEWED_TESTS = (
    "test_analytical_cylinder.py", "test_long_wave_limits.py",
    "test_roll_damping.py", "test_equilibrium_checks.py",
    "test_input_schemas.py", "test_diffraction_units.py",
    "test_output_schemas.py", "test_cross_solver_acceptance.py",
    "test_canonical_example.py", "test_verdict_provenance.py",
    "test_unit_box_benchmark.py",
)


def load_numerical(name: str):
    """Load one self-contained numerical implementation without adapters."""
    alias = "diffraction_report_" + name
    if alias in sys.modules:
        return sys.modules[alias]
    spec = importlib.util.spec_from_file_location(alias, MODULE / (name + ".py"))
    module = importlib.util.module_from_spec(spec)
    sys.modules[alias] = module
    spec.loader.exec_module(module)
    return module


def function(tree, name):
    """Find an explicitly named source-test function."""
    return next(n for n in ast.walk(tree) if isinstance(n, ast.FunctionDef) and n.name == name)


def read_basis():
    """Extract inputs and targets from the documented public cylinder tests."""
    tree = ast.parse((TESTS / "test_analytical_cylinder.py").read_text(encoding="utf-8"))
    assignments = {
        n.targets[0].id: n.value for n in tree.body
        if isinstance(n, ast.Assign) and isinstance(n.targets[0], ast.Name)
    }
    geom = {kw.arg: ast.literal_eval(kw.value) for kw in assignments["GEOM"].keywords}
    regression = function(tree, "test_regression_values_for_the_reference_geometry")
    calls = [n.value for n in regression.body if isinstance(n, ast.Assign)]
    frequencies = [ast.literal_eval(n.args[0]) for n in calls]
    convergence = function(tree, "test_damping_converges_in_mode_count")
    modes = [
        ast.literal_eval(kw.value) for n in ast.walk(convergence)
        if isinstance(n, ast.Call) and isinstance(n.func, ast.Name)
        and n.func.id == "heave_added_mass_damping"
        for kw in n.keywords if kw.arg == "modes"
    ]
    def target(test):
        assertion = next(n for n in function(tree, test).body if isinstance(n, ast.Assert))
        return 100 * ast.literal_eval(assertion.test.comparators[0])
    return {
        "geometry": geom, "density": ast.literal_eval(assignments["RHO"]),
        "gravity": load_numerical("analytical_cylinder").STANDARD_GRAVITY,
        "frequencies": frequencies, "mode_counts": modes,
        "added_mass_target_percent": target("test_added_mass_converges_in_mode_count"),
        "damping_target_percent": target("test_damping_converges_in_mode_count"),
    }


def read_long_wave_basis():
    """Use the public small-body comparator test, separately from the cylinder."""
    tree = ast.parse((TESTS / "test_long_wave_limits.py").read_text(encoding="utf-8"))
    cls = next(n for n in tree.body if isinstance(n, ast.ClassDef) and n.name == "TestExpectedRao")
    values = ast.literal_eval(next(n for n in cls.body if isinstance(n, ast.Assign)).value)
    test = function(cls, "test_heave_is_unity_at_every_heading")
    headings = ast.literal_eval(next(n for n in test.body if isinstance(n, ast.For)).iter)
    return {"omega": values[0], "depth": values[1], "headings": list(headings)}


def compute_example(cache_dir: Path | None = None):
    """Call the module; reuse completed cases only when their digest matches."""
    basis = read_basis()
    cylinder = load_numerical("analytical_cylinder")
    geom = cylinder.CylinderGeometry(**basis["geometry"])
    implementation = hashlib.sha256((MODULE / "analytical_cylinder.py").read_bytes()).hexdigest()
    coefficients = []
    for omega in basis["frequencies"]:
        for modes in basis["mode_counts"]:
            key = hashlib.sha256(json.dumps([implementation, basis, omega, modes], sort_keys=True).encode()).hexdigest()
            cached = cache_dir / (key + ".json") if cache_dir else None
            if cached and cached.exists():
                envelope = json.loads(cached.read_text(encoding="utf-8"))
                if envelope["input_digest"] != key or envelope["result_digest"] != example_digest(envelope["result"]):
                    raise ValueError("Cached numerical evidence digest mismatch")
                row = envelope["result"]
            else:
                r = cylinder.heave_added_mass_damping(
                    omega, geom, basis["density"], gravity=basis["gravity"], modes=modes,
                )
                row = {key: getattr(r, key) for key in ("omega", "modes", "added_mass", "damping", "residual", "period")}
                row["added_mass_t"] = r.added_mass / 1000
                if cached:
                    cached.parent.mkdir(parents=True, exist_ok=True)
                    cached.write_text(json.dumps({"input_digest": key, "result_digest": example_digest(row), "result": row}, indent=2) + "\n", encoding="utf-8", newline="\n")
            coefficients.append(row)
    refinement = []
    for omega in basis["frequencies"]:
        cases = [r for r in coefficients if r["omega"] == omega]
        coarse = min(cases, key=lambda result: result["modes"])
        fine = max(cases, key=lambda result: result["modes"])
        row = {"omega": omega}
        for key in ("added_mass", "damping"):
            row[key + "_difference_percent"] = 100 * abs(coarse[key] - fine[key]) / abs(fine[key])
        refinement.append(row)
    lw = read_long_wave_basis()
    limits = load_numerical("long_wave_limits")
    curves = {}
    for mode in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
        rows = []
        for heading in lw["headings"]:
            value = limits.expected_rao(mode, lw["omega"], heading, lw["depth"])
            rows.append({"heading": heading, "value": value,
                         "display_value": math.degrees(value) if mode in {"roll", "pitch", "yaw"} else value})
        curves[mode] = rows
    passed = sum(r[key + "_difference_percent"] < basis[key + "_target_percent"]
                 for r in refinement for key in ("added_mass", "damping"))
    return {"basis": basis, "coefficients": coefficients, "refinement": refinement,
            "long_wave_basis": lw, "long_wave": curves,
            "displaced_volume": geom.displaced_volume,
            "displaced_mass_t": basis["density"] * geom.displaced_volume / 1000,
            "gap": geom.gap,
            "pass_percent": 100 * passed / (len(refinement) * 2),
            "max_added_mass_difference": max(r["added_mass_difference_percent"] for r in refinement),
            "max_damping_difference": max(r["damping_difference_percent"] for r in refinement)}


def example_digest(example):
    """Hash numerical evidence using a stable JSON representation."""
    return hashlib.sha256(json.dumps(example, sort_keys=True).encode()).hexdigest()


def numerical_source_digest():
    """Identify the numerical source files used by the recorded computation."""
    return hashlib.sha256(b"".join(
        (MODULE / name).read_bytes() for name in ("analytical_cylinder.py", "long_wave_limits.py")
    )).hexdigest()


def lookup(data, path):
    """Resolve a presentation field into the numerical evidence."""
    for part in path.split("."):
        data = data[int(part)] if isinstance(data, list) else data[part]
    return data


def format_number(value, fmt=".1f"):
    """Use engineering precision without rendering a nonzero value as zero."""
    text = format(value, fmt)
    if value != 0 and float(text) == 0:
        return "<" + format(10 ** (-int(fmt[1:-1])), fmt) if value > 0 else "−<" + format(10 ** (-int(fmt[1:-1])), fmt)
    return text.replace("-", "−")


def number(data, path, fmt=".1f"):
    """Bind a visible numeric span to its raw result and display precision."""
    return f'<span data-value-key="{path}" data-format="{fmt}">{format_number(lookup(data, path), fmt)}</span>'


def cite(ref):
    """Link a concise source title to the reference register."""
    return f'<a href="#{ref}">[{ref}]</a>'


def table(headers, rows):
    """Render real table columns for numerical evidence and its sources."""
    return "<table><thead><tr>" + "".join(f"<th>{h}</th>" for h in headers) + "</tr></thead><tbody>" + "".join(
        "<tr>" + "".join(f"<td>{v}</td>" for v in row) + "</tr>" for row in rows
    ) + "</tbody></table>"


def chart_data(data):
    """Define one quantity against one variable, with explicit limit meaning."""
    charts = []
    for key, label, units in (("added_mass_t", "Heave added mass", "t"), ("damping", "Heave radiation damping", "N s/m")):
        series = []
        for modes in data["basis"]["mode_counts"]:
            rows = [r for r in data["coefficients"] if r["modes"] == modes]
            series.append({"name": f"Cylinder, {modes} modes", "points": [[r["omega"], r[key]] for r in rows]})
        charts.append({"id": key, "component": label, "x_label": "Angular frequency (rad/s)", "units": units,
                       "series": series, "limit": 0.0 if key == "damping" else None,
                       "limit_label": "Nonnegative radiation damping" if key == "damping" else "No general added-mass acceptance limit is assigned"})
    for key, label in (("added_mass", "Added-mass refinement difference"), ("damping", "Damping refinement difference")):
        charts.append({"id": key + "-refinement", "component": label, "x_label": "Angular frequency (rad/s)", "units": "%",
                       "series": [{"name": "Cylinder, coarse against fine", "points": [[r["omega"], r[key + "_difference_percent"]] for r in data["refinement"]]}],
                       "limit": data["basis"][key + "_target_percent"], "limit_label": "Repository target benchmark; strictly below the line"})
    for mode, rows in data["long_wave"].items():
        charts.append({"id": "long-wave-" + mode, "component": mode, "x_label": "Wave travel heading (degrees)",
                       "units": "degrees/m" if mode in {"roll", "pitch", "yaw"} else "m/m",
                       "series": [{"name": "Small-body comparator", "points": [[r["heading"], r["display_value"]] for r in rows]}],
                       "limit": 1.0 if mode == "heave" else None,
                       "limit_label": "Heave surface-following asymptote; not an acceptance band" if mode == "heave" else "Comparator only; no observed motion or acceptance band"})
    return charts


def chart_svg(chart):
    """Render accessible native interactive SVG, with no external library."""
    points = [p for series in chart["series"] for p in series["points"]]
    xmin, xmax = min(p[0] for p in points), max(p[0] for p in points)
    ys = [p[1] for p in points] + ([chart["limit"]] if chart["limit"] is not None else [])
    ymin, ymax = min(0, min(ys)), max(ys)
    ymax = ymax * 1.15 if ymax else 1.0
    def x(value):
        return 75 + 620 * (value - xmin) / (xmax - xmin or 1)
    def y(value):
        return 250 - 210 * (value - ymin) / (ymax - ymin)
    def label(value):
        return format_number(value, ".1e" if 0 < abs(value) < 0.1 else ".1f")
    out = [f'<figure id="{chart["id"]}"><figcaption>{html.escape(chart["component"])} ({chart["units"]}). {html.escape(chart["limit_label"])}.</figcaption>',
           '<svg viewBox="0 0 760 310" role="img" aria-label="' + html.escape(chart["component"]) + '">',
           '<path d="M75 35V250H700" fill="none" stroke="currentColor"/>',
           f'<text x="385" y="300" text-anchor="middle">{chart["x_label"]}</text>',
           f'<text x="18" y="150" transform="rotate(-90 18 150)" text-anchor="middle">{chart["units"]}</text>']
    for tick in sorted(set(p[0] for p in points)):
        out.append(f'<text x="{x(tick):.1f}" y="275" text-anchor="middle">{label(tick)}</text>')
    for fraction in (0, 0.5, 1):
        value = ymin + fraction * (ymax - ymin)
        out.append(f'<text x="68" y="{y(value):.1f}" text-anchor="end">{label(value)}</text>')
    if chart["limit"] is not None:
        yy = y(chart["limit"])
        out.append(f'<path d="M75 {yy:.1f}H700" stroke="var(--caution)" stroke-dasharray="6 4"/><text x="700" y="{yy - 6:.1f}" text-anchor="end">Limit {label(chart["limit"])} {chart["units"]}</text>')
    for index, series in enumerate(chart["series"]):
        color = "var(--brand)" if index == 0 else "var(--accent)"
        coords = " ".join(f"{x(a):.1f},{y(b):.1f}" for a, b in series["points"])
        out.append(f'<g data-series="{chart["id"]}-{index}"><polyline points="{coords}" fill="none" stroke="{color}" stroke-width="2"/>')
        for a, b in series["points"]:
            limit = label(chart["limit"]) if chart["limit"] is not None else "not assigned"
            tip = f'Case: {series["name"]}; component: {chart["component"]}; {chart["x_label"]}: {label(a)}; demand: {label(b)} {chart["units"]}; limit: {limit} {chart["units"]}'
            out.append(f'<circle cx="{x(a):.1f}" cy="{y(b):.1f}" r="5" fill="{color}" tabindex="0" data-tip="{html.escape(tip)}"><title>{html.escape(tip)}</title></circle>')
        out.append('</g>')
    out.append('</svg><div>')
    for index, series in enumerate(chart["series"]):
        out.append(f'<label><input type="checkbox" checked data-toggle="{chart["id"]}-{index}"> {html.escape(series["name"])}</label>')
    out.append('</div><output aria-live="polite">Focus or hover over a point for its case, component, demand, limit and units.</output></figure>')
    return "\n".join(out)


INTERACTIONS = r"""
const changes = JSON.parse(document.getElementById('changes-data').textContent);
const sectionSelect = document.getElementById('comment-section');
document.querySelectorAll('h2[id],h3[id]').forEach(h => {
  const option = document.createElement('option');
  option.value = h.id; option.textContent = h.textContent;
  sectionSelect.appendChild(option);
});
document.querySelectorAll('[data-tip]').forEach(point => {
  const show = () => { point.closest('figure').querySelector('output').textContent = point.dataset.tip; };
  point.addEventListener('focus', show); point.addEventListener('pointerenter', show);
});
document.querySelectorAll('[data-toggle]').forEach(input => {
  input.addEventListener('change', () => {
    document.querySelector('[data-series="' + input.dataset.toggle + '"]').style.display = input.checked ? '' : 'none';
  });
});
const changeList = document.getElementById('change-list');
document.getElementById('change-since').addEventListener('change', event => {
  changeList.replaceChildren();
  changes.revisions.filter(r => r.revision > Number(event.target.value)).forEach(r => {
    r.changes.forEach(change => {
      const li = document.createElement('li'); li.textContent = change; changeList.appendChild(li);
    });
  });
});
let directory = null;
let comments = [];
const status = document.getElementById('comment-status');
const digest = document.documentElement.dataset.evidence;
function renderComments() {
  const list = document.getElementById('comment-list'); list.replaceChildren();
  comments.forEach(comment => {
    const li = document.createElement('li');
    li.textContent = comment.section + ': ' + comment.text;
    list.appendChild(li);
  });
}
function validComments(value) {
  return value && value.evidence === digest && Array.isArray(value.comments) &&
    value.comments.every(c => c && typeof c.text === 'string' && typeof c.section === 'string');
}
document.getElementById('choose-folder').addEventListener('click', async () => {
  try {
    if (!window.showDirectoryPicker) {
      status.textContent = 'Folder saving is unavailable. Export comments and place the downloaded file beside this report.';
      return;
    }
    const selected = await window.showDirectoryPicker({mode: 'readwrite'});
    let loaded = null;
    try {
      const file = await selected.getFileHandle('comments.json');
      loaded = JSON.parse(await (await file.getFile()).text());
      if (!validComments(loaded)) throw new Error('Comments belong to different numerical evidence or have an invalid format.');
    } catch (error) {
      if (error.name !== 'NotFoundError') throw error;
    }
    directory = selected;
    if (loaded) comments = loaded.comments;
    renderComments();
    status.textContent = 'The selected report folder is available. Save comments writes comments.json beside the report.';
  } catch (error) { status.textContent = 'Folder access was not completed. ' + error.message; }
});
document.getElementById('add-comment').addEventListener('click', () => {
  const field = document.getElementById('comment-text');
  if (!field.value.trim()) { status.textContent = 'Enter a comment before adding it.'; return; }
  comments.push({section: sectionSelect.value, text: field.value.trim()});
  field.value = ''; renderComments();
  status.textContent = 'The comment is held in this open page. Save or export comments before closing it.';
});
document.getElementById('save-comments').addEventListener('click', async () => {
  try {
    if (!directory) { status.textContent = 'Choose the folder containing the report before saving comments.'; return; }
    const file = await directory.getFileHandle('comments.json', {create: true});
    const writable = await file.createWritable();
    await writable.write(JSON.stringify({evidence: digest, comments}, null, 2) + '\n');
    await writable.close(); status.textContent = 'Comments are saved beside the report.';
  } catch (error) { status.textContent = 'Comments were not saved. ' + error.message; }
});
document.getElementById('export-comments').addEventListener('click', () => {
  const url = URL.createObjectURL(new Blob([JSON.stringify({evidence: digest, comments}, null, 2) + '\n'], {type:'application/json'}));
  const link = document.createElement('a'); link.href = url; link.download = 'comments.json'; link.click();
  URL.revokeObjectURL(url); status.textContent = 'Place the exported comments.json beside the report.';
});
document.getElementById('import-comments').addEventListener('change', async event => {
  try {
    const value = JSON.parse(await event.target.files[0].text());
    if (!validComments(value)) throw new Error('The comment evidence or format does not match this report.');
    comments = value.comments; renderComments(); status.textContent = 'Comments are loaded.';
  } catch (error) { status.textContent = 'Comments were not loaded. ' + error.message; }
});
"""


def build_report(data, output):
    """Write the complete engineering narrative and computed evidence."""
    n = lambda path, fmt=".1f": number(data, path, fmt)
    s = cite
    record = json.loads((HERE / "execution.json").read_text(encoding="utf-8"))
    if record["example_digest"] != example_digest(record["example"]):
        raise ValueError("Recorded numerical evidence digest mismatch")
    if record["example"]["basis"] != data["basis"] or record["numerical_source_digest"] != numerical_source_digest():
        raise ValueError("Recorded execution inputs or numerical source do not match")
    changes = json.loads((HERE / "changes.json").read_text(encoding="utf-8"))
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    style = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    charts = chart_data(data)
    plots = {c["id"]: chart_svg(c) for c in charts}
    input_rows = [
        ["Radius, a", n("basis.geometry.radius") + " m", "Public source-test geometry", s("C")],
        ["Draft, d", n("basis.geometry.draft") + " m", "Public source-test geometry", s("C")],
        ["Water depth, h", n("basis.geometry.water_depth") + " m", "Flat bed, quiescent water", s("C")],
        ["Water density, ρ", n("basis.density") + " kg/m³", "Source-test fluid input; not site metocean", s("C")],
        ["Gravity, g", n("basis.gravity", ".5f") + " m/s²", "Implementation default, used explicitly", s("M")],
        ["Angular frequencies, ω", ", ".join(n(f"basis.frequencies.{i}") for i in range(len(data["basis"]["frequencies"]))) + " rad/s", "Source-test regression frequencies", s("C")],
        ["Retained modes, N", ", ".join(n(f"basis.mode_counts.{i}", ".0f") for i in range(len(data["basis"]["mode_counts"]))) , "Source-test refinement pair", s("C")],
    ]
    coeff_rows = [[n(f"coefficients.{i}.omega"), n(f"coefficients.{i}.period"), n(f"coefficients.{i}.modes", ".0f"),
                   n(f"coefficients.{i}.added_mass_t", ".3f"), n(f"coefficients.{i}.damping"), s("M")]
                  for i in range(len(data["coefficients"]))]
    refine_rows = [[n(f"refinement.{i}.omega"), n(f"refinement.{i}.added_mass_difference_percent"), n(f"refinement.{i}.damping_difference_percent"), s("C")]
                   for i in range(len(data["refinement"]))]
    for row, result in zip(refine_rows, data["refinement"], strict=True):
        row.insert(-1, "; ".join(
            key.replace("_", " ").capitalize() + (": target is met" if result[key + "_difference_percent"] < data["basis"][key + "_target_percent"] else ": target is not met")
            for key in ("added_mass", "damping")
        ))
    failed_screens = [
        (i, key) for i, row in enumerate(data["refinement"]) for key in ("added_mass", "damping")
        if row[key + "_difference_percent"] >= data["basis"][key + "_target_percent"]
    ]
    if failed_screens:
        screen_conclusion = " ".join(
            f'The {key.replace("_", " ")} refinement target is not met at {n(f"refinement.{i}.omega")} rad/s; further mode refinement is required before that coefficient is adopted as a converged reference.'
            for i, key in failed_screens
        )
    else:
        screen_conclusion = "The refinement targets are met at all sampled frequencies; the result remains restricted to this geometry and expansion pair."
    inventory = []
    evidence_topics = (
        "Cylinder physics and regression", "Long-wave comparators", "Roll damping arithmetic",
        "Equilibrium arithmetic", "Input contracts", "Unit conversions", "Output contracts",
        "Program-comparison policy", "Canonical preparation example", "Comparison provenance",
        "Synthetic unit-box reporting example",
    )
    for filename, topic in zip(REVIEWED_TESTS, evidence_topics, strict=True):
        tree = ast.parse((TESTS / filename).read_text(encoding="utf-8"))
        count = sum(isinstance(node, ast.FunctionDef) and node.name.startswith("test_") for node in ast.walk(tree))
        inventory.append([topic, str(count), "Source inspected; not an execution count", s("E")])
    criteria_module = load_numerical("cross_solver_acceptance")
    criteria_rows = [[category.value.replace("_", " ").capitalize(), format_number(100 * criterion.threshold) + " %",
                      "Reference term" if category.value == "hydrostatics" else "Reference mode peak",
                      {"hydrostatics": "Detect differing geometry or conventions", "excitation": "Compare diffraction forcing after convergence",
                       "added_mass": "Compare radiation inertia after convergence", "radiation_damping": "Allow the implemented radiation sensitivity band",
                       "motion_rao": "Compare motion after matching excitation and restoring",
                       "resonant_roll_rao": "Screen damping-sensitive motion; not a design limit"}[category.value], s("A")]
                     for category, criterion in criteria_module.DEFAULT_CRITERIA.items()]
    lw_rows = []
    for mode, rows in data["long_wave"].items():
        for i, row in enumerate(rows):
            lw_rows.append([mode.capitalize(), n(f"long_wave.{mode}.{i}.heading"), n(f"long_wave.{mode}.{i}.display_value", ".3f"),
                            "degrees/m" if mode in {"roll", "pitch", "yaw"} else "m/m", s("L")])
    revision_rows = [
        [str(revision["revision"]), revision["date"],
         "Initial module analysis; calculated cylinder evidence is distinguished from unassessed design behaviour."
         if revision["revision"] == 1 else " ".join(revision["changes"])]
        for revision in changes["revisions"]
    ]
    revision_options = "".join(
        f'<option value="{revision["revision"]}">Revision {revision["revision"]}</option>'
        for revision in changes["revisions"]
    )
    body = f"""
<header><div class="kicker">Hydrodynamics</div><h1>Diffraction module analysis</h1>
<p class="sub">Draft for maintainer review</p></header>
<nav aria-label="Report contents"><p>Contents: <a href="#summary">Summary</a> · <a href="#introduction">Introduction</a> · <a href="#results">Results</a> · <a href="#basis">Design data</a> · <a href="#method">Method</a> · <a href="#detail">Detailed results</a> · <a href="#findings">Findings</a> · <a href="#limits">Limits</a> · <a href="#recommendations">Recommendations</a> · <a href="#references">References and appendices</a></p></nav>

<h2 id="summary">1. Executive Summary</h2>
<p>The diffraction module is a preparation, conversion and assessment toolkit for frequency-domain hydrodynamics. Analysis is performed to determine the reproducibility of its public cylinder radiation example and the evidence available for wider use; key results are summarized below. {s("M")} {s("W")}</p>
<ul><li>The cylinder example is available as a reproducible radiation reference.
<ul><li>{n("pass_percent")} % of the sampled non-zero refinement comparisons meet the repository target benchmarks: added-mass change below {n("basis.added_mass_target_percent")} % and damping change below {n("basis.damping_target_percent")} %, against the finer expansion. {s("C")}</li>
<li>{screen_conclusion} {s("C")}</li>
<li>The computed mass and damping coefficients are tabulated in <a href="#detail">Section 6</a>; these describe forced heave radiation, not a freely responding body. {s("M")}</li></ul></li>
<li>A wider diffraction design is assessed only after its model basis is supplied.
<ul><li>Independent program agreement, preferred program, panel size relative to body length and comparative runtime are NOT_EVALUATED. No paired independent program result is computed here. {s("E")}</li>
<li>Surge, sway, heave, roll, pitch and yaw long-wave comparators are shown, with no observed motion arrays. Long-wave agreement is NOT_EVALUATED for every mode. {s("L")}</li></ul></li></ul>

<h3 id="revision">Revision history</h3>
{table(["Revision", "Date", "What changed and why"], revision_rows)}

<h2 id="introduction">2. Introduction</h2>
<p>A common model description is converted into hydrodynamic inputs, and completed results are organized for engineering assessment and motion-data transfer. The main workflow is explained from preparation to interpretation; the executable example is restricted to the independent numerical cylinder reference. {s("W")} {s("M")}</p>
<p>Geometry and mass properties are first described in a canonical specification. Frequency and heading axes, environment, solver options and output selections are supplied explicitly. Preparation is followed by an external diffraction solution, result extraction, unit and convention checks, physical screening and export. Native solver execution remains outside the evidence demonstrated by this report. {s("W")} {s("I")} {s("O")}</p>
<p>A motion response amplitude operator, abbreviated RAO, relates complex displacement to incident wave amplitude. Radiation added mass and damping instead describe forces generated by prescribed body motion. Radiation coefficients alone do not provide excitation forces or a motion RAO. {s("T")} {s("M")}</p>

<h2 id="results">3. Summary of Results</h2>
<p>Computed radiation coefficients are presented against angular frequency. The plotted points are the documented frequencies; joining lines are visual guides and carry no interpolation accuracy claim. {s("C")}</p>
{plots["added_mass_t"]}{plots["damping"]}
<p>Positive heave damping is returned at each sampled frequency. A nonnegative damping baseline is drawn because radiation removes energy from this prescribed heave motion; it is not a numerical accuracy tolerance. The meaning and applicable geometry are bounded by the cylinder physics tests. {s("C")} {s("T")}</p>
<h3 id="long-wave">3.1 Long-wave comparators for all motion modes</h3>
<p>The small-body comparator example uses ω = {n("long_wave_basis.omega", ".2f")} rad/s and depth {n("long_wave_basis.depth")} m from its separate public test case. The body follows the surface excursion or slope under the stated idealisation; these curves are expected magnitudes, not computed diffraction motion results. {s("L")}</p>
{"".join(plots["long-wave-" + mode] for mode in data["long_wave"])}
<p>Surge and sway use finite-depth surface particle excursions, heave uses the surface elevation, and roll and pitch use the wave slope without dynamic amplification. Yaw uses the module’s slender-body approximation; an irrotational excursion field gives a different result for a body symmetric in plan. No yaw acceptance claim is made. {s("L")}</p>

<h2 id="basis">4. Design Data</h2>
<p>A surface-piercing vertical circular cylinder is prescribed to heave in finite-depth water. The dimensions and fluid inputs are taken from the public repository example; they are not attributed to a physical installation or a published experimental cylinder. {s("C")}</p>
{table(["Input", "Value", "Basis", "Source"], input_rows)}
<figure><svg viewBox="0 0 760 330" role="img" aria-label="Cylinder section, matched interface and boundary conditions">
<path d="M45 65H715M45 285H715" stroke="var(--brand)" fill="none"/>
<path d="M280 65V190H480V65" stroke="currentColor" stroke-width="3" fill="var(--soft)"/>
<path d="M280 190V285M480 190V285" stroke="var(--accent)" stroke-dasharray="5 4"/>
<path d="M380 25V240M373 36L380 25L387 36M373 230L380 240L387 230" stroke="currentColor" fill="none"/>
<text x="395" y="32">Prescribed heave velocity</text><text x="50" y="52">Free surface z = 0; g ∂φ/∂z = ω²φ</text>
<text x="290" y="110">Cylinder</text><text x="290" y="140">Radius a = {format_number(data["basis"]["geometry"]["radius"])} m</text>
<text x="495" y="150">Draft d = {format_number(data["basis"]["geometry"]["draft"])} m</text><text x="495" y="177">Side: ∂φ/∂r = 0</text>
<text x="295" y="213">Base z = −d</text><text x="295" y="235">Unit heave velocity</text>
<text x="50" y="245">Exterior expansion</text><text x="50" y="265">Outgoing radiation</text>
<text x="490" y="220">Dashed: matching interfaces</text><text x="490" y="245">Gap h − d = {format_number(data["gap"])} m</text>
<text x="50" y="310">Flat bed z = −h; h = {format_number(data["basis"]["geometry"]["water_depth"])} m; ∂φ/∂z = 0</text>
</svg><figcaption>Figure 4.1. Cylinder cross-section and boundary conditions; not to scale. The inner and outer eigenfunction expansions are matched below the cylinder, without a panel mesh. {s("M")}</figcaption></figure>
<figure><svg viewBox="0 0 760 210" role="img" aria-label="Plan view and wave travel heading convention">
<ellipse cx="350" cy="105" rx="75" ry="75" fill="var(--soft)" stroke="currentColor"/>
<path d="M350 105H595L585 100M595 105L585 110M350 105V15L345 25M350 15L355 25M180 180L290 70L278 75M290 70L285 83" fill="none" stroke="var(--brand)"/>
<path d="M470 105A120 120 0 0 0 435 20" fill="none" stroke="var(--accent)"/>
<text x="605" y="110">+x</text><text x="365" y="25">+y</text><text x="470" y="55">β, wave travel heading</text><text x="40" y="195">Incident wave travels toward the body</text><text x="340" y="110">a</text>
</svg><figcaption>Figure 4.2. Plan of the circular waterplane and the positive heading measured from +x toward +y. Heading does not affect the axisymmetric heave radiation case. {s("L")} {s("M")}</figcaption></figure>
<h3 id="criteria">4.1 Acceptance basis and rationale</h3>
<p>Target benchmarks are engineering screens defined by the repository, not limits from a classification standard. For this example the expansion-refinement targets are applied only to the sampled heave coefficients; the broader program-comparison targets below remain NOT_EVALUATED. {s("A")} {s("C")}</p>
{table(["Quantity", "Target benchmark", "Denominator", "Rationale", "Source"], criteria_rows)}
<p>Inside-band program differences require comparable self-convergence evidence before an unconditional acceptance is returned by the tiered evaluator. The benchmark runner has a separate comparison route, so a report badge alone does not establish that this criterion was applied. {s("A")} {s("B")}</p>
<p>For the public cylinder, the strict refinement targets are added mass below {n("basis.added_mass_target_percent")} % and damping below {n("basis.damping_target_percent")} %, relative to the fine expansion. The repository tests impose these targets at the lower regression frequency; the same targets are also applied here at the higher frequency as an additional screen. They are not transferable design tolerances. {s("C")}</p>

<h2 id="method">5. Analysis Methodology</h2>
<p>Linear inviscid, incompressible and irrotational flow is assumed in the frequency domain. A flat impermeable bed, a linear free surface and small body motion are imposed. {s("T")}</p>
<div class="eq">∇²φ = 0; Φ(r,z,t) = Re[φ(r,z) exp(−iωt)]<br>g ∂φ/∂z = ω²φ at z = 0; ∂φ/∂z = 0 at z = −h<br>ω² = g k tanh(kh); T = 2π/ω</div>
<p>The complex potential is φ, the physical potential is Φ, time is t, angular frequency is ω, wavenumber is k, water depth is h and period is T. The imaginary unit satisfies i² = −1 and defines the stated time convention. The cylindrical coordinates are radius r and vertical elevation z, positive upward. Gravity is g. The equations follow the open theory manual’s linear boundary-value problem and the module’s dispersion implementation. {s("T")} {s("M")}</p>
<p>For the cylinder, the water below the base and the water outside the radius are expanded separately and matched in potential and radial velocity. The body-base condition is unit heave velocity, the cylindrical wall is impermeable and the exterior wave is outgoing. The expansion is truncated at N modes, with the same count used in both regions. A sharp lower edge produces slow local velocity convergence; an algebraic residual alone is not a force-error bound. {s("M")} {s("C")}</p>
<div class="eq">A₃₃ = ρ Re[∫<sub>base</sub> φ dS]; B₃₃ = ρω Im[∫<sub>base</sub> φ dS]<br>ΔQ = 100 |Q<sub>coarse</sub> − Q<sub>fine</sub>| / |Q<sub>fine</sub>|</div>
<p>The subscript 33 denotes heave–heave coupling. Added mass is A₃₃, radiation damping is B₃₃, water density is ρ and dS is the horizontal base area element. The force integral and signs are those of the implemented unit-velocity convention. The refinement quantity Q is either added mass or damping. The finer expansion supplies the reference denominator, which is nonzero in every reported comparison. {s("M")} {s("C")}</p>
<div class="eq">[−ω²(M + A) − iω(B + B<sub>ext</sub>) + C] ξ = F<sub>exc</sub>; RAO = ξ / a<sub>w</sub></div>
<p>For interpretation of a full motion analysis, M is structural mass and inertia, A is radiation added mass, B is radiation damping, B<sub>ext</sub> is externally supplied damping, C is restoring stiffness, ξ is complex motion, F<sub>exc</sub> is complex wave excitation and a<sub>w</sub> is incident wave amplitude. This is the stationary linear motion balance with the stated negative-exponential convention; no such motion system is solved in the cylinder example. {s("T")} {s("D")}</p>
<p>In the main workflow, a canonical specification is checked before mesh packaging and deck conversion. Frequency units, heading direction, reference point, density and rotational units are reconciled before completed results are compared or exported. Missing coefficient tables are distinguished from measured zeros because a spreadsheet bridge can supply placeholder matrices. {s("W")} {s("I")} {s("O")} {s("B")}</p>
<p>Preparation returns solver input files and associated meshes, not solved coefficients. Completed motion arrays are held by frequency and heading for each motion component; added-mass and damping matrices are held by frequency with per-block unit labels. Optional hydrostatics retain displacement, centres and stiffness. Export produces motion-data configuration, comma-separated coefficient tables, a workbook and a summary; each recipient convention is checked before those files are used. {s("W")} {s("O")}</p>

<h2 id="detail">6. Detailed Results</h2>
<p>The public cylinder example is calculated by calling the numerical module at each documented frequency and expansion count. All displayed coefficients, periods, differences and chart points are taken from those returned values; the generator contains no typed result pins. {s("M")} {s("C")}</p>
{table(["ω (rad/s)", "T (s)", "Modes", "A₃₃ (t)", "B₃₃ (N s/m)", "Source"], coeff_rows)}
<p>The algebraic residual is the returned relative norm of the solved matching system, retained in the numerical evidence. It describes the linear solve, while the refinement study addresses sensitivity to retained modes. Neither establishes an experimental validation error. {s("M")} {s("C")}</p>
{plots["added_mass-refinement"]}{plots["damping-refinement"]}
<p>{screen_conclusion} The verdict uses unrounded coefficient differences, including where a displayed difference rounds to the target value. Full-frequency convergence, other geometries, excitation and all motion amplitudes remain NOT_EVALUATED. {s("C")}</p>
<h3 id="evidence">6.1 Existing evidence and its meaning</h3>
<p>The analytical reference tests inspect dispersion roots, density scaling, damping behaviour, boundary conditions, matching continuity, truncation sensitivity and regression values. The continuity test deliberately zeroes solved coefficients to show that a trivial solution fails; free-surface and bed conditions alone are weaker evidence because the basis satisfies them by construction. {s("C")}</p>
<p>The long-wave tests exercise all motion comparators, including finite-depth dispersion and zero-reference handling. Roll-damping and equilibrium tests inspect their separate arithmetic contracts. Schema, unit and canonical-example tests support software behaviour, while synthetic comparison fixtures exercise decision logic without proving physical agreement between programs. {s("E")} {s("L")} {s("D")}</p>
<p>The published unit-box report is a synthetic report-pipeline example: its coefficient series are generated with seeded perturbations rather than native diffraction solutions. The canonical unit-box preparation example demonstrates specification parsing and portable mesh packaging in its source tests. Both are useful software evidence and neither establishes a motion-accuracy bound. {s("P")}</p>
<p>The stored example reports under the solver validation tree are not treated as independent reference-program outputs. The module source explicitly records this evidence limitation. Historical solver-proximity statements in its docstring are not recalculated or promoted to an error bound here. {s("M")}</p>
<h3 id="runtime">6.2 Runtime and computation</h3>
<p>The recorded first reference computation takes {format_number(record["elapsed_seconds"])} s for {record["case_count"]} coefficient cases and the comparator curves, with one numerical thread. The measured environment is {html.escape(record["architecture"])} with {record["logical_processors"]} logical processors and {format_number(record["memory_gib"])} GiB total memory. Imports and input extraction are included; rendering and test execution are excluded. This measurement is specific to these small reference systems. Native program runtime and comparative speed are NOT_EVALUATED. {s("R")}</p>

<h2 id="findings">7. Key Findings</h2>
<p>A reusable heave radiation reference is provided for the stated circular cylinder, with explicit expansion sensitivity. The evidence supports a reference-case check, while the complete diffraction workflow remains dependent on supplied geometry, inertia, conventions and independent result evidence. {s("M")} {s("W")}</p>
<p>Coefficient units are mixed by matrix block: translation, rotation and their coupling have distinct dimensions. Translational RAOs use m/m, while unified rotational RAOs use degrees/m. The long-wave functions return rotational slopes in rad/m, which are converted to degrees/m in this report. Native solver units and phase signs require explicit reconciliation; a common schema does not prove that reconciliation. {s("O")} {s("L")}</p>
<p>The calculated displaced-water mass is {n("displaced_mass_t", ".3f")} t, based on volume {n("displaced_volume", ".3f")} m³. This is a geometric reference only: structural mass, centre of gravity and stability are not supplied for this prescribed-motion case. Weight–buoyancy equilibrium and stability are therefore NOT_EVALUATED. {s("M")} {s("Q")}</p>

<h2 id="limits">8. Limitations</h2>
<p>Use is bounded by the linear potential-flow idealisation and by the evidence actually calculated. Viscous separation, breaking waves, large motion, impact and nonlinear roll response are absent from the cylinder reference. {s("T")} {s("D")}</p>
<p>No physical panel mesh is used by the matched-eigenfunction cylinder, so no mesh-size recommendation follows from its mode count. A panel-refinement sequence, wave coverage and independent reference evidence are required for a body-specific diffraction design. Regional sea-state periods and natural periods are NOT_EVALUATED because no metocean analysis or mass/restoring basis is supplied. No regional period values are assumed. {s("M")} {s("I")}</p>
<h3 id="not-covered">8.1 Not covered</h3>
<p>Native solver installation and execution, multibody interaction, control surfaces, quadratic transfer functions, mean drift, mooring coupling, pressure fields, detailed mesh generation and downstream dynamics are not evaluated. The module contains interfaces for several of these areas, but the unified output schema does not contain every native excitation or second-order result. {s("I")} {s("O")} {s("B")}</p>

<h2 id="recommendations">9. Recommendations and Way Forward</h2>
<p>The cylinder reference is retained as a small, reproducible radiation check. Wider application is advanced through a complete input basis and independently sourced comparisons with stated acceptance criteria. {s("C")} {s("A")}</p>
<ul><li>Use the cylinder refinement check when changes affect dispersion, eigenfunction matching or force integration. Recalculate the reported cases when those implementations change. {s("C")}</li>
<li>Increase the retained expansion count where a refinement target is not met, then repeat the coefficient comparison at the same frequency against a finer expansion. Accept that reference coefficient only after its stated target is met. {s("C")}</li>
<li>Complete the model basis before assessing body motion: geometry, mass, centre of gravity, inertia, environment, external damping and operating wave range. The missing-data register identifies the outstanding quantities. {s("I")} {s("Q")}</li>
<li>Define a body-specific panel-refinement sequence and target benchmarks before comparing programs. Preserve unit, origin, phase and heading evidence with the result arrays. No universal panel-length ratio is inferred from this reference. {s("A")} {s("O")}</li>
<li>Establish roll damping from an explicit frequency-dependent inertia and restoring basis. If resonance lies in the operating range, obtain damping evidence from decay or forced-motion tests before assigning a design roll limit. {s("D")} {s("T")}</li>
<li>Extend the reference evidence in sequence.<ul><li>Add a cited public excitation and motion benchmark with the original result arrays.</li><li>Compare converged panel results with the present cylinder radiation reference.</li><li>Complete all motion-mode long-wave checks, mesh quality, equilibrium and operating-range damping sensitivity before accepting a body-specific design.</li></ul> {s("A")} {s("C")} {s("L")} {s("Q")}</li></ul>

<h3 id="reproduction">9.1 Reproduction and interpretation</h3>
<p>The generator reads the public source-test inputs, calls the numerical files directly and builds this page. The accompanying test rebuilds the page and checks every bound numerical field and chart dataset against fresh module returns. Completed calculation cases are retained in an optional external scratch directory with input and result digests; changed inputs produce distinct records. The fixed execution record describes the measured first computation. {s("R")}</p>
<p>Chart series are selected with the checkboxes; point details are available by keyboard focus and hover. Comments are entered by section and saved beside the report through a selected folder. Where folder access is unavailable, the comment file is exported and placed beside the report, then imported when it is reopened. Changes are listed relative to the selected earlier revision.</p>
<h3 id="source-register">9.2 Source register and reproduction locators</h3>
<p>Repository locators and source editions are collected here. Source-test inputs are public repository examples, not values recovered from the cited historical paper. No numeric value from a licensed standard is used.</p>
<ul class="refs">
<li id="T">[T] Open potential-flow theory manual, stable web edition: <a href="https://capytaine.org/stable/theory_manual/theory.html#linear-boundary-value-problem">Linear boundary value problem</a>, <a href="https://capytaine.org/stable/theory_manual/theory.html#forces-on-body-surfaces">Forces on body surfaces</a> and <a href="https://capytaine.org/stable/theory_manual/theory.html#dynamic-coupling-and-impedance">Dynamic coupling and impedance</a>. The physical assumptions, negative-exponential convention, free-surface condition and motion balance are used.</li>
<li id="M">[M] Numerical implementation, repository edition read for this analysis: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/analytical_cylinder.py">analytical_cylinder.py</a>, geometry, wavenumbers, matching and heave force integration. Its historical citation is “Added mass and damping of a vertical cylinder in finite-depth waters”, Applied Ocean Research, volume 3, issue 3, 1981. The full historical text is not used as a numerical source.</li>
<li id="C">[C] Public cylinder example and target benchmarks: <a href="../../../../tests/hydrodynamics/diffraction/test_analytical_cylinder.py">test_analytical_cylinder.py</a>, geometry and density declarations, reference-geometry regression, mode-convergence and boundary-condition tests.</li>
<li id="L">[L] Long-wave comparator definition and public example: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/long_wave_limits.py">long_wave_limits.py</a>, expected_rao; <a href="../../../../tests/hydrodynamics/diffraction/test_long_wave_limits.py">test_long_wave_limits.py</a>, TestExpectedRao. Heading is wave travel direction from +x.</li>
<li id="W">[W] Main preparation and external-solution workflow: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/workflow.py">workflow.py</a> and <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/spec_converter.py">spec_converter.py</a>. Canonical preparation example: <a href="../../../../examples/workflows/aqwa-diffraction-deck-prep/input.yml">deck preparation input</a>.</li>
<li id="I">[I] Input contract: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/input_schemas.py">input_schemas.py</a>, geometry, inertia, environment, wave axes, solver and output fields.</li>
<li id="O">[O] Output and units: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/output_schemas.py">output_schemas.py</a>, result and matrix containers; <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/diffraction_units.py">diffraction_units.py</a>, unit conversion definitions, including SI prefixes; <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py">orcaflex_exporter.py</a>, tabular and motion-data exports.</li>
<li id="A">[A] Engineering comparison policy, repository edition: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/cross_solver_acceptance.py">cross_solver_acceptance.py</a>, DEFAULT_CRITERIA and convergence qualifier. Values are repository targets, not published standard limits.</li>
<li id="B">[B] Evidence cautions: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/docs/agent-pack.md">module evidence inventory</a>; <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py">spreadsheet bridge</a>, placeholder coefficient provenance; <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py">benchmark runner</a>, comparison route.</li>
<li id="D">[D] Uncoupled roll damping basis and frequency-dependent critical damping: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/roll_damping.py">roll_damping.py</a> and <a href="../../../../tests/hydrodynamics/diffraction/test_roll_damping.py">its source tests</a>.</li>
<li id="Q">[Q] Equilibrium arithmetic and required input tolerances: <a href="../../../../src/digitalmodel/hydrodynamics/diffraction/equilibrium_checks.py">equilibrium_checks.py</a>. No stability acceptance is inferred.</li>
<li id="E">[E] Existing source-test inventory: <a href="../../../../tests/hydrodynamics/diffraction/">diffraction tests</a>. The appendix counts test function definitions in the selected files, not collected or passed cases.</li>
<li id="P">[P] Published repository examples: <a href="../../../benchmarks/unit_box/benchmark_report.html">synthetic unit-box report</a> and <a href="../../../../tests/hydrodynamics/diffraction/test_unit_box_benchmark.py">its construction and provenance tests</a>; <a href="../../../../examples/hydrodynamics/diffraction/unit_box_rao/README.md">canonical unit-box preparation example</a> and <a href="../../../../tests/hydrodynamics/diffraction/test_canonical_example.py">its packaging source tests</a>. No native result is inferred from a preparation-only or synthetic example.</li>
<li id="R">[R] Reproduction: <a href="generate_report.py">generator</a>, <a href="execution.json">execution record</a>, <a href="../../../../tests/docs/test_diffraction_module_report.py">report contract tests</a> and <a href="changes.json">change register</a>. Run the generator with NumPy and SciPy, one numerical thread and an external scratch directory; use its help for arguments. A fresh execution record is written only with the explicit measurement option. The original numerical outputs are retained with their digest; floating-point round-off can vary across numerical libraries without changing the reported engineering precision.</li>
</ul>

<h2 id="references">10. References, Acronyms and Appendices</h2>
<p>References are registered in <a href="#source-register">Section 9.2</a>. Detailed tables and unassessed design inputs are retained here so the body conclusions remain bound to the computed case.</p>
<h3 id="acronyms">10.1 Acronyms and status legend</h3>
<p>RAO means response amplitude operator; SI means International System of Units; QA means quality assurance. PASS means the stated numerical criterion is met. NOT_EVALUATED means the evidence or inputs required for the stated assessment are absent. “ASSUMED - to be confirmed” identifies an adopted assumption requiring confirmation before application.</p>
<h3 id="comparison-table">10.2 Refinement and long-wave tables</h3>
<p>Refinement differences use the finer coefficient as reference. A displayed value below the reporting resolution is bounded explicitly rather than printed as zero. {s("C")}</p>
{table(["ω (rad/s)", "Added-mass difference (%)", "Damping difference (%)", "Unrounded assessment", "Source"], refine_rows)}
{table(["Mode", "Heading (degrees)", "Comparator magnitude", "Units", "Source"], lw_rows)}
<h3 id="damping-appendix">10.3 Roll resolution and damping sensitivity</h3>
<p>A resonant roll grid and damping-sensitivity curve are NOT_EVALUATED. Roll restoring stiffness, structural and added inertia, natural period and operating frequency range are not supplied. An arbitrary grid or damping value is not substituted. The required grid resolves the eventual resonance peak and is refined until the selected motion target is stable. {s("D")} {s("A")}</p>
<div class="eq">B<sub>crit</sub> = 2√[C<sub>roll</sub>(I<sub>roll</sub> + A<sub>roll</sub>(ω<sub>n</sub>))]; ζ = B<sub>total</sub>/B<sub>crit</sub><br>ω<sub>n</sub>² = C<sub>roll</sub>/(I<sub>roll</sub> + A<sub>roll</sub>(ω<sub>n</sub>))</div>
<p>Critical roll damping is B<sub>crit</sub>, roll restoring stiffness is C<sub>roll</sub>, structural roll inertia is I<sub>roll</sub>, added roll inertia is A<sub>roll</sub>, natural angular frequency is ω<sub>n</sub> and the damping ratio is ζ. Total damping is B<sub>total</sub>. The relations apply to an uncoupled mode, and the inertia is evaluated at the same frequency as the damping basis. An undamped resonance has no finite linear motion amplitude; it is not an operating prediction. Damping percentages and warning limits remain required design inputs until their source and application basis are recorded. {s("D")}</p>
<h3 id="qa">10.4 QA pre-flight evidence</h3>
{table(["Check", "Evidence / status", "Source"], [
    ["Source inputs", "Read from public cylinder source tests; no result coefficient is typed into the generator.", s("C")],
    ["Single-frequency calculation", "Each coefficient case is calculated separately before the report is assembled.", s("M")],
    ["Boundary conditions and truncation", "Existing source tests inspect these; this report recomputes coefficients and refinement only.", s("C")],
    ["Numerical reproduction", "The accompanying report test compares returned values, chart datasets and the rebuilt HTML.", s("R")],
    ["Weight and buoyancy", "NOT_EVALUATED: structural mass is absent; displaced mass alone is reported.", s("Q")],
    ["Centres of gravity and buoyancy", "NOT_EVALUATED: no mass distribution or free-equilibrium assessment is supplied.", s("Q")],
    ["Panel mesh quality", "NOT_EVALUATED: no panel mesh is used by this numerical reference.", s("M")],
    ["Independent program agreement", "NOT_EVALUATED: no independently produced paired native result arrays are assessed.", s("B")],
])}
<h3 id="data-register">10.5 Missing-data and assumption register</h3>
{table(["Data", "Status", "Consequence / required action", "Source"], [
    ["Linear idealisation", "ASSUMED - to be confirmed", "Confirm small waves and motions, a flat bed and acceptable potential-flow applicability for the intended use.", s("T")],
    ["Structural mass, centres and inertia", "NOT_EVALUATED", "Supply these before equilibrium, stability or free-motion assessment.", s("I")],
    ["Metocean and operating range", "NOT_EVALUATED", "Supply a metocean analysis before regional period coverage or operability assessment.", s("I")],
    ["External damping and roll limits", "NOT_EVALUATED", "Supply sourced damping targets, warning limits and test evidence before design roll interpretation.", s("D")],
    ["Panel length and convergence", "NOT_EVALUATED", "Establish a body-specific refinement sequence before mesh acceptance.", s("A")],
    ["Independent reference arrays", "NOT_EVALUATED", "Retain the original published coefficients and conventions before program acceptance.", s("B")],
])}
<h3 id="test-inventory">10.6 Existing test inventory</h3>
<p>These source definitions are present in the repository and were inspected to identify evidence scope. Parameterised cases are not expanded, and these counts are not presented as passed tests. The report contract checks are recorded separately in the delivery validation record. {s("E")}</p>
{table(["Evidence topic", "Test definitions", "Meaning", "Source"], inventory)}
<h3 id="change-panel">10.7 Engineering changes</h3>
<label>Show changes since <select id="change-since"><option value="0">Before initial issue</option>{revision_options}</select></label>
<ul id="change-list">{"".join("<li>" + html.escape(change) + "</li>" for revision in changes["revisions"] for change in revision["changes"])}</ul>
<h3 id="comments">10.8 Section comments</h3>
<p>Comments are stored in a separate file beside this report. They do not alter its numerical evidence.</p>
<p><label>Section <select id="comment-section"></select></label></p>
<p><label>Comment <textarea id="comment-text" rows="4" cols="65"></textarea></label></p>
<p><button id="add-comment">Add comment</button> <button id="choose-folder">Choose report folder</button> <button id="save-comments">Save comments</button> <button id="export-comments">Export comments</button></p>
<p><label>Import saved comments <input id="import-comments" type="file" accept="application/json"></label></p>
<p id="comment-status" role="status">Comments remain in the open page until saved or exported.</p><ul id="comment-list"></ul>
<footer>Diffraction module analysis. The numerical scope and limitations stated in this report apply to every displayed result.</footer>
"""
    payload = lambda value: json.dumps(value, sort_keys=True, ensure_ascii=False).replace("</", "<\\/")
    displayed = re.findall(r'<span data-value-key="[^"]+" data-format="[^"]+">(.*?)</span>', body)
    point_labels = [re.findall(r"<title>(.*?)</title>", plot) for plot in plots.values()]
    # Comment identity uses displayed precision; raw calculation digests stay exact.
    comment_digest = hashlib.sha256(payload([displayed, point_labels]).encode("utf-8")).hexdigest()
    output.write_text(
        '<!DOCTYPE html>\n<html lang="en" data-theme="light" data-evidence="' + comment_digest + '">\n'
        '<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">'
        '<title>Diffraction module analysis</title>\n<style>\n' + brand + '\n' + style + '</style></head>\n'
        '<body class="hs-doc"><main class="wrap">' + body + '</main>\n'
        '<script type="application/json" id="example-data">' + payload(data) + '</script>\n'
        '<script type="application/json" id="chart-data">' + payload(charts) + '</script>\n'
        '<script type="application/json" id="changes-data">' + payload(changes) + '</script>\n'
        '<script id="report-interactions">' + INTERACTIONS + '</script>\n</body></html>\n', encoding="utf-8", newline="\n",
    )


def main():
    """Compute once with resumable external records, then render the report."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache-dir", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=HERE / "report.html")
    parser.add_argument("--measure", action="store_true", help="Record the first uncached computation timing")
    args = parser.parse_args()
    if args.measure and (HERE / "execution.json").exists():
        parser.error("An execution record already exists; it is retained as measured evidence")
    if args.measure and args.cache_dir.exists() and any(args.cache_dir.glob("*.json")):
        parser.error("A first computation measurement requires an unused numerical cache")
    for variable in ("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS"):
        os.environ[variable] = "1"
    start = time.perf_counter()
    data = compute_example(args.cache_dir)
    elapsed = time.perf_counter() - start
    if args.measure:
        import platform
        import psutil
        record = {"elapsed_seconds": round(elapsed, 1), "case_count": len(data["coefficients"]),
                  "numerical_threads": 1, "architecture": platform.machine(),
                  "logical_processors": os.cpu_count(),
                  "memory_gib": round(psutil.virtual_memory().total / 1024 ** 3, 1),
                  "example_digest": example_digest(data), "example": data,
                  "numerical_source_digest": numerical_source_digest(), "date": datetime.now(timezone.utc).date().isoformat(),
                  "scope": "Numerical imports, input extraction, coefficient calculations and comparator curves; rendering excluded"}
        (HERE / "execution.json").write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8", newline="\n")
    build_report(data, args.output)
    print("Report generated; numerical evidence digest verified.")


if __name__ == "__main__":
    main()
