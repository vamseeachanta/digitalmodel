# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build the bounded hull-module analysis without meshing or solving.

Both --scratch and --output are explicit. Numerical sources and display controls
are documented in sources.json; all intermediate files remain in external scratch.
"""

import argparse
import hashlib
import html
import json
import math
import re
from fractions import Fraction
from pathlib import Path
from types import ModuleType

import numpy as np

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
METHOD = ROOT / "src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py"
METHOD_BYTES = METHOD.read_bytes()
# Load only the numerical file; the package initializer imports plotting routes.
forward_speed = ModuleType("hull_report_forward_speed")
exec(compile(METHOD_BYTES, str(METHOD), "exec"), forward_speed.__dict__)
# sources.json, numerical-control: specified floating-point roundoff bounds.
RTOL = ATOL = 1e-12
HEADING_LABELS = ("0", "π/2", "π")  # sources.json, hand-calculation.


def external_scratch(scratch):
    """Resolve before writing; protect checkout files and symlinked paths."""
    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Scratch must be outside the checkout")
    scratch.mkdir(parents=True, exist_ok=True)
    return scratch


def hand_reference():
    """Derive the specified anchors using rational arithmetic, not production."""
    # sources.json, hand-calculation: independent exact inputs and cosines.
    omega, gravity = [Fraction(2), Fraction(4)], Fraction(8)
    k = [w * w / gravity for w in omega]
    cosines = [Fraction(1), Fraction(0), Fraction(-1)]
    speed = Fraction(1)
    return {
        "k": [float(ki) for ki in k],
        "moving": [[float(w - ki * speed * c) for c in cosines]
                   for w, ki in zip(omega, k)],
        "stationary": [[float(w)] * len(cosines) for w in omega],
    }


def source_digests():
    """Bind the actual method, report sources, tests, generator and layout."""
    if METHOD.read_bytes() != METHOD_BYTES:
        raise ValueError("Imported calculation source changed; restart the builder")
    register = json.loads((HERE / "sources.json").read_text(encoding="utf-8"))
    paths = {p for entry in register.values() for p in entry.get("paths", [])}
    paths.update(entry["path"] for entry in register.values() if "path" in entry)
    paths.update(str(p.relative_to(ROOT).as_posix()) for p in
                 (Path(__file__).resolve(), HERE / "sources.json", HERE / "changes.json"))
    return {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
            for path in sorted(paths)}


def calculate(scratch):
    """Execute only dispersion and signed encounter-frequency transformations."""
    external_scratch(scratch)
    # sources.json, hand-calculation: synthetic inputs, not physical defaults.
    inputs = {
        "g": 8.0, "omega": [2.0, 4.0], "speed": 1.0,
        "stationary_speed": 0.0, "heading_rad": [0.0, math.pi / 2, math.pi],
        "water_depth": "deep water (infinite)",
    }
    omega, headings = np.array(inputs["omega"]), np.array(inputs["heading_rad"])
    original_omega, original_headings = omega.copy(), headings.copy()
    k = forward_speed.wave_number(omega, water_depth=np.inf, g=inputs["g"])
    moving = forward_speed.encounter_frequency(
        omega, inputs["speed"], headings, water_depth=np.inf, g=inputs["g"],
    )
    stationary = forward_speed.encounter_frequency(
        omega, inputs["stationary_speed"], headings, water_depth=np.inf, g=inputs["g"],
    )
    reference = hand_reference()
    data = {
        "inputs": inputs, "k": k.tolist(), "moving": moving.tolist(),
        "stationary": stationary.tolist(), "reference": reference,
        "rtol": RTOL, "atol": ATOL,
        "inputs_preserved": bool(np.array_equal(omega, original_omega)
                                 and np.array_equal(headings, original_headings)),
        "units": {
            "g": "m/s²", "omega": "rad/s", "speed": "m/s",
            "stationary_speed": "m/s", "heading_rad": "rad", "water_depth": "m",
            "k": "1/m", "moving": "rad/s", "stationary": "rad/s",
            "difference_k": "1/m", "difference_moving": "rad/s",
            "difference_stationary": "rad/s", "rtol": "dimensionless",
            "atol": "same unit as compared quantity",
        },
        "source_sha256": source_digests(),
    }
    for key, result in (("k", k), ("moving", moving), ("stationary", stationary)):
        data["difference_" + key] = (result - reference[key]).tolist()
    return data


def validate(data):
    """Fail closed if a returned calculation disagrees with the hand case."""
    for key, expected in hand_reference().items():
        actual = np.asarray(data[key])
        if actual.shape != np.shape(expected) or not np.allclose(
                actual, expected, rtol=RTOL, atol=ATOL):
            raise ValueError(f"{key} disagrees with the hand calculation")
        reference = np.asarray(data["reference"][key])
        difference = np.asarray(data["difference_" + key])
        if (reference.shape != np.shape(expected)
                or not np.array_equal(reference, expected)
                or difference.shape != actual.shape
                or not np.array_equal(difference, actual - expected)):
            raise ValueError(f"{key} reference or difference disagrees with the hand calculation")
    if not data["inputs_preserved"]:
        raise ValueError("Input preservation failed for the hand calculation")


def cell(data, path, scientific=False, label=None):
    """Render a visible value with an auditable payload path and units."""
    value = data
    parts = path.split(".")
    for part in parts:
        value = value[int(part)] if isinstance(value, list) else value[part]
    key = parts[1] if parts[0] in ("inputs", "reference") else parts[0]
    unit = data["units"][key]
    shown = label if label is not None else format(value, ".1e" if scientific else ".1f")
    return (f'<span data-value="{path}" data-unit="{html.escape(unit)}">'
            f"{html.escape(shown)}</span>")


def heading_cells(data):
    return "".join(f"<th>β = {cell(data, f'inputs.heading_rad.{j}', label=label)} (rad)</th>"
                   for j, label in enumerate(HEADING_LABELS))


def result_table(data, key):
    rows = []
    for i, row in enumerate(data[key]):
        values = "".join(f"<td>{cell(data, f'{key}.{i}.{j}')}</td>" for j in range(len(row)))
        rows.append(f'<tr><td>{cell(data, f"inputs.omega.{i}")}</td>{values}</tr>')
    return ('<table><thead><tr><th>ω (rad/s)</th>' + heading_cells(data)
            + "</tr></thead><tbody>" + "".join(rows) + "</tbody></table>")


def comparison_table(data):
    rows = []
    for key in ("k", "moving", "stationary"):
        for indices in np.ndindex(np.shape(data[key])):
            suffix = ".".join(str(i) for i in indices)
            path = f"{key}.{suffix}"
            rows.append(f"<tr><td>{path}</td><td>{cell(data, path)}</td>"
                        f'<td>{cell(data, "reference." + path)}</td>'
                        f'<td>{cell(data, "difference_" + path, scientific=True)}</td>'
                        f'<td>{data["units"][key]}</td><td>[hand-calculation]</td></tr>')
    return ('<table><thead><tr><th>Payload coordinate</th><th>Calculated</th>'
            '<th>Hand calculation</th><th>Signed difference</th><th>Unit</th><th>Source</th>'
            "</tr></thead><tbody>" + "".join(rows) + "</tbody></table>")


def phase_sketch(data):
    """Generate cardinal propagation arrows from the supplied heading array.

    Pixel dimensions are schematic layout coordinates, not engineering data.
    """
    arrows = []
    for j, beta in enumerate(data["inputs"]["heading_rad"]):
        center = 130 + j * 250
        cosine = math.cos(beta)
        x, y = center + 70 * cosine, 145 - 70 * math.sin(beta)
        arrows.append(f'<g data-heading="{beta}" data-cosine="{cosine}">'
                      f'<path d="M{center} 145 L{x:.1f} {y:.1f}" marker-end="url(#arrow)"/>'
                      f'<text fill="currentColor" stroke="none" x="{center - 80}" y="210">β = {HEADING_LABELS[j]} rad</text>'
                      f'<text fill="currentColor" stroke="none" x="{center - 80}" y="240">{("Following", "Beam", "Head")[j]} seas</text></g>')
    return ('<svg viewBox="0 0 760 275" role="img" aria-label="Observer moves in positive x; wave propagation follows beta">'
            '<defs><marker id="arrow" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto">'
            '<path d="M0 0 L6 3 L0 6 Z" fill="currentColor"/></marker></defs>'
            '<g stroke="currentColor" fill="none"><path d="M40 35 H710" marker-end="url(#arrow)"/>'
            + "".join(arrows) + '</g><text x="40" y="20">Observer: x = U t; U points along +x</text></svg>')


def render(scratch):
    """Surround fresh arithmetic with the established self-contained layout."""
    scratch = external_scratch(scratch)
    data = calculate(scratch)
    validate(data)
    (scratch / "calculation.json").write_text(
        json.dumps(data, sort_keys=True, indent=2, allow_nan=False) + "\n", encoding="utf-8",
    )
    changes = json.loads((HERE / "changes.json").read_text(encoding="utf-8"))
    history = "".join("<tr>" + "".join(f"<td>{html.escape(item[k])}</td>"
                                     for k in ("revision", "date", "change")) + "</tr>"
                      for item in changes)
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8"), flags=re.S)
    provenance = "".join(f"<li><code>{html.escape(path)}</code>; SHA-256: {digest}</li>"
                         for path, digest in data["source_sha256"].items())
    inputs = "".join(f"<tr><td>{label}</td><td>{cell(data, 'inputs.' + key)}</td>"
                     f'<td>{data["units"][key]}</td><td>[hand-calculation]</td></tr>'
                     for key, label in (("g", "Synthetic gravity"), ("speed", "Moving observer speed"),
                                        ("stationary_speed", "Stationary observer speed")))
    omega = ", ".join(cell(data, f"inputs.omega.{i}") for i in range(len(data["inputs"]["omega"])))
    wave_numbers = ", ".join(cell(data, f"k.{i}") for i in range(len(data["k"])))
    exclusions = ("BEM execution", "Meshing", "Empirical speed/amplitude corrections",
                  "Bank effects", "Standards conformity", "Finite-depth dispersion",
                  "Passing interaction", "Squat screening")
    not_evaluated = "".join(f"<li>{name}: NOT_EVALUATED.</li>" for name in exclusions)
    payload = json.dumps(data, sort_keys=True, allow_nan=False).replace("<", "\\u003c")
    change_payload = json.dumps(changes).replace("<", "\\u003c")
    page = f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Parametric hull module analysis</title><style>{brand}\n{layout}</style></head>
<body class="hs-doc"><main class="wrap">
<header><h1>Parametric hull module analysis</h1><p>Synthetic observer-frequency example and existing correction routes.</p></header>
<h2 id="s1">1. Summary</h2>
<p>The signed encounter-frequency transformation is checked against an independent phase derivation for the stated deep-water case. Every calculated entry agrees with the hand calculation within the roundoff criterion in section <a href="#s6">6</a>. The worked results appear in section <a href="#s5">5</a>. This agreement establishes arithmetic for this case; hydrodynamic response accuracy and engineering qualification are NOT_EVALUATED.</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{history}</tbody></table>
<h2 id="s2">2. Scope and public interfaces</h2>
<p>The module orchestrates hull variations, frequency-domain hydrodynamics and passing-interaction calculations. The bounded analysis executes only <code>wave_number</code> and <code>encounter_frequency</code>. Static source inspection is distinguished from executed arithmetic [methods, routes, delegates].</p>
<table><thead><tr><th>Route</th><th>Inputs, delegate and outputs</th><th>Assessment</th></tr></thead><tbody>
<tr><td>Sweep</td><td><code>SweepConfig</code> and hull catalog → <code>HullParametricSpace.generate_profiles</code> → <code>HullMeshGenerator.generate</code> → <code>run_bem_analysis</code> → <code>compute_rao</code> → result entries and <code>RAODatabase</code>; <code>sweep_to_dataframe</code> summarizes peaks.</td><td>Static mapping; execution NOT_EVALUATED.</td></tr>
<tr><td>Forward speed</td><td><code>wave_number</code> supplies dispersion to <code>encounter_frequency</code>. <code>correct_rao_for_speed</code> interpolates supplied response amplitudes and phases. <code>strip_theory_speed_correction</code> modifies supplied mass and damping arrays.</td><td>Only deep-water dispersion and encounter arithmetic are evaluated.</td></tr>
<tr><td>Shallow water</td><td><code>classify_depth</code> classifies supplied depth/draft. <code>dnv_shallow_water_factor</code> returns empirical factors; <code>validate_shallow_water_results</code> compares supplied heave added masses. Bank functions return force, moment or clearance screening values.</td><td>Empirical basis and physical accuracy NOT_EVALUATED.</td></tr>
<tr><td>Passing interaction and charts</td><td><code>hull_profile_to_vessel_config</code> converts profiles; <code>run_passing_ship_sweep</code> delegates to <code>PassingShipCalculator</code> and <code>generate_time_history</code>. Dataframe, envelope and threshold helpers feed plots. Chart helpers also plot supplied sweep responses and depth ratios.</td><td>Static mapping; no interaction or chart campaign.</td></tr>
<tr><td>Separate squat interface</td><td><code>squat_screening</code> contains empirical applicability and clearance screens; it is not exported by the package initializer.</td><td>NOT_EVALUATED in this encounter-frequency example.</td></tr>
</tbody></table>
<h2 id="s3">3. Inputs and units</h2>
<p>The input basis is synthetic and is defined in <a href="sources.json">the source register</a> [hand-calculation]. Gravity is a test parameter, not a physical gravity recommendation. No geometry, environmental survey, measured response or empirical coefficient is supplied.</p>
<table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead><tbody>{inputs}
<tr><td>Angular frequency array</td><td>{omega}</td><td>rad/s</td><td>[hand-calculation]</td></tr>
<tr><td>Water depth</td><td>{data["inputs"]["water_depth"]}</td><td>m; infinite-depth limit</td><td>[hand-calculation]</td></tr></tbody></table>
<h2 id="s4">4. Method and heading coordinate</h2>
<div class="eq">φ = k x cos(β) − ω t; x = U t; φ(U t,t) = −[ω − k U cos(β)] t.<br>ωₑ = −dφ/dt = ω − k U cos(β); k = ω²/g (deep water) [hand-calculation, methods].</div>
<p>Here φ is phase in radians, x is observer position in metres, t is time in seconds, U is observer speed, k is wave number and β measures wave propagation direction from positive x. The propagation projection along x is k cos(β). No absolute value is applied to the signed frequency. Negative values would indicate reversed phase progression; they are not exercised by this case.</p>
<figure>{phase_sketch(data)}<figcaption>Wave propagation relative to the observer direction; schematic only. The headings are the existing software contract [direction-contract], not a newly selected external convention.</figcaption></figure>
<p>The existing tests assign following seas to aligned propagation and head seas to opposing propagation. The contradictory docstring labels are corrected to match those tests and the unchanged arithmetic [methods, direction-contract]. Alignment with other heading definitions or external standards requires a separate convention assessment.</p>
<h2 id="s5">5. Worked values</h2>
<p>The wave numbers are {wave_numbers} 1/m [hand-calculation, methods]. Rows retain input-frequency order; columns retain heading order.</p>
{result_table(data, "moving")}
<p>Table: signed encounter frequencies (rad/s) for the moving observer [hand-calculation, methods].</p>
{result_table(data, "stationary")}
<p>Table: signed encounter frequencies (rad/s) for the stationary observer; each row retains its input angular frequency [hand-calculation, methods].</p>
<h2 id="s6">6. Arithmetic checks and criterion</h2>
<p>The independent reference uses exact rational arithmetic for dispersion and exact cardinal cosines, without calling production dispersion. The governing comparison is |calculated − reference| ≤ atol + rtol × |reference|. The relative bound is {cell(data, "rtol", scientific=True)} and the absolute bound is {cell(data, "atol", scientific=True)} in the compared quantity’s unit [numerical-control]. These are roundoff bounds, not physical accuracy limits.</p>
{comparison_table(data)}
<p>Table: every worked result and its independent reference, with signed differences computed from unrounded values. The tests assert both result shapes, every entry, original input preservation, units and visible rounding [report-checks].</p>
<p>The builder validates this arithmetic before rendering. The delivered calculation payload binds source and layout digests. The rebuild test compares two complete deliveries byte for byte with the committed artifacts; no volatile metadata is removed. A changed returned calculation fixture is rejected before rendering. These are software checks; suite execution counts belong to the delivery record and are not fabricated by this report.</p>
<h2 id="s7">7. Concrete correctness findings</h2>
<ul><li>The encounter-frequency docstring reversed the direction labels relative to its formula and the existing direction tests. Only those labels and the unsupported external-convention attribution are corrected [methods, direction-contract].</li>
<li><code>run_parametric_sweep</code> records configured forward speed in result metadata but does not pass it to the solver or invoke the forward-speed correction helpers. A recorded speed therefore does not establish a corrected response [routes, delegates].</li>
<li><code>_extract_hull_params</code> and <code>_parse_variation_id</code> split variation suffixes at underscores, although <code>_make_variation_id</code> emits parameter keys containing underscores. Parameter identity can be lost before dataframe or database storage [routes, delegates].</li>
<li><code>PassingShipSweepConfig.stagger_n_points</code> and <code>parallel</code> are not forwarded by the passing-interaction sweep; the time-history delegate uses its own sampling defaults. Configuring those fields does not establish sampling refinement or parallel execution [routes, delegates].</li></ul>
<p>The latter findings are static code observations and remain follow-up; numerical behavior is retained in this bounded report.</p>
<h2 id="s8">8. Limitations and NOT_EVALUATED register</h2>
<ul>{not_evaluated}</ul>
<p>Response interpolation, strip-theory coupling, nonmonotonic encounter mappings, negative signed-frequency cases, finite-depth convergence and invalid-input domains are not qualified by the worked case. The reported baseline tests exercise additional software behavior but do not independently qualify empirical coefficients or establish a physical benchmark. Standard names in existing source docstrings are source attributions only; their conformity is not established here. No meshing, BEM, licensed solver or hull campaign is executed.</p>
<h2 id="s9">9. Source register and reproduction</h2>
<p><a href="sources.json">Source register</a>; <a href="changes.json">local revision record</a>. These local records describe inputs, equations, roundoff bounds and source roles. The authoritative calculation resides in this repository. The synthetic case is ready for software arithmetic demonstration only; physical qualification is unavailable. No client or measured dataset is used.</p>
<details><summary>Exact source and layout SHA-256 bindings</summary><ol>{provenance}</ol></details>
<p>The existing module-report layout and palette are inlined from the registered sources; their numerical answers are not reused. All tables and the phase sketch are generated by this local builder. Full inputs, results and unrounded differences are embedded as JSON for readback.</p>
<pre>PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 uv run --no-project --with numpy python docs/reports/modules/parametric_hull_analysis/build_report.py --scratch "$TMPDIR/hull-calc-a" --output "$TMPDIR/hull-report-a"
PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 uv run --no-project --with numpy python docs/reports/modules/parametric_hull_analysis/build_report.py --scratch "$TMPDIR/hull-calc-b" --output "$TMPDIR/hull-report-b"</pre>
<p>Run from the repository root with the supplied cache variables exported. Both scratch directories and comparison deliveries remain outside the checkout. To regenerate the committed delivery explicitly, set <code>--output docs/reports/modules/parametric_hull_analysis</code>. No test writes to that destination. The builder requires NumPy; the existing domain tests also require pandas, pydantic, matplotlib, SciPy and xarray.</p>
<details id="changes-panel"><summary>Engineering changes</summary><p>{html.escape(changes[-1]["change"])}</p></details>
<h2 id="s10">10. Way forward and review notes</h2>
<p>The wider convention question requires a separate phase and heading assessment at each solver interface. The unused speed and sampling fields and the variation-key parsing findings require focused regression cases before any behavioral changes. Empirical correction and bank-effect sources require independent worked evidence before use in engineering decisions. A separate mesh-convergence and physical-response benchmark is needed to assess hull responses; a deterministic rebuild cannot supply that evidence.</p>
<p><a href="https://github.com/vamseeachanta/digitalmodel/issues/3327">The parent issue</a> remains open for deferred qualification. BEM means boundary element method; RAO means response amplitude operator. Maintainer review is required before engineering adoption.</p>
<label for="notes">Review notes</label><textarea id="notes" rows="5" style="width:100%"></textarea>
<button id="save-notes" type="button">Save review notes</button><p id="save-status" aria-live="polite">Notes are saved only after writing a file.</p>
</main><script id="calculation" type="application/json">{payload}</script>
<script id="changes" type="application/json">{change_payload}</script><script>{JAVASCRIPT}</script></body></html>
'''
    return "\n".join(line.rstrip() for line in page.splitlines()) + "\n"


JAVASCRIPT = r"""
document.getElementById('save-notes').addEventListener('click', async () => {
  const blob = new Blob([document.getElementById('notes').value], {type: 'text/plain;charset=utf-8'});
  const status = document.getElementById('save-status');
  try {
    if ('showSaveFilePicker' in window) {
      const handle = await window.showSaveFilePicker({suggestedName: 'review-notes.txt'});
      const stream = await handle.createWritable(); await stream.write(blob); await stream.close();
      status.textContent = 'Review notes saved.';
    } else {
      const url = URL.createObjectURL(blob); const link = document.createElement('a');
      link.href = url; link.download = 'review-notes.txt'; link.click(); URL.revokeObjectURL(url);
      status.textContent = 'Review notes downloaded for placement beside the report.';
    }
  } catch (error) { status.textContent = 'Notes were not saved: ' + error.message; }
});
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    destination = args.output.resolve()
    if (destination == ROOT or ROOT in destination.parents) and destination != HERE:
        raise ValueError("Output inside checkout must be the report delivery directory")
    page = render(args.scratch)
    destination.mkdir(parents=True, exist_ok=True)
    (destination / "index.html").write_text(page, encoding="utf-8", newline="\n")
    for name in ("changes.json", "sources.json"):
        content = (HERE / name).read_text(encoding="utf-8")
        (destination / name).write_text(content, encoding="utf-8", newline="\n")
    print("Generated index.html, changes.json and sources.json; arithmetic criterion met.")


if __name__ == "__main__":
    main()
