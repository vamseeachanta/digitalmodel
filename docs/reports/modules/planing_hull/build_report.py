# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the bounded hydrostatic analysis with explicit scratch and output paths."""
import argparse
import hashlib
import html
import json
import math
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
REGISTER = HERE / "source_register.json"
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "The synthetic below-chine hydrostatic example, independent pressure integral and method limitations are presented to establish a reproducible calculation basis.",
}, {
    "revision": "B", "date": "2026-10-28",
    "change": "Numerical dependencies are declared in the isolated rebuild and test commands, and the pressure projection is corrected to the horizontal plane to make the calculation reproducible and its force direction explicit.",
}]
# H: REL_TOL rationale in the existing independent hydrostatics test note.
REL_TOL = 64 * math.ulp(1.0)
REBUILD = ('PYTHONPATH=src uv run --no-project --with pytest --with numpy --with scipy python '
           'docs/reports/modules/planing_hull/build_report.py '
           '--scratch "$TMPDIR/planing-calculation-a" --output "$TMPDIR/planing-report-a"')


def canonical(data):
    """Encode portable, deterministic data, rejecting nonfinite numbers."""
    return json.dumps(data, sort_keys=True, indent=2, allow_nan=False) + "\n"


def external_scratch(scratch):
    """Resolve the intended scratch root before any calculation or write."""
    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents or scratch in ROOT.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    return scratch


def hand_values(depth, density, gravity):
    """H: integrate both side pressures independently for z=abs(x)."""
    h = max(depth, 0.0)  # H: emerged keel has no wetted side.
    b = h  # H: elementary unit-slope right triangle, no production helpers.
    area = h * h  # H: integral(2*z dz,0..h).
    force = 2 * density * gravity * (h * b - b * b / 2)
    return area, force


def source_digests(register):
    """Pin all calculation, inspection, layout and local delivery sources."""
    paths = {item["path"] for item in register["sources"] if "path" in item}
    paths.update(path.relative_to(ROOT).as_posix() for path in (REGISTER, HERE / "README.md"))
    return {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
            for path in sorted(paths)}


def calculate(scratch):
    """Call the public strip hydrostatics only; retain raw data externally."""
    from digitalmodel.hydrodynamics.planing_hull import PlaningHullGeometry, PlaningStripModel

    scratch = external_scratch(scratch)
    register = json.loads(REGISTER.read_text(encoding="utf-8"))
    inputs = register["inputs"]
    values = {key: item["value"] for key, item in inputs.items()}
    geometry = PlaningHullGeometry(**{key: values[key] for key in (
        "length", "beam", "deadrise", "chine_height", "lcg",
    )})
    model = PlaningStripModel(geometry, n_strips=values["n_strips"], rho=values["density"])
    rows = []
    for depth in register["worked_depths"]["value"]:
        area, hand = hand_values(depth, values["density"], values["gravity"])
        force = model.buoyancy_force_per_length(depth)
        difference = force - hand
        rows.append({
            "depth": depth, "area": area, "force": force, "hand_force": hand,
            "difference": difference,
            "passes": math.isclose(force, hand, rel_tol=REL_TOL, abs_tol=0.0),
            "units": {"depth": "m", "area": "m^2", "force": "N/m"},
        })
    # Presentation grid only: equal subdivision between the dry and deepest
    # worked anchors, with eight subdivisions per worked interval (H depths).
    lower, upper = rows[0]["depth"], rows[-1]["depth"]
    intervals = (len(rows) - 1) * 8
    curve = [{"depth": lower + (upper - lower) * i / intervals,
              "force": model.buoyancy_force_per_length(lower + (upper - lower) * i / intervals)}
             for i in range(intervals + 1)]
    data = {"inputs": inputs, "rows": rows, "curve": curve,
            "strip_length": model.dx, "strip_centres": model.strip_x.tolist(),
            "source_digests": source_digests(register)}
    validate(data)
    scratch.mkdir(parents=True, exist_ok=True)
    (scratch / "calculation.json").write_text(canonical(data), encoding="utf-8", newline="\n")
    return data


def validate(data):
    """Reject stale results/checks before emitting a report with a pass claim."""
    density = data["inputs"]["density"]["value"]
    gravity = data["inputs"]["gravity"]["value"]
    for row in data["rows"]:
        area, force = hand_values(row["depth"], density, gravity)
        for actual, expected in ((row["area"], area), (row["hand_force"], force),
                                 (row["force"], force),
                                 (row["difference"], row["force"] - force)):
            if not math.isclose(actual, expected, rel_tol=REL_TOL, abs_tol=0.0):
                raise ValueError("Stale calculation: independent oracle mismatch")
        if not row["passes"]:
            raise ValueError("Stale calculation: independent oracle check failed")
    for point in data["curve"]:
        force = hand_values(point["depth"], density, gravity)[1]
        if not math.isclose(point["force"], force, rel_tol=REL_TOL, abs_tol=0.0):
            raise ValueError("Stale calculation: curve oracle mismatch")


def number(key, value, unit, decimals, element="span"):
    """Identify each visible worked number for independent display assertions."""
    text = f"{value:.{decimals}f}"
    # Display differences with one decimal and no negative zero.
    if float(text) == 0:
        text = f"{0:.{decimals}f}"
    return (f'<{element} data-value="{html.escape(key)}" data-unit="{html.escape(unit)}">'
            f'{text}</{element}>')


def force_figure(data):
    """Generate a static SVG curve and separate pressure-integral anchors."""
    rows = data["rows"]
    lower, upper = rows[0]["depth"], rows[-1]["depth"]
    scale = max(row["hand_force"] for row in rows)
    # SVG view-box margins and sizes are presentation coordinates only.
    def position(depth, force):
        return 80 + (depth - lower) / (upper - lower) * 600, 300 - force / scale * 240

    points = " ".join(f"{x:.3f},{y:.3f}" for x, y in (
        position(point["depth"], point["force"]) for point in data["curve"]
    ))
    anchors = "".join(
        f'<circle class="hand-anchor" cx="{x:.3f}" cy="{y:.3f}" r="5" fill="white" stroke="currentColor"><title>Independent pressure integral</title></circle>'
        for x, y in (position(row["depth"], row["hand_force"]) for row in rows)
    )
    labels = "".join(
        f'<text x="{position(row["depth"], 0)[0]:.3f}" y="325" text-anchor="middle">'
        f'{number(f"axis_depth_{i}", row["depth"], "m", 2, element="tspan")}</text>'
        for i, row in enumerate(rows)
    )
    # Axis mid-point is a display subdivision, not a new worked case.
    labels += "".join(
        f'<text x="72" y="{position(lower, force)[1]:.3f}" text-anchor="end">'
        f'{number(f"axis_force_{i}", force, "N/m", 1, element="tspan")}</text>'
        for i, force in enumerate((0.0, scale / 2, scale))
    )
    return f'''<figure><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 760 375" role="img" aria-label="Upward hydrostatic force per length against keel submergence" style="width:100%;height:auto">
<path d="M80 60 V300 H680" fill="none" stroke="currentColor"/>
<polyline id="force-curve" points="{points}" fill="none" stroke="var(--brand)" stroke-width="3"/>
{anchors}{labels}<text x="380" y="357" text-anchor="middle">Keel submergence (m)</text>
<text x="80" y="30">Upward force per length (N/m)</text></svg>
<figcaption>Builder-computed strip hydrostatics (solid curve, M) and independently integrated hand anchors (circles, H). The plotted interval remains below the chine; emerged cases are dry.</figcaption></figure>'''


def input_table(data):
    labels = {"lcg": "Longitudinal centre from bow", "density": "Synthetic fluid density"}
    rows = "".join(
        f'<tr><td>{html.escape(labels.get(key, key.replace("_", " ")))}</td><td>'
        f'{number(key, item["value"], item["unit"], item["decimals"])}</td>'
        f'<td>{html.escape(item["unit"])}</td><td>{html.escape(item["source"])}</td></tr>'
        for key, item in data["inputs"].items()
    )
    return '<table><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis</th></tr>' + rows + '</table>'


def result_table(data):
    rows = []
    for i, row in enumerate(data["rows"]):
        fields = (("depth", "m", 3), ("area", "m^2", 4), ("force", "N/m", 3),
                  ("hand_force", "N/m", 3), ("difference", "N/m", 1))
        cells = "".join(f'<td>{number(f"row_{i}_{key}", row[key], unit, decimals)}</td>'
                        for key, unit, decimals in fields)
        rows.append(f'<tr>{cells}<td>{"PASS" if row["passes"] else "FAIL"}</td></tr>')
    return '''<table><tr><th>Depth (m)</th><th>Hand area (m²)</th><th>Module force (N/m)</th><th>Hand force (N/m)</th><th>Module − hand (N/m)</th><th>Roundoff criterion</th></tr>''' + "".join(rows) + '</table>'


def source_table(register, data):
    entries = []
    for source in register["sources"]:
        target = "../../../../" + source["path"] if "path" in source else source["url"]
        label = source.get("path", source["locator"])
        entries.append(f'<li id="ref-{source["id"]}"><b>{source["id"]}</b>: '
                       f'<a href="{html.escape(target, quote=True)}">{html.escape(label)}</a>; '
                       f'{html.escape(source["locator"])}. {html.escape(source["use"])}</li>')
    digests = "".join(f'<li>{html.escape(path)}; SHA-256: <code>{digest}</code></li>'
                      for path, digest in data["source_digests"].items())
    return "<ol>" + "".join(entries) + "</ol><details><summary>Exact source digests</summary><ul>" + digests + "</ul></details>"


def render(scratch):
    """Render all displayed worked numbers from freshly checked calculation data."""
    data = calculate(scratch)
    validate(data)  # A changed returned value cannot reuse an earlier pass flag.
    register = json.loads(REGISTER.read_text(encoding="utf-8"))
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8"), flags=re.S)
    history = "".join(f'<tr><td>{item["revision"]}</td><td>{item["date"]}</td><td>{item["change"]}</td></tr>' for item in CHANGES)
    centres = ", ".join(number(f"centre_{i}", x, "m", 3) for i, x in enumerate(data["strip_centres"]))
    summary_force = number("summary_force", data["rows"][2]["force"], "N/m", 3)
    payload = canonical(data).replace("<", "\\u003c")
    page = f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Planing hull module analysis</title><style>{brand}\n{layout}</style></head>
<body class="hs-doc"><main class="wrap">
<header><h1>Planing hull module analysis</h1><p>Draft — synthetic below-chine hydrostatics</p>
<p>Strip forces and motion data flow, with a bounded independent worked calculation.</p></header>
<h2 id="s1">1. Executive Summary</h2>
<p>The strip hydrostatics are compared with an independently integrated side-pressure load for a synthetic straight-sided wedge. The reference force is {summary_force} N/m [H, M]. All worked cases meet the binary64 roundoff criterion stated in the method; dry cases return exact zero. This conclusion covers the specified below-chine geometry and gravity convention.</p>
<p>Dynamic accuracy and physical release are NOT_EVALUATED because experimental or qualified public response targets are absent. A reproducible rebuild establishes calculation freshness only.</p>
<h3>Revision history</h3><table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>{history}</table>
<h2 id="s2">2. Introduction</h2>
<p>The public interface exports geometry, strip-force methods, the motion solver, a response data class and a response convenience function [P]. The analysis will support review of the implementation without extending its engineering qualification. The <a href="https://github.com/vamseeachanta/digitalmodel/issues/3329">module issue</a> remains open for the deferred methods.</p>
<p>The calculation calls only the geometry and strip hydrostatics. No response campaign or empirical coefficient adjustment is performed by this builder.</p>
<h2 id="s3">3. Design Data and Basis</h2>
<p>{html.escape(register["basis"])} Inputs are selected for hand integration, not taken from an operating hull. The chine lies on each straight side at half-beam [H, D]. All worked submerged depths lie strictly below it.</p>
{input_table(data)}<p>Table: Synthetic worked inputs, units and source basis.</p>
<p>The strip length is {number("strip_length", data["strip_length"], "m", 3)} m. Strip centres from the bow are {centres} m [M]. This layout exposes an accidental length multiplier: a force reported per unit length does not acquire the strip length. A total strip force would require a separate integration and would have force units.</p>
<h2 id="s4">4. Analysis Methodology</h2>
<p>For the specified wedge, height above keel equals absolute transverse distance. Integrating submerged width gives cross-sectional area. Independently, gauge pressure is integrated over the horizontal projection of both straight sides [H]. The upward force differential is dFz = p cos(beta) ds = p dx, where beta is the side angle to the horizontal, ds is side length and dx is horizontal width.</p>
<div class="eq">h₊ = max(h, 0); b = h₊; A = ∫ 2z dz = h₊²;<br>F′ = 2∫ ρg(h₊ − x) dx = 2ρg(h₊b − b²/2) = ρgA [H].</div>
<p>The integrals run from the keel/centreline to the waterline. Here h is keel depth (m), b is waterline half-width (m), A is area (m²), ρ is density (kg/m³), g is gravity (m/s²), and F′ is upward load per length (N/m). The side count and integral factors are mathematical definitions. Gravity follows the rounded convention in H; G supplies a public reference for that convention.</p>
<p>The production body evaluates area as squared depth divided by the deadrise tangent [M]. The independent hand expression uses the elementary unit slope. Raw forces are compared using relative tolerance equal to 64 binary64 epsilons [H], with zero absolute tolerance; dry cases require exact zero. This allowance covers degree conversion, trigonometry and short arithmetic chains, not model uncertainty or discretisation. Differences are displayed to one decimal place; acceptance uses unrounded values.</p>
<h3>Strip and motion data flow — inspection only</h3>
<p>Geometry validation and properties feed the strip constructor, which sets longitudinal centres, strip length and the deadrise tangent [D, M]. Added mass takes a supplied half-wetted beam; the contact-width method takes entry velocity and duration; the simplified entry load takes entry velocity; hydrostatics take keel depth; steady lift takes speed and trim, with wetted length unused [M]. These methods return per-length quantities.</p>
<p>The motion constructor estimates displacement mass and pitch inertia. Its added-mass delegate integrates a full-half-beam strip value along the hull; restoring coefficients use a prescribed waterplane approximation and damping uses a critical-damping fraction [S]. Wave number and encounter-phase delegates feed clipped wave-velocity strip loads, which are multiplied by strip length and summed into force and moment. Separate heave and pitch state equations are advanced by the time integrator. The convenience function discards an initial transient window, selects nearest Fourier bins at encounter harmonics, normalises amplitudes and returns phase angles [S]. These steps are described, not qualified by the worked hydrostatic case.</p>
<h2 id="s5">5. Detailed Results</h2>
{result_table(data)}<p>Table: Hand area and force from H compared with the fresh module call M; signed difference is module minus hand. The source register supplies all input units. Zero denotes a true dry result, not missing data.</p>
{force_figure(data)}
<p>The deeper hand anchor follows the squared-depth relation in H. The below-chine curve is generated by fresh calls to the strip method; circles use the independent pressure integral. No dynamic response is plotted.</p>
<h2 id="s6">6. Concrete Correctness Findings</h2>
<p>The following source findings are separated from the passing hydrostatic comparison. Source locations refer to the exact bytes pinned in the register. No production correction is made within this report scope.</p>
<table><tr><th>Location</th><th>Finding</th><th>Effect on this analysis</th></tr>
<tr><td>M, strip_model.py:156–159</td><td>The hydrostatic docstring first multiplies squared depth by a tangent of a different angle, then states division by the deadrise tangent. The body at lines 173–174 uses division.</td><td>The pressure integral supports the body for straight sides. The selected unit slope cannot distinguish multiplication from division; the existing multi-angle tests in H provide that separate evidence.</td></tr>
<tr><td>M, strip_model.py:12–26, 95–118, 124–146</td><td>The contact-width square root has inconsistent length dimensions. The displayed derivative simplification has a different coefficient from its preceding expression; the simplified entry-force expression is dimensionally incomplete for load per length. The entry-force body does not use duration or the contact-width method.</td><td>Contact width and impact load remain outside computed qualification; the formulas require a separate dimensional and primary-source review.</td></tr>
<tr><td>S, solver.py:7–18, 165–198, 260–295, 346–354</td><td>The overview describes nonlinear coupling and damping from an added-mass derivative. The derivative body advances separate heave and pitch equations with excitation from wave velocity alone; damping is a prescribed critical-damping fraction. The restoring delegate does not use its speed argument or call steady planing lift.</td><td>Coupled dynamic accuracy, damping and speed-dependent restoring are not established by the overview or this example.</td></tr>
<tr><td>S, solver.py:45–50, 437–467</td><td>Phases are described relative to a wave crest; the body returns Fourier angles after slicing the transient without explicitly restoring the original time reference or comparing a wave-reference transform.</td><td>The phase-reference convention requires an independent synthetic-signal check before use.</td></tr>
<tr><td>D, geometry.py:42–62; M, strip_model.py:53–71, 171–174; S, solver.py:339–344, 413–449</td><td>Finite-value checks are incomplete, strip count and time settings are not guarded, the wedge area is not capped at the chine, and calm/short-window convenience branches return zero without distinguishing invalid or insufficient settings.</td><td>The report fixes finite synthetic inputs and stays below the chine. General input robustness, above-chine behavior and silent fallback status remain deferred.</td></tr>
</table><p>Table: Inspected inconsistencies and their consequence for the stated qualification boundary [D, M, S].</p>
<h2 id="s7">7. Limitations and Not Covered</h2>
<p>The worked case is a static straight-sided pressure balance. It does not establish hull equilibrium, total displacement, design loading, drag, resistance, power or suitability for an operating hull. Surface tension, free-surface transients, chine immersion and geometries outside the selected wedge are not represented. No dimensional defect is corrected by a passing rebuild.</p>
<ul><li>Impact added mass: NOT_EVALUATED against experiments or public benchmark data.</li>
<li>Wetting: NOT_EVALUATED against experiments or public benchmark data.</li>
<li>Dynamic damping: NOT_EVALUATED against experiments or public benchmark data.</li>
<li>Time integration: NOT_EVALUATED for accuracy or convergence against an independent dynamic reference.</li>
<li>Phase extraction: NOT_EVALUATED against an independent phase reference.</li>
<li>Response amplitudes: NOT_EVALUATED against experiments or public benchmark data.</li></ul>
<p>Existing tests T check shapes, finiteness, scaling, zero response and broad plausibility ranges. Those checks are behavioral evidence, not experimental targets. The hydrostatic tolerance is a roundoff criterion only; no engineering release tolerance is inferred for the deferred methods.</p>
<h2 id="s8">8. Way Forward</h2>
<p>The calculation maintainer should reconcile the hydrostatic docstring with the integral, then review impact dimensions and source attributions before selecting a dynamic benchmark. Input-domain guards and explicit insufficient-window status should be specified and tested. The coordinate, velocity and phase-reference conventions should be documented together.</p>
<p>For motion qualification, independently sourced nonzero targets, governing geometry and loading, units, applicable speed and frequency ranges, and acceptance criteria are required. A synthetic signal should first establish phase and amplitude extraction; time-step, strip and window refinement should then be assessed against those criteria. No operating recommendation follows until that evidence is available. Deferred work remains on the open module issue.</p>
<h2 id="s9">9. References and Appendices</h2>
<p>The <a href="source_register.json">source register</a> records the synthetic basis, public gravity citation, units and intended use. The <a href="README.md">local README</a> provides rebuild and test commands. Local sources are bound by SHA-256 digests; public gravity is a cited convention, not a local measurement. The legacy gravity query cited in H did not resolve during source inspection; the alternative public reference G supplies the stated convention.</p>
{source_table(register, data)}
<h3>Rebuild and actual checks</h3><pre>{html.escape(REBUILD)}</pre>
<p>Scratch and output shall be distinct, with scratch outside the checkout. The builder writes its complete calculation to scratch and emits only the report and <a href="changes.json">change record</a> to output. No volatile metadata is generated. The source digests identify the evaluated source state; a changed source requires a rebuild.</p>
<p>The builder executes the pressure-integral comparison at every worked depth and checks every plotted sample before rendering. The dedicated tests independently assert all worked inputs, raw values, units, displayed rounding, curve coordinates and hand anchors; compare two separate CLI builds byte for byte with the delivered files; alter a returned force to require visible change or freshness rejection; and check links, revision rows and the section count without a browser [R]. Test execution counts and outcomes accompany the delivery record; a clean rebuild is not physical qualification.</p>
<details id="changes-panel"><summary>Engineering changes</summary><p>{CHANGES[-1]["change"]}</p></details>
<h2 id="s10">10. Acronyms and Review Notes</h2>
<p>LCG denotes longitudinal centre of gravity; RAO denotes response amplitude operator; FFT denotes fast Fourier transform; RK denotes the explicit staged time integration described in S. PASS means the unrounded hydrostatic comparison meets the criterion in H. NOT_EVALUATED identifies missing qualification evidence, not a numerical zero.</p>
<label for="review-comments">Review comments</label><textarea id="review-comments" rows="4" style="width:100%"></textarea>
<button id="save-comments" type="button">Save comments</button><p id="save-status" role="status"></p>
<footer>Report scope: synthetic below-chine hydrostatic calculation. Dynamic qualification remains open.</footer>
<script id="calculation" type="application/json">{payload}</script>
<script>{COMMENTS_JS}</script></main></body></html>
'''
    return '\n'.join(line.rstrip() for line in page.splitlines()) + '\n'


# Adapted from the existing self-contained report's local comment export pattern.
COMMENTS_JS = '''document.getElementById('save-comments').addEventListener('click', async () => {
  const text = JSON.stringify({comments: document.getElementById('review-comments').value}, null, 2);
  const status = document.getElementById('save-status');
  try {
    if (window.showSaveFilePicker) {
      const handle = await window.showSaveFilePicker({suggestedName:'comments.json',
        types:[{description:'JSON comments',accept:{'application/json':['.json']}}]});
      const writable = await handle.createWritable(); await writable.write(text); await writable.close();
      status.textContent = 'Comments were saved to the selected file.';
    } else {
      const url = URL.createObjectURL(new Blob([text],{type:'application/json'}));
      const link = document.createElement('a'); link.href=url; link.download='comments.json'; link.click();
      setTimeout(() => URL.revokeObjectURL(url),1000);
      status.textContent = 'A download was requested. Confirm the comments file is present.';
    }
  } catch (error) { status.textContent = 'Comments were not saved. Select a writable file and retry.'; }
});'''


def build(scratch, output):
    """Validate roots, render externally, and write exactly the small delivery."""
    scratch = external_scratch(scratch)
    output = Path(output).resolve()
    if scratch == output or scratch in output.parents or output in scratch.parents:
        raise ValueError("Scratch and output must not overlap")
    page = render(scratch)
    output.mkdir(parents=True, exist_ok=True)
    for name, text in {"index.html": page, "changes.json": canonical(CHANGES)}.items():
        (output / name).write_text(text, encoding="utf-8", newline="\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    build(args.scratch, args.output)


if __name__ == "__main__":
    main()
