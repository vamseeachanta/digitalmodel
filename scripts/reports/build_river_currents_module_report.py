# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build the bounded river-current module analysis using normal public imports.

Supply --scratch outside the checkout and an explicit --output-dir. The small
worked case uses stipulated mathematical inputs; existing towing and extreme
examples are linked without running another campaign. No empirical calibration
or operational acceptance is inferred from an exact software rebuild.
"""

import argparse
import ast
import hashlib
import html
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = "src/digitalmodel/hydrodynamics/river_currents"
REVIEWED_REVISION = "09089a121a16069c4b657d1a320a630a23ab1dd5"
LAYOUT = "docs/api/cfd/turbulent-flat-plate-verification.html"
PALETTE = "docs/api/_assets/brand.css"
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "Initial module analysis records the synthetic profile and channel-correction arithmetic, public function review and unresolved physical basis so that the evidence can be rebuilt and assessed.",
}]

# All physical inputs are stipulated for the hand derivation in the report and
# its independent test. None is an empirical recommendation or package default.
INPUTS = {
    "depth_m": {"value": 8.0, "unit": "m", "basis": "Stipulated rectangular channel [H]."},
    "surface_speed_m_s": {"value": 3.0, "unit": "m/s", "basis": "Stipulated surface speed [H]."},
    "exponent_m": {"value": 2.0, "unit": "dimensionless", "basis": "Stipulated square-root profile [H]."},
    "retardation": {"value": 0.4, "unit": "dimensionless", "basis": "Assumed lateral shape only [H]."},
    "width_m": {"value": 20.0, "unit": "m", "basis": "Stipulated rectangular channel [H]."},
    "beam_m": {"value": 4.0, "unit": "m", "basis": "Stipulated rectangular obstruction [H]."},
    "draft_m": {"value": 2.0, "unit": "m", "basis": "Stipulated rectangular obstruction [H]."},
}

# This register describes every non-private package function and fit method.
# Signature details and defaults remain in the linked, digest-bound source.
INVENTORY = [
    {"function": "profiles.power_law_profile", "inputs": "Bed height z, depth H (m), surface speed (m/s), exponent m (dimensionless)", "outputs": "Scalar or array speed (m/s)", "review": "Bed-referenced law; clips height to the wetted depth. Synthetic oracle exercised."},
    {"function": "profiles.power_law_depth_averaged", "inputs": "H (m), surface speed (m/s), exponent m", "outputs": "Depth mean (m/s)", "review": "Analytical power integral; agrees with the independent synthetic integral."},
    {"function": "profiles.log_law_profile", "inputs": "z, roughness length (m), shear speed (m/s), dimensionless wall constant", "outputs": "Scalar or array speed (m/s)", "review": "Clamps below roughness to zero. Roughness and shear require a separate physical basis."},
    {"function": "profiles.log_law_depth_averaged", "inputs": "H above roughness (m), shear speed (m/s), wall constant", "outputs": "Depth mean (m/s)", "review": "Integrates the positive log region and divides by the full depth."},
    {"function": "profiles.river_velocity_profile", "inputs": "Point count, H (m), law selector and corresponding profile inputs", "outputs": "Bed heights (m), speeds (m/s)", "review": "Uniform sampling wrapper; the report supplies its exponent explicitly."},
    {"function": "bank_effects.lateral_factor", "inputs": "Position and width (m), assumed retardation (dimensionless)", "outputs": "Dimensionless multiplier", "review": "Assumed symmetric quadratic has unit width mean; calibration NOT_EVALUATED."},
    {"function": "bank_effects.blockage_factor", "inputs": "Width, depth, beam and draft (m)", "outputs": "Dimensionless section multiplier", "review": "Rectangular remaining-area continuity; no local flow solution."},
    {"function": "bank_effects.bend_factor", "inputs": "Position and width (m), assumed outer bias (dimensionless)", "outputs": "Dimensionless multiplier", "review": "Assumed linear bias; right bank is outer bank. Independent of water-level rise."},
    {"function": "bank_effects.bend_superelevation", "inputs": "Speed (m/s), width and centreline radius (m), gravity (m/s²)", "outputs": "Outer-minus-inner water level (m)", "review": "Constant-speed radial balance; positive inner radius required; stable logarithm. Existing independent tests retained."},
    {"function": "bank_effects.corrected_profile", "inputs": "Scalar speed callback (m/s), fixed dimensionless factor", "outputs": "Corrected speed callback (m/s)", "review": "Finite nonnegative factor and finite callback result required; no coupled hydraulics."},
    {"function": "extremes.fit_weibull", "inputs": "Positive, finite, nonconstant samples in consistent speed or discharge units", "outputs": "WeibullFit: dimensionless shape, dimensional scale", "review": "Transformed-rank least squares, zero location; neither maximum likelihood nor uncertainty estimation."},
    {"function": "extremes.WeibullFit.return_level", "inputs": "Return period (years), independent sample rate (per year)", "outputs": "Return level in sample units", "review": "Uses annual nonexceedance with explicit sample rate. Stationarity and independence need evidence."},
    {"function": "extremes.fit_gumbel", "inputs": "Finite nonnegative, nonconstant annual maxima in consistent units", "outputs": "GumbelFit: dimensional location and scale", "review": "Transformed-rank least squares for annual maxima; fit support is unbounded."},
    {"function": "extremes.GumbelFit.return_level", "inputs": "Return period (years)", "outputs": "Annual-maximum level in sample units", "review": "One maximum per year is required; physical extremes NOT_EVALUATED."},
    {"function": "extremes.seasonal_summary", "inputs": "Finite nonnegative values and matching season labels", "outputs": "Count, mean and maximum per season in input units", "review": "Groups caller labels; does not annualise, decluster or fit a trend."},
    {"function": "vessel_forces.shallow_drag_factor", "inputs": "Water depth and draft (m)", "outputs": "Dimensionless drag multiplier", "review": "Full-width under-keel slot hypothesis; ignores side bypass and diverges at grounding."},
    {"function": "vessel_forces.current_load", "inputs": "Geometry (m), bed-referenced signed speed callback (m/s), depth (m), direction (degrees), density (kg/m³), collinear motion (m/s), slice count", "outputs": "Signed surge/sway (N) and yaw (N·m)", "review": "Midpoint signed quadratic integration over occupied draft; always applies slot hypothesis and approximate upstream coefficients."},
    {"function": "vessel_forces.towing_assessment", "inputs": "Loads (N, N·m), nominal pull (N), efficiency and reserve, optional yaw capacity (N·m)", "outputs": "Required/available pull, absolute yaw demand and comparison booleans", "review": "Steady force and yaw comparisons only. Missing yaw capacity accepts only zero moment; no navigation clearance."},
    {"function": "examples.compute", "inputs": "Revisioned synthetic basis dictionary with SI keys", "outputs": "Bank/extreme and towing result dictionaries", "review": "Calls shipped public calculations; linked existing examples are not rerun by this builder."},
    {"function": "examples.profile_svg", "inputs": "Depth (m), surface speed (m/s)", "outputs": "Inline SVG", "review": "Uses the example's default profile convention; this report generates its explicitly stipulated profile separately."},
    {"function": "examples.render", "inputs": "Basis dictionary and computed results", "outputs": "Self-contained analysis HTML", "review": "Includes revision history, assumptions and conditional towing narrative."},
    {"function": "examples.build", "inputs": "Explicit basis file and output directory", "outputs": "Analysis HTML, results JSON and byte-digest manifest", "review": "Deterministic rendering/build pattern reused; existing deliverables remain the reference for those examples."},
    {"function": "examples.main", "inputs": "Required basis and output CLI arguments", "outputs": "Writes the existing example deliverables", "review": "Command wrapper; no additional method or qualification."},
]


def json_text(value):
    """Serialize deterministic machine evidence without non-finite numbers."""
    return json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n"


def calculate():
    """Evaluate every reported result through the existing public API."""
    import digitalmodel.hydrodynamics.river_currents as api

    inp = {key: row["value"] for key, row in INPUTS.items()}
    depth, width = inp["depth_m"], inp["width_m"]
    speed, exponent = inp["surface_speed_m_s"], inp["exponent_m"]

    def profile(z):
        return api.power_law_profile(z, depth, speed, exponent_m=exponent)

    mean = api.power_law_depth_averaged(depth, speed, exponent_m=exponent)
    left, centre, right = [api.lateral_factor(x, width, inp["retardation"])
                           for x in (0, width / 2, width)]
    confinement = api.blockage_factor(width, depth, inp["beam_m"], inp["draft_m"])
    corrected = api.corrected_profile(profile, centre * confinement)
    rows = [
        ("bed_speed_m_s", profile(0), "m/s", "power_law_profile", "u at bed [H, P]"),
        ("quarter_speed_m_s", profile(depth / 4), "m/s", "power_law_profile", "u at quarter depth [H, P]"),
        ("surface_speed_m_s", profile(depth), "m/s", "power_law_profile", "u at surface [H, P]"),
        ("mean_speed_m_s", mean, "m/s", "power_law_depth_averaged", "Depth integral divided by H [H, P]"),
        ("discharge_per_width_m2_s", depth * mean, "m²/s", "H × power_law_depth_averaged", "Depth integral [H, P]"),
        ("left_bank_factor", left, "dimensionless", "lateral_factor", "x = left bank [H, B]"),
        ("centre_factor", centre, "dimensionless", "lateral_factor", "x = mid-width [H, B]"),
        ("right_bank_factor", right, "dimensionless", "lateral_factor", "x = right bank [H, B]"),
        # Simpson's rule is exact for the assumed quadratic, by integrating its
        # polynomial basis; weights 1,4,1 divided by 6 are mathematical constants.
        ("lateral_mean_factor", (left + 4 * centre + right) / 6, "dimensionless", "Simpson integral of lateral_factor", "Exact quadratic integral over normalized width [H, B]"),
        ("remaining_area_factor", confinement, "dimensionless", "blockage_factor", "A/(A - beam × draft) [H, B]"),
        ("corrected_quarter_speed_m_s", corrected(depth / 4), "m/s", "corrected_profile", "Fixed centre × confinement at quarter depth [H, B, P]"),
        ("corrected_surface_speed_m_s", corrected(depth), "m/s", "corrected_profile", "Fixed centre × confinement at surface [H, B, P]"),
    ]
    # Plot resolution is a presentation choice, not a hydraulic input. Sampling
    # includes both endpoints and the quarter-height point of the oracle.
    heights, speeds = api.river_velocity_profile(
        33, depth, model="power_law", surface_velocity=speed, exponent_m=exponent,
    )
    return {
        "reviewed_revision": REVIEWED_REVISION,
        "status": "Synthetic arithmetic only; physical and operational qualification NOT_EVALUATED",
        "inputs": INPUTS,
        "results": {key: {"value": float(value), "unit": unit, "method": method, "basis": basis}
                    for key, value, unit, method, basis in rows},
        "profile": {"height_m": heights.tolist(), "speed_m_s": speeds.tolist(),
                    "basis": "Public river_velocity_profile with the stipulated inputs [H, P]; uniform plotting samples only."},
    }


def source_link(path, label, line=None):
    """Resolve repository evidence relative to the delivered report directory."""
    target = "../../../../" + path + (f"#L{line}" if line else "")
    return f'<a href="{html.escape(target, quote=True)}">{html.escape(label)}</a>'


def inventory_rows():
    """Bind each reviewed function to its actual source line."""
    rows = []
    for entry in INVENTORY:
        module, *names = entry["function"].split(".")
        path = f"{PACKAGE}/{module}.py"
        node = ast.parse((ROOT / path).read_text(encoding="utf-8"))
        for name in names:
            node = next(child for child in node.body if getattr(child, "name", None) == name)
        cells = [source_link(path, entry["function"], node.lineno)]
        cells.extend(html.escape(entry[key]) for key in ("inputs", "outputs", "review"))
        rows.append("<tr>" + "".join(f"<td>{cell}</td>" for cell in cells) + "</tr>")
    return "\n".join(rows)


def profile_svg(data):
    """Generate an inline chart from the API samples, with units and no assets."""
    depth = data["inputs"]["depth_m"]["value"]
    speed = data["inputs"]["surface_speed_m_s"]["value"]
    # Coordinates below are presentation dimensions in SVG user units.
    points = " ".join(f"{70 + 340 * u / speed:.1f},{280 - 230 * z / depth:.1f}"
                      for z, u in zip(data["profile"]["height_m"], data["profile"]["speed_m_s"]))
    return f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 350" role="img" aria-label="Synthetic speed against height above bed">
<path d="M70 40 V280 H445" fill="none" stroke="currentColor"/>
<polyline points="{points}" fill="none" stroke="var(--brand)" stroke-width="3"/>
<text x="65" y="305">0.0</text><text x="400" y="305">{speed:.1f}</text>
<text x="30" y="285">0.0</text><text x="30" y="55">{depth:.1f}</text>
<text x="160" y="335">Speed (m/s)</text><text x="80" y="25">Height above bed (m)</text>
</svg>'''


def render(data):
    """Render the house layout around the computed input-method-answer chain."""
    escape = html.escape
    exemplar = (ROOT / LAYOUT).read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / PALETTE).read_text(encoding="utf-8"), flags=re.S)
    # Retain the shared styles while omitting inherited trailing whitespace.
    layout = "\n".join(line.rstrip() for line in layout.splitlines())
    brand = "\n".join(line.rstrip() for line in brand.splitlines())
    inputs = "\n".join(
        f'<tr><td>{escape(key)}</td><td>{row["value"]:.1f}</td><td>{escape(row["unit"])}</td><td>{escape(row["basis"])}</td></tr>'
        for key, row in data["inputs"].items())
    results = "\n".join(
        f'<tr><td>{escape(key)}</td><td>{row["value"]:.1f}</td><td>{escape(row["unit"])}</td><td>{escape(row["method"])}</td><td>{escape(row["basis"])}</td></tr>'
        for key, row in data["results"].items())
    history = "".join(f'<tr><td>{escape(row["revision"])}</td><td>{escape(row["date"])}</td><td>{escape(row["change"])}</td></tr>' for row in CHANGES)
    payload = json_text(data).replace("<", "\\u003c").replace("&", "\\u0026").replace(">", "\\u003e")
    r = data["results"]
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>River currents module analysis</title><style>{brand}\n{layout}
table{{display:block;overflow-x:auto}}svg{{max-width:520px;width:100%}}td{{vertical-align:top}}
</style></head><body class="hs-doc"><main class="wrap">
<header><h1>River currents module analysis</h1><p>Synthetic profile and rectangular-channel correction arithmetic; physical acceptance NOT_EVALUATED.</p></header>
<h2 id="s1">1. Summary</h2>
<p>The public bed-referenced power profile returns a depth mean of {r['mean_speed_m_s']['value']:.1f} m/s
and discharge per unit width of {r['discharge_per_width_m2_s']['value']:.1f} m²/s for the stipulated inputs [H, P].
The assumed lateral shape conserves its width mean; the confinement multiplier follows remaining-area continuity [H, B].
Independent hand arithmetic checks these identities. Empirical bank retardation, fitted extremes,
load coefficients and operating limits remain conditional or NOT_EVALUATED.</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{history}</tbody></table>
<h2 id="s2">2. Context and scope</h2>
<p>The reviewed source revision is <code>{REVIEWED_REVISION}</code>. The manifest records exact source
and generator digests. The review covers all public functions and fit methods in the package, including
the example builder [P, B, E, L, X]. It checks the compact profile/correction arithmetic; it does not establish
calibration, design acceptance or compliance. No calculation source correction was required for this case.</p>
<p>The existing {source_link('docs/domains/hydrodynamics/river_currents/river-current-analysis.html', 'towing and extreme-fit analysis')}
and {source_link('examples/hydrodynamics/river_currents/basis.json', 'synthetic example basis')} remain separate evidence [X].
Their outputs are retained unchanged; this report does not rerun or qualify a campaign.</p>
<h2 id="s3">3. Input basis</h2>
<p>[H] is a stipulated mathematical example, with no site data. Heights are measured upward from the bed.
The square-root exponent and retardation are explicit assumptions; no default river exponent is adopted.
The rectangular obstruction is an area-continuity abstraction. Missing measured inputs are not replaced by zero.</p>
<table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead><tbody>{inputs}</tbody></table>
<p>Evaluation heights are bed, quarter depth and surface; lateral evaluation positions are the two banks and mid-width [H].
The fixed centre and remaining-area multipliers illustrate composition only, without flow redistribution.</p>
<h2 id="s4">4. Method and public function review</h2>
<p>[P] gives u(z) = u_s (z/H)^(1/m), with height clipped to the wetted depth, and its depth integral.
The logarithmic alternative uses shear speed, roughness and a caller-selectable wall constant; it clamps
the invalid sub-roughness region to zero. Finite input validation is incomplete in the profile functions;
callers need to reject non-finite values before physical use. This remains an open finding.</p>
<p>[B] assumes f(x) = 1 − r + 6 r x(1 − x) on normalized width, and a separate linear bend bias
1 + a(2x − 1). These are unit-mean shapes, without empirical calibration. Confinement is
A/(A − beam × draft). Constant-speed bend water-level rise follows u²/g × ln((R + W/2)/(R − W/2));
positive inner radius is required. This radial-balance model does not predict secondary circulation.</p>
<p>[L] integrates signed relative speed times its absolute value over the occupied draft using midpoint
quadrature, then scales existing approximate force and yaw coefficients. A full-width under-keel slot
drag hypothesis is always applied. It ignores side bypass; joint use with a channel-area factor requires
a separate physical basis. The towing comparison uses resultant force × reserve / efficiency and absolute
yaw demand against supplied capacities. A positive boolean addresses only those steady comparisons.</p>
<p>[E] uses least squares on transformed plotting ranks (rank − 0.5)/n. The zero-location Weibull
annual return calculation uses F(level)^rate = 1 − 1/T; Gumbel requires annual maxima and uses
F(level) = 1 − 1/T. Here rate is independent samples per year and T is return period in years.
Distribution fitting, declustering, uncertainty, stationarity and physical upper/lower support require further evidence.
Seasonal summaries retain input units and supplied labels without annualising.</p>
<table><thead><tr><th>Public function or method</th><th>Inputs and units</th><th>Outputs and units</th><th>Review and limits</th></tr></thead><tbody>{inventory_rows()}</tbody></table>
<h2 id="s5">5. Worked calculation</h2>
<p>[H] independently stipulates H = 8 m, u_s = 3 m/s and m = 2, so u = 3√(z/8).
Substitution at z = 0, 2 and 8 m gives 0, 1.5 and 3 m/s. Direct depth integration gives
q = (3/√8)(2/3)8^(3/2) = 16 m²/s; mean speed = q/H = 2 m/s.
These are hand-derived expectations, compared with public API answers in the results table.</p>
<p>For stipulated r = 0.4, ∫₀¹ f(x) dx = 1 − r + 6r(1/2 − 1/3) = 1;
the bank factor is 0.6 and centre factor is 1.2. Simpson weights integrate this quadratic exactly [H].
The rectangular continuity factor is (20 × 8)/(20 × 8 − 4 × 2) = 20/19 [H, B].
The corrected quarter and surface speeds are respectively (3/2)(6/5)(20/19) = 36/19 m/s
and 3(6/5)(20/19) = 72/19 m/s. These fractions are exact arithmetic, not calibrated predictions.</p>
<figure>{profile_svg(data)}<figcaption>Public API samples of the stipulated profile; height above bed and speed in SI units [H, P]. Plot sampling is a presentation choice, not a convergence study.</figcaption></figure>
<h2 id="s6">6. Checks and evidence meaning</h2>
<p>The {source_link('tests/reporting/test_river_currents_module_report.py', 'report tests')} compare normal public API outputs
with the independent derivation [H]. Positive answers use relative tolerance 1e-12 and zero absolute tolerance;
the bed-zero check is separate. This allowance covers binary64 roundoff in the short arithmetic chain,
without adopting a hydraulic accuracy or operational limit.</p>
<p>Numerical oracles, production replay and byte freshness are separate checks. Negative controls change
the mean result magnitude or sign while the builder, embedded data and manifest still agree; the independent
oracle must reject them. Another control alters delivered results without updating their digest.
Two fresh external builds must match each other and the committed deliverables byte for byte.</p>
<p>Existing baseline tests are linked as evidence of their stated assertions, rather than physical qualification:
{source_link('tests/hydrodynamics/river_currents/test_profiles.py', 'profiles')},
{source_link('tests/hydrodynamics/river_currents/test_bank_effects.py', 'radial balance')} and
{source_link('tests/hydrodynamics/river_currents/test_extensions.py', 'extensions and existing examples')}.
Test execution counts and commands belong to the accompanying change record; this deterministic page contains no run timestamp.</p>
<h2 id="s7">7. Results and traceable answers</h2>
<p>Every answer below is returned by a public method or explicitly integrated from its returned values.
Displays use one decimal place; the linked JSON retains unrounded values for assertions.
The remaining-area factor is small enough that rounding hides detail; its exact hand basis is 20/19 [H].</p>
<table><thead><tr><th>Answer</th><th>Value</th><th>Unit</th><th>API or composition</th><th>Basis</th></tr></thead><tbody>{results}</tbody></table>
<p><a href="results.json">Full-precision calculation data</a> · <a href="manifest.json">Source and byte manifest</a> ·
<a href="README.md">Rebuild instructions</a></p>
<h2 id="s8">8. Limits and NOT_EVALUATED</h2>
<ul><li>Empirical retardation, bend bias, roughness and exponent selection: NOT_EVALUATED; measurements and geometry are absent.</li>
<li>Actual extreme levels: NOT_EVALUATED; no measured series, independence assessment, uncertainty interval or accepted return period is supplied.</li>
<li>Force coefficients, under-keel slot calibration and combined correction policy: conditional assumptions in existing examples; NOT_EVALUATED for physical use.</li>
<li>Operating limits, thrust at speed, tug resistance, navigation clearance, passing interaction, tidal reversal and unsteady flow: NOT_EVALUATED.</li>
<li>Profile non-finite input rejection and consistent signed-flow semantics across the logarithmic and power laws remain open review findings; no general input-hardening claim is made.</li>
<li>The public bibliographic attributions in the source docstrings have not been checked against original publications here. No licensed coefficient table, default exponent or external compliance claim is adopted.</li></ul>
<h2 id="s9">9. Way forward</h2>
<p>Retain the mathematical regression as software evidence. Before physical use, obtain surveyed geometry,
bed-referenced measured profiles, a consistent signed-flow convention and calibrated bank/bend/load inputs.
Assess the full-width slot and section-area hypotheses together before combining them. Qualify temporal sampling,
declustering and uncertainty before fitting physical extremes. The maintainers need to resolve finite-input
policy and establish a separate acceptance basis; this report does not select design or operating limits.</p>
<h2 id="s10">10. References and evidence</h2>
<ul><li>[H] Independent hand derivation in the worked calculation and the dedicated report test. All physical input values are stipulated synthetic assumptions.</li>
<li>[P] {source_link(PACKAGE + '/profiles.py', 'Profile implementation')}: bed law, analytical means and sampling. Mathematical coefficients follow the displayed assumed law.</li>
<li>[B] {source_link(PACKAGE + '/bank_effects.py', 'Bank and bend implementation')}: assumed shapes, constant-density continuity and radial-balance derivation. Docstring bibliographic links are unverified here.</li>
<li>[E] {source_link(PACKAGE + '/extremes.py', 'Extreme-fit implementation')}: probability transforms, plotting positions and return-level conventions; distribution names identify software models only.</li>
<li>[L] {source_link(PACKAGE + '/vessel_forces.py', 'Steady load and towing implementation')}, with
{source_link('src/digitalmodel/hydrodynamics/ocimf_loading.py', 'upstream approximate coefficient implementation')}: no coefficient values are prescribed by this analysis.</li>
<li>[X] {source_link(PACKAGE + '/examples.py', 'Existing deterministic example builder')}: existing result engine and byte-digest rendering pattern;
{source_link('docs/domains/hydrodynamics/river_currents/river-current-analysis.results.json', 'existing example results')} and
{source_link('docs/domains/hydrodynamics/river_currents/river-current-analysis.manifest.json', 'existing example manifest')}.</li>
<li>Source inventory and layout evidence are bound in manifest.json. The canonical palette and existing flat-plate report layout are inlined; all figures are generated by this builder.</li></ul>
<details id="changes-panel"><summary>Change panel — revision A</summary><p>{escape(CHANGES[-1]['change'])}</p><a href="changes.json">Machine-readable revision history</a></details>
<script id="calculation" type="application/json">{payload}</script>
</main></body></html>
'''


def readme():
    """Return portable rebuild instructions and the bounded evidence contract."""
    text = """# River currents module analysis

The [module analysis](index.html) documents the public function review and a
stipulated square-root profile with rectangular-channel corrections. Independent
hand integrals supply numerical expectations. Physical calibration, fitted
extremes, load coefficients and operating limits remain `NOT_EVALUATED`.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
HISTORY_ROW

From the repository root, with the package import dependencies available, set
`REPORT_SCRATCH` to a fresh external directory and run:

```sh
PYTHONPATH=src python scripts/reports/build_river_currents_module_report.py \\
  --scratch "$REPORT_SCRATCH" --output-dir docs/reports/modules/river_currents
PYTHONPATH=src python -m pytest tests/reporting/test_river_currents_module_report.py
```

The CLI requires both locations. All calculation intermediates stay in external
scratch. The output directory contains only the small deliverables. The builder
uses normal public imports and does not rerun the linked towing/extreme examples.

[Results](results.json) retain full precision; visible answers use one decimal
place. [Changes](changes.json) match the HTML revision history. The
[manifest](manifest.json) binds the other deliverables, generator, calculation
sources and test evidence by SHA-256 of exact UTF-8 bytes with LF endings.
The manifest does not hash itself. Determinism establishes freshness; the
independent hand-calculation assertions establish the synthetic arithmetic.

The reviewed package revision is `09089a121a16069c4b657d1a320a630a23ab1dd5`.
No package source correction is included. Further calibration and acceptance
work remains open under the limits and way forward in the analysis.
"""
    row = CHANGES[-1]
    return text.replace("HISTORY_ROW", f"| {row['revision']} | {row['date']} | {row['change']} |")


def build(scratch, output_dir):
    """Stage deterministic deliverables externally before writing final bytes."""
    scratch, output_dir = Path(scratch).resolve(), Path(output_dir).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    if scratch == output_dir or scratch in output_dir.parents or output_dir in scratch.parents:
        raise ValueError("Scratch and output directories must be separate")
    data = calculate()
    outputs = {"index.html": render(data), "results.json": json_text(data),
               "changes.json": json_text(CHANGES), "README.md": readme()}
    sources = [f"{PACKAGE}/{name}" for name in (
        "__init__.py", "profiles.py", "bank_effects.py", "extremes.py", "vessel_forces.py", "examples.py")]
    sources += ["src/digitalmodel/hydrodynamics/models.py",
                "src/digitalmodel/hydrodynamics/ocimf_loading.py",
                "examples/hydrodynamics/river_currents/basis.json",
                "tests/reporting/test_river_currents_module_report.py", LAYOUT, PALETTE]
    sources += [f"tests/hydrodynamics/river_currents/{name}" for name in (
        "test_profiles.py", "test_bank_effects.py", "test_extensions.py")]
    manifest = {
        "reviewed_revision": REVIEWED_REVISION,
        "digest_convention": "SHA-256 of exact UTF-8 bytes with LF endings; manifest excludes itself.",
        "generator": Path(__file__).relative_to(ROOT).as_posix(),
        "generator_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "sources": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in sources},
        "artifacts": {name: hashlib.sha256(text.encode("utf-8")).hexdigest() for name, text in outputs.items()},
    }
    outputs["manifest.json"] = json_text(manifest)
    scratch.mkdir(parents=True, exist_ok=True)
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, text in outputs.items():
        staged = scratch / name
        with staged.open("w", encoding="utf-8", newline="\n") as stream:
            stream.write(text)
        (output_dir / name).write_bytes(staged.read_bytes())
    return manifest


def main():
    """Require explicit scratch and final output locations."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
