# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build bounded HIPPS arithmetic evidence with external calculation scratch.

Both --scratch and --output-dir are explicit. No solver or external service is
used. All physical data are synthetic assumptions, never recommended limits.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import inspect
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = "src/digitalmodel/subsea/hipps"
REVIEWED_REVISION = "09089a121a16069c4b657d1a320a630a23ab1dd5"
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "Present synthetic reliability integrals and valve closure kinematics with independent arithmetic and byte-bound evidence to make the module scope and qualification gaps reviewable.",
}]
# These inputs are stipulated synthetic assumptions, not reliability data,
# measured equipment properties, recommended design inputs or safety limits.
INPUTS = {
    "lambda_per_hour": 1e-5, "proof_interval_h": 1000.0, "beta": 0.0,
    "stroke_m": 0.12, "loaded_speed_m_s": 0.02,
    "halfway_s": 3.0, "seating_s": 6.0, "rangeability": 100.0,
}
INPUT_BASIS = {
    "lambda_per_hour": ("1/h", "Synthetic constant dangerous-undetected rate in p(t)=lambda*t; assumed."),
    "proof_interval_h": ("h", "Synthetic averaging interval; perfect reset and no repair within the interval are assumed."),
    "beta": ("dimensionless", "Independent channels are stipulated; zero common-cause contribution is assumed only for this example."),
    "stroke_m": ("m", "Synthetic uniform travel distance; assumed."),
    "loaded_speed_m_s": ("m/s", "Synthetic constant loaded travel speed; assumed, not measured."),
    "halfway_s": ("s", "Synthetic sample instant at half of the stipulated seating time."),
    "seating_s": ("s", "Synthetic seating instant, independently obtained by distance divided by speed."),
    "rangeability": ("dimensionless", "Synthetic equal-percentage conductance ratio; assumed, not an installed-flow characteristic."),
}
# Reviewed function contracts, expressed independently of implementation
# docstrings to avoid reproducing band tables or unqualified standards claims.
CONTRACTS = {
    "pfd_avg_1oo1": (
        "lambda_du: dangerous-undetected failures/h; ti: h; positive comparison checks only.",
        "Dimensionless linear-model time average; lambda*ti/2.",
        "Synthetic approximation demonstrated; rate/time finiteness and applicability validation incomplete.",
    ),
    "pfd_avg_1oo2": (
        "lambda_du: failures/h; ti: h; beta: dimensionless common-cause fraction, defaults to zero.",
        "Dimensionless ((1-beta)*lambda)^2*ti^2/3 + beta*lambda*ti/2.",
        "Independent case demonstrated; beta model, repair, proof coverage and dependence unqualified.",
    ),
    "risk_reduction_factor": (
        "pfd_avg: dimensionless, positive and at most unity.",
        "Dimensionless reciprocal of the supplied average.",
        "Arithmetic reciprocal only; not demonstrated system risk reduction. Tiny inputs can overflow.",
    ),
    "classify_sil": (
        "pfd_avg: dimensionless; optional target_sil: enum or integer, defaults to None.",
        "SILResult: encoded band, reciprocal and optional band comparison.",
        "Inventory only. Encoded band is not an achieved system SIL; endpoint policy, below-floor cap and target coercion need review.",
    ),
    "overpressure_envelope": (
        "p_initial/p_design: one consistent pressure unit; dpdt: pressure/time; t_close/t_detect/t_logic: matching time. Detection and logic delays default to zero.",
        "OverpressureResult: summed response time, p_initial+dpdt*time, design-minus-closure margin, pressure ratio and encoded comparison.",
        "Inventory only. Caller-qualified rise bound and nonzero physical delay basis absent; not a coupled transient. Finite-domain checks incomplete.",
    ),
    "liquid_capacity": (
        "coefficient: Cv/Kv; absolute upstream/downstream/vapor pressures; SG; N1 unit_constant; Fp piping_factor; FLP recovery_factor; FF critical_pressure_ratio. All factors are required caller inputs.",
        "ValveCapacity: volumetric rate in N1 units, effective pressure drop and choking flag; min(actual drop, choking drop) is used.",
        "Inventory only. Consistent units, qualified properties, factors, cavitation and catalogue iteration not evaluated. Final rate representability incomplete.",
    ),
    "required_liquid_coefficient": (
        "Nonnegative rate in N1 units plus liquid_capacity inputs except coefficient.",
        "Required Cv/Kv at fixed factors by dividing demand by unit-coefficient capacity.",
        "Inventory only. Positive demand at zero capacity raises; no catalogue-dependent factor iteration.",
    ),
    "gas_mass_capacity": (
        "coefficient: Cv/Kv; absolute pressures; inlet density; N6 unit_constant; Fp; caller-qualified choked_ratio.",
        "ValveCapacity: mass rate in N6 units, effective pressure drop and choking flag; expansion and limiting pressure ratio included.",
        "Inventory only. Mass rate is not liquid or standard volume rate. Gas properties, factor qualification and final finite outputs not evaluated.",
    ),
    "stroke_time": (
        "stroke_length: m or rad; actuator_speed: matching length or angle per second; finite positive inputs.",
        "Seconds, stroke_length/actuator_speed; excludes detection and logic delays.",
        "Synthetic kinematics demonstrated; loaded speed qualification and finite-output checks incomplete.",
    ),
    "closure_fraction": (
        "elapsed and closure_time: s, nonnegative; characteristic: linear or equal_percentage; rangeability required for equal_percentage, otherwise None.",
        "Normalized conductance: linear remaining travel or rangeability^(-elapsed/closure_time); exact zero at seating. Zero closure time is a step.",
        "Synthetic law demonstrated; equal-percentage seating discontinuity retained. Conductance is not installed flow, travel or measured dynamics.",
    ),
    "wave_speed": (
        "density: kg/m³; bulk_modulus/elastic_modulus: Pa; diameter/wall_thickness: m; nonnegative restraint_factor; all supplied.",
        "m/s from combined fluid and thin-wall compliance.",
        "Inventory only. Support model, constant properties, thin-wall applicability and intermediate representability need qualification.",
    ),
    "joukowsky_rise": (
        "density: kg/m³; acoustic_speed: m/s; velocity_change: nonnegative m/s reduction.",
        "Pa from density*acoustic_speed*velocity_change.",
        "Inventory only. Ideal frictionless single-phase liquid relation; no multiphase or operational bound qualification.",
    ),
    "simulate_surge": (
        "length/diameter: m; density: kg/m³; acoustic_speed: m/s; initial_flow: m³/s; reservoir/downstream absolute pressure: Pa; closure_time/duration: s; cells: integer. Default linear characteristic, rangeability None.",
        "SurgeResult: valve time/pressure/flow histories, all-node peak, sampled positive valve rise rate and grid time step. Final time rounds up to grid.",
        "Inventory only. Single reach, fixed reservoir, frictionless single-phase constant properties; no cavitation or column separation. A sampled rise rate is not a qualified response-time bound.",
    ),
    "SIL": ("No calculation inputs.", "Encoded classification enum.", "No licensed band table reproduced; architectural integrity not evaluated."),
    "SILResult": ("pfd_avg, sil, rrf; optional target_sil and meets_target default to None.", "Frozen classification record; dimensionless quantities.", "Record construction does not validate physical or functional-safety basis."),
    "OverpressureResult": ("t_response in caller time; p_at_close/margin in caller pressure; utilisation dimensionless; passed boolean.", "Frozen envelope record.", "Record construction does not validate the bounding-rise assumption."),
    "ValveCapacity": ("rate in caller N1/N6 units; effective_drop in caller pressure; choked boolean.", "Frozen sizing record.", "Record construction does not validate units; distinguish volume and mass rates."),
    "SurgeResult": ("time_s, valve_pressure_pa, valve_flow_m3_s, peak_pressure_pa, dpdt_pa_s, time_step_s.", "Frozen SI history record; peak covers all nodes, slope samples the valve only.", "Record construction does not qualify transients or the sampled slope."),
}
SOURCES = [
    f"{PACKAGE}/{name}" for name in
    ("__init__.py", "sil_analysis.py", "overpressure.py", "valve_sizing.py", "pressure_surge.py", "docs/agent-pack.md")
] + [
    "tests/reporting/test_hipps_module_report.py",
    "docs/api/_assets/brand.css",
    "docs/api/cfd/turbulent-flat-plate-verification.html",
]
# Exact reviewed package bytes are recorded below; style/test changes are bound
# separately by the delivered manifest. A package change requires fresh review.
REVIEWED_PACKAGE_DIGESTS = {'src/digitalmodel/subsea/hipps/__init__.py': 'c7ac4c8f037c70ff3763ec9dd1ee0fdf071c909cf112ed2991b19c7b6784b5b9', 'src/digitalmodel/subsea/hipps/sil_analysis.py': 'a9f5317206f61b3eea8fc162d893c4c1666e8dbd87a9c9857ba81015b6dfa02c', 'src/digitalmodel/subsea/hipps/overpressure.py': 'd692981dd0982e6ce02b4199be567a523f1f6450d3fa8dc3e84bbc887de39c72', 'src/digitalmodel/subsea/hipps/valve_sizing.py': '9ae6b984ae9b02c170022e46f2b2cb11eb74b07a6af39232b813fca82820ae84', 'src/digitalmodel/subsea/hipps/pressure_surge.py': 'e5a9c708ff5dc5dff92ff5108c2ca65718b9ed6edcca38b959c0a682ff1cde6a', 'src/digitalmodel/subsea/hipps/docs/agent-pack.md': 'e36a028d6afe74fe073f5e9ab9af906fa79307d1c47f7e0868599a8ac6ae159f'}


def digest(path):
    """Hash exact bytes, without platform metadata."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def json_text(data):
    """Serialize deterministic evidence with finite numbers only."""
    return json.dumps(data, indent=2, sort_keys=True, allow_nan=False) + "\n"


def inventory(api):
    """Collect every public export, its actual signature and reviewed contract."""
    rows = []
    if set(api.__all__) != set(CONTRACTS):
        raise ValueError("Public inventory changed; review contracts before rebuilding")
    for name in api.__all__:
        obj = getattr(api, name)
        inputs, outputs, limits = CONTRACTS[name]
        path = Path(inspect.getsourcefile(obj)).resolve().relative_to(ROOT).as_posix()
        line = inspect.getsourcelines(obj)[1]
        signature = str(inspect.signature(obj)) if inspect.isfunction(obj) else "record or enum"
        rows.append({"name": name, "signature": signature, "inputs": inputs,
                     "outputs": outputs, "limits": limits, "source": path, "line": line})
    return rows


def calculate():
    """Call normal package APIs for all reported numerical results and samples."""
    for name, expected in REVIEWED_PACKAGE_DIGESTS.items():
        if digest(ROOT / name) != expected:
            raise ValueError("Reviewed package bytes changed; review source revision before rebuild")
    from digitalmodel.subsea import hipps as api

    i = dict(INPUTS)
    single = api.pfd_avg_1oo1(i["lambda_per_hour"], i["proof_interval_h"])
    independent = api.pfd_avg_1oo2(i["lambda_per_hour"], i["proof_interval_h"], beta=i["beta"])
    time = api.stroke_time(stroke_length=i["stroke_m"], actuator_speed=i["loaded_speed_m_s"])
    results = {}

    def record(key, label, value, unit, method, basis, function):
        results[key] = {"label": label, "value": value, "unit": unit,
                        "method": method, "basis": basis, "api": function}

    record("single_average", "Single-channel average", single, "dimensionless",
           "Integral of lambda*t over the interval, divided by interval length.",
           "[H] Linear-model integral: lambda*T/2 = 1/200.", "pfd_avg_1oo1")
    record("independent_average", "Independent dual-channel average", independent, "dimensionless",
           "Integral of (lambda*t)^2 over the interval, divided by interval length; beta is zero.",
           "[H] Independent linear-model integral: lambda²*T²/3 = 1/30000.", "pfd_avg_1oo2")
    record("reciprocal", "Reciprocal of single-channel average", api.risk_reduction_factor(single),
           "dimensionless", "Reciprocal of the single-channel time average.",
           "[H] Reciprocal of 1/200; no system-level risk reduction established.", "risk_reduction_factor")
    record("closure_time", "Uniform-speed closure time", time, "s", "Travel divided by loaded speed.",
           "[H] Synthetic 0.12 m divided by 0.02 m/s.", "stroke_time")
    for key, law in (("linear", "linear"), ("equal_percentage", "equal_percentage")):
        kwargs = {"rangeability": i["rangeability"]} if key == "equal_percentage" else {}
        for suffix, elapsed in (("half", i["halfway_s"]), ("seated", i["seating_s"])):
            value = api.closure_fraction(elapsed, time, law, **kwargs)
            method = "Remaining fraction 1-t/T." if key == "linear" else "Remaining fraction R^(-t/T) before seating."
            record(f"{key}_{suffix}", f"{law.replace('_', ' ').capitalize()} {suffix}", value,
                   "dimensionless", method + " At or after seating the API returns exact zero.",
                   "[H] Stipulated normalized conductance law; seating is a separate boundary rule.", "closure_fraction")
    samples = [{
        "elapsed_s": t,
        "linear": api.closure_fraction(t, time, "linear"),
        "equal_percentage": api.closure_fraction(t, time, "equal_percentage", rangeability=i["rangeability"]),
    } for t in (0.0, i["halfway_s"], i["seating_s"])]
    return {"reviewed_revision": REVIEWED_REVISION, "inputs": i,
            "input_basis": {key: {"unit": unit, "basis": basis} for key, (unit, basis) in INPUT_BASIS.items()},
            "results": results, "closure_samples": samples, "inventory": inventory(api)}


def result_row(row):
    """Render an answer from the API result, with units and derivation basis."""
    value = row["value"]
    display = f"{value:.1e}" if 0 < abs(value) < 0.01 else f"{value:.1f}"
    return "<tr>" + "".join(f"<td>{html.escape(str(item))}</td>" for item in
                           (row["label"], display, row["unit"], row["api"], row["basis"])) + "</tr>"


def closure_diagram(data):
    """Generate sample markers only; avoid implying measured dynamic curves."""
    marks = []
    for row in data["closure_samples"]:
        x = 90 + 90 * row["elapsed_s"]
        for law, offset in (("linear", -7), ("equal_percentage", 7)):
            value = row[law]
            y = 245 - 185 * value
            # Coincident endpoint markers are horizontally offset for visibility.
            marks.append(f'<circle cx="{x + offset:.1f}" cy="{y:.1f}" r="5" fill="var(--brand)"><title>{html.escape(law)}: {row["elapsed_s"]:.1f} s, fraction {value:.1f}</title></circle>')
            if row["elapsed_s"] == data["inputs"]["halfway_s"]:
                marks.append(f'<text x="{x + 14:.1f}" y="{y:.1f}">{html.escape(law)}: {value:.1f}</text>')
    return f'''<svg viewBox="0 0 740 310" role="img" aria-labelledby="closure-title closure-desc">
<title id="closure-title">Synthetic conductance samples</title>
<desc id="closure-desc">Start, halfway and seated values from the public closure API. Markers show samples only; there is no interpolated dynamic curve.</desc>
<path d="M90 50 V245 H645" fill="none" stroke="currentColor"/>
<text x="90" y="30">Remaining conductance fraction [H]</text>
<text x="50" y="65">1.0</text><text x="50" y="250">0.0</text>
<text x="90" y="275">0.0</text><text x="360" y="275">{data["inputs"]["halfway_s"]:.1f}</text>
<text x="630" y="275">{data["inputs"]["seating_s"]:.1f}</text><text x="250" y="300">Elapsed closure time (s) [H]</text>
{''.join(marks)}</svg>'''


def render(data):
    """Render the ten-section offline report from computed evidence."""
    e = html.escape
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8"), flags=re.S)
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    history = "".join("<tr>" + "".join(f"<td>{e(item[key])}</td>" for key in ("revision", "date", "change")) + "</tr>" for item in CHANGES)
    input_rows = "".join(f'<tr><td>{e(key)}</td><td>{value:g}</td><td>{e(data["input_basis"][key]["unit"])}</td><td>{e(data["input_basis"][key]["basis"])}</td></tr>' for key, value in data["inputs"].items())
    result_rows = "".join(result_row(row) for row in data["results"].values())
    contracts = "".join(f'<tr><td><code>{e(row["name"] + row["signature"])}</code><br>{e(row["source"])}:{row["line"]}</td><td>{e(row["inputs"])}</td><td>{e(row["outputs"])}</td><td>{e(row["limits"])}</td></tr>' for row in data["inventory"])
    provenance = "".join(f'<li><code>{e(path)}</code>; SHA-256: <code>{digest(ROOT / path)}</code></li>' for path in SOURCES)
    # Escape the script closing sentinel and HTML-sensitive characters in JSON.
    payload = json_text(data).replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")
    links = " ".join(f'<a href="#s{i}">{i}</a>' for i in range(1, 11))
    page = f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>HIPPS module analysis</title><style>{brand}\n{layout}
.wrap{{max-width:1100px;margin:auto;padding:28px;background:var(--panel)}}
td{{vertical-align:top;overflow-wrap:anywhere}} code{{overflow-wrap:anywhere}}
.scroll{{overflow-x:auto}} svg{{max-width:100%;height:auto}} nav a{{padding:6px}}
@media print{{details{{display:block}} nav{{display:none}}}}
</style></head><body class="hs-doc"><main class="wrap">
<header><p class="kicker">Subsea engineering · module analysis</p><h1>HIPPS module analysis</h1>
<p class="sub">Synthetic reliability and valve closure arithmetic; qualification remains open.</p></header>
<nav aria-label="Report sections">{links}</nav>
<h2 id="s1">1. Summary</h2>
<p>The public package evaluates reliability approximations, encoded classification, a linear overpressure envelope, caller-factor liquid/gas sizing and ideal liquid surge. This report demonstrates only synthetic reliability integrals and valve kinematics. The answers are generated through public methods and checked against independent hand calculations [H].</p>
<p>A returned band is not an achieved system SIL. Physical protection, architectural capability and functional-safety qualification are NOT_EVALUATED [P]. No equipment selection, compliance or operational release follows from these answers.</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{history}</tbody></table>
<h2 id="s2">2. Context and scope</h2>
<p>HIPPS means High Integrity Pressure Protection System. The intended package context is an instrumented closure function protecting a pressure boundary. This analysis inventories the available software, its units and known limits, without choosing a design basis or recommending reliability data. The full exported inventory is in the evidence section [P].</p>
<p>Reviewed source revision: <code>{REVIEWED_REVISION}</code>. Package digests are checked before calculation. Classification, overpressure, liquid/gas capacity and surge APIs are inventoried only; this report executes no sizing or coupled transient case. No licensed solver is imported or queried.</p>
<h2 id="s3">3. Input basis</h2>
<p>All rates, dimensions, times and rangeability below are synthetic assumptions [H], not recommended reliability data, equipment properties or safety limits. The dangerous-undetected failure rate is constant; the proof interval has a perfect reset. Independent channels, synchronized interval origin, no repairs, no diagnosed-mode contribution and no proof-test outage are stipulated solely to define the integral.</p>
<div class="scroll"><table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis [H]</th></tr></thead><tbody>{input_rows}</tbody></table></div>
<p>Hours belong to the reliability integral; seconds belong to closure. No cross-domain time conversion or physical transient coupling is performed. A numerical sample at seating uses the stipulated seating instant rather than inferring a measured closure profile.</p>
<h2 id="s4">4. Method</h2>
<div class="eq">[H] p(t)=lambda*t; average = (integral from zero to T of p(t) dt)/T = lambda*T/2.</div>
<div class="eq">[H] Independent dual-channel average = (integral from zero to T of p(t)² dt)/T = lambda²*T²/3, for beta=0.</div>
<p>The divisors follow from integrating a linear term and its square. These are low-demand linear approximations, not exact exponential reliability probabilities. The independent result is the average of the squared instantaneous approximation, not the square of its average. The API also contains a beta-factor expression; its common-cause basis is NOT_EVALUATED here.</p>
<div class="eq">[H] Closure time = stroke / loaded speed. Before seating: linear fraction = 1-t/Tc; equal-percentage fraction = R^(-t/Tc). At seating both return zero.</div>
<p>T is the proof interval in hours; Tc is closure time in seconds; R is dimensionless rangeability. Uniform speed is an imposed kinematic law. The equal-percentage seating discontinuity remains in the API: the limit just before seating is the reciprocal of rangeability, followed by exact zero. Conductance fraction is not measured installed flow.</p>
<h2 id="s5">5. Worked calculation</h2>
<p>[H] Substituting the input register gives the single-channel integral target 1/200 and the independent dual-channel target 1/30000. Taking the reciprocal of the single-channel target gives the reciprocal check. Stroke divided by speed defines Tc. Halfway travel gives linear target 1/2 and equal-percentage target 1/10. Seating is checked as exact zero separately. The API answers, units and derivation basis are in Results; no returned answer is typed into the report template.</p>
<figure>{closure_diagram(data)}<figcaption>Generated public-API samples [H]. Coincident endpoint markers are offset horizontally for visibility. No curve interpolation or installed dynamic response is implied.</figcaption></figure>
<h2 id="s6">6. Checks</h2>
<p>The dedicated tests compare normal public API outputs with the hand integrals and distance/speed relation [H, T]. Nonzero values and time use relative tolerance 1e-12 with no absolute allowance: this allows floating-point roundoff in short arithmetic chains and is not an engineering acceptance limit. Seating zero is tested by exact equality.</p>
<p>Separate tests compare delivered values with the independent targets, replay production outputs for freshness, build into two fresh external output directories and compare every byte with the committed delivery. Independent SHA-256 readback checks all manifest bindings. Changed signs and values fail numerical assertions even when the builder, report and newly calculated hashes agree. Tampering with a delivered result also fails its artifact digest [T]. Reproducibility is not physical validation.</p>
<p>Local links, required sections, input/result units, public export inventory and escaped embedded JSON are checked. Execution counts are recorded in the delivery description rather than embedded as a time-dependent assertion. All intermediate calculation files stay in explicit external scratch.</p>
<h2 id="s7">7. Results</h2>
<div class="scroll"><table><thead><tr><th>Quantity</th><th>API answer</th><th>Unit</th><th>Public API</th><th>Basis</th></tr></thead><tbody>{result_rows}</tbody></table></div>
<p>Displayed answers are rounded; <a href="results.json">results.json</a> retains full finite values. The comparisons use unrounded values. The reciprocal is arithmetic only; it is not demonstrated system risk reduction. No classification band table or design acceptance verdict is published.</p>
<h2 id="s8">8. Limits / NOT_EVALUATED</h2>
<ul><li>Functional safety: achieved system integrity, demand mode qualification, hazard/risk assessment, required integrity, lifecycle evidence and independent assessment are NOT_EVALUATED.</li>
<li>Architecture: sensor, logic and final-element allocation, voting dependencies, diagnostics, redundancy constraints and systematic capability are NOT_EVALUATED. A scalar average or enum cannot establish them.</li>
<li>Failure modes and common cause: failure-mode/effects/diagnostic analysis, field rate provenance, proof coverage, test duration, repair, bypass, useful life, beta selection and dependent-channel effects are NOT_EVALUATED.</li>
<li>Valve physics: loaded actuator performance, pressure-dependent speed, stiction, leakage, actual seating profile, installed conductance, catalogue factor iteration and fluid-property qualification are NOT_EVALUATED.</li>
<li>Transient coupling: no detection/logic delays or pressure-rise bound are qualified. Single-phase frictionless surge excludes gas, friction, multiphase flow, cavitation and column separation. Positive absolute pressure does not establish vapor-pressure margin. A sampled rise rate depends on the time grid and is not a qualified response-time bound.</li>
<li>Open implementation findings [P]: reliability and envelope finite-domain validation, approximation applicability, band endpoint/capping and target coercion policies, intermediate/output representability and stale envelope documentation need separate review. No method or acceptance policy is replaced by this report.</li>
<li>Standards: API 17O and IEC 61508 / IEC 61511 metadata, editions and clause titles have not been independently verified for this report; they are qualification work items, not cited normative evidence. No licensed tables or compliance conclusion are reproduced. Valve sizing source claims in implementation documentation are not requalified here.</li></ul>
<h2 id="s9">9. Way forward</h2>
<p>Before a design assessment, establish the hazard basis and required integrity with the responsible engineering disciplines. Supply qualified failure data, failure-mode analysis, proof-test and repair assumptions and common-cause evidence; review complete architecture and lifecycle obligations separately from arithmetic.</p>
<p>Qualify loaded actuator and installed valve response, then establish pressure transients with an applicable physical model, boundary data and convergence evidence. Do not feed a sampled grid slope into the envelope as a qualified worst-case bound. Resolve the documented domain and representability findings with failing regression tests before changing calculation code. Verify standards edition and clause metadata before any standards-based assessment.</p>
<p>This report closes only the presentation and arithmetic-evidence scope. System qualification, physical acceptance and operational use remain open. Lesson: independent hand targets must remain separate from production replay and regenerated file hashes.</p>
<h2 id="s10">10. References and evidence</h2>
<p>[H] Hand derivation in this report and <code>tests/reporting/test_hipps_module_report.py::test_reliability_integrals_and_closure_kinematics_match_hand_calculation</code>. Inputs are the synthetic stipulations in Input basis. Integration coefficients, reciprocal, uniform-speed quotient and normalized closure laws follow from the stated mathematics; no licensed equation or tabulated value is needed.</p>
<p>[P] Reviewed repository package and its knowledge pack at the revision above. Code behavior is the software evidence, not an independent engineering authority. [T] Dedicated report tests, listed below, supply independent targets, negative controls and delivery checks. Palette and layout are inlined from the existing house style; no domain claims are inherited from the layout exemplar.</p>
<details><summary>Complete public function and result-type inventory [P]</summary><div class="scroll"><table><thead><tr><th>Symbol, signature and source line</th><th>Inputs and units</th><th>Outputs and method</th><th>Status and limits</th></tr></thead><tbody>{contracts}</tbody></table></div></details>
<details><summary>Source byte evidence</summary><ul>{provenance}</ul></details>
<p>The <a href="manifest.json">manifest</a> binds exact delivered bytes and current source digests; it excludes its own digest to avoid self-reference. The reviewed revision identifies the package basis, while the generator and report tests are bound by their own digests. <a href="changes.json">Change history</a> matches the revision table. <a href="README.md">Rebuild instructions</a> describe explicit output and scratch paths. The page works offline with inline styles and generated SVG; the embedded JSON is inert evidence.</p>
</main><script id="calculation" type="application/json">{payload}</script></body></html>
'''
    return '\n'.join(line.rstrip() for line in page.splitlines()) + '\n'


README = """# HIPPS module analysis

Open [the self-contained report](index.html) for synthetic reliability integrals,
valve closure kinematics, the public API inventory and NOT_EVALUATED limits.
The calculation does not establish achieved system integrity or design acceptance.

With source imports enabled, rebuild from the repository root:

```sh
PYTHONPATH=src python scripts/reports/build_hipps_module_report.py \\
  --scratch ../hipps-calculation --output-dir docs/reports/modules/hipps
```

Scratch must be outside the checkout. Both paths are required. The calculation
uses the standard library and normal package imports; tests additionally need
pytest. No solver is required. All intermediate evidence stays in scratch.

[Results](results.json) retain unrounded API values, input units and basis,
contracts and sampled conductance values. [Change history](changes.json) matches
the report. The [manifest](manifest.json) records SHA-256 over exact artifact
bytes, generator bytes and source bytes; it excludes itself. The reviewed
package revision is recorded separately from the generator/test digests.

Run `tests/reporting/test_hipps_module_report.py` with source imports and an
external pytest base directory. Hand-calculation targets and mutation tests
check arithmetic independently; replay and byte rebuilds check freshness only.
Standards, architecture, failure-mode and transient qualification remain open.
"""


def build(scratch, output_dir):
    """Write deterministic delivery and external intermediate calculation data."""
    scratch = Path(scratch).resolve()
    output_dir = Path(output_dir).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    data = calculate()
    scratch.mkdir(parents=True, exist_ok=True)
    (scratch / "calculation.json").write_text(json_text(data), encoding="utf-8", newline="\n")
    outputs = {"index.html": render(data), "results.json": json_text(data),
               "changes.json": json_text(CHANGES), "README.md": README}
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, content in outputs.items():
        (output_dir / name).write_text(content, encoding="utf-8", newline="\n")
    manifest = {
        "schema_version": 1,
        "digest_convention": "SHA-256 over exact UTF-8 artifact bytes with LF line endings; manifest excludes itself.",
        "reviewed_revision": REVIEWED_REVISION,
        "generator": Path(__file__).relative_to(ROOT).as_posix(),
        "generator_sha256": digest(Path(__file__)),
        "sources": {path: digest(ROOT / path) for path in SOURCES},
        "artifacts": {name: digest(output_dir / name) for name in outputs},
    }
    (output_dir / "manifest.json").write_text(json_text(manifest), encoding="utf-8", newline="\n")


def main():
    """Require explicit scratch and delivery locations on the command line."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
