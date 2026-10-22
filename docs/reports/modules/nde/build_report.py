# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the standalone NDE analysis from its public synthetic pulse example.

From the repository root::

    PYTHONPATH=src uv run python docs/reports/modules/nde/build_report.py

Only the pulse timing function is called. No dataset, solver or network access
is required. Inputs are read from the existing public example without executing
test code. The report is illustrative, not a qualified inspection calculation.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import math
import re
import sys
import tomllib
from html import escape
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parents[4]
REPORT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT / "src"))

from digitalmodel.nde.pulse_timing import pulse_peak_time  # noqa: E402


LOCAL_SOURCES = {
    "A": "src/digitalmodel/nde/pulse_timing.py",
    "B": "tests/nde/test_pulse_timing.py",
    "C": "src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py",
    "D": "tests/nde/test_nde.py",
    "E": "tests/test_acoustic_scattering_loader.py",
    "F": "docs/examples/acoustic_nde_validation.py",
}
EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
SECTIONS = [
    "Executive Summary", "Introduction", "Summary of Results", "Data and Basis",
    "Methodology", "Worked Example", "Validation", "Findings and Recommendations",
    "Appendices and References", "Not Covered",
]
CHANGES = {
    "document": "nde-module-analysis",
    "revision": "A",
    "date": "2026-10-22",
    "changes": [{
        "section": "s1",
        "summary": "Initial analysis of sampled acoustic pulse timing, with a "
        "computed synthetic example and the evidence needed for inspection use.",
        "reason": "Distinguish the demonstrated timing calculation from "
        "unestablished inspection accuracy and dataset compatibility.",
    }],
}


def fixture_inputs(path: Path) -> dict:
    """Read the exact supported Gaussian fixture; reject a changed expression."""
    tree = ast.parse(path.read_text(encoding="utf-8"))
    try:
        function = next(node for node in tree.body if isinstance(node, ast.FunctionDef)
                        and node.name == "test_synthetic_pulse_travel_time")
        assignments = {node.targets[0].id: node.value for node in function.body
                       if isinstance(node, ast.Assign)}
        times = ast.literal_eval(assignments["times"])
        polarities = ast.literal_eval(function.decorator_list[0].args[1])
        expression = assignments["pressures"].elt
        exponential = expression.right
        power = exponential.args[0].operand
        distance = ast.literal_eval(power.left.left)
        speed = ast.literal_eval(power.left.right.left)
        exponent = ast.literal_eval(power.right)
        expected = ast.parse(
            f"polarity * math.exp(-(({distance!r} - {speed!r} * t) ** {exponent!r}))",
            mode="eval",
        ).body
        if ast.dump(expression) != ast.dump(expected):
            raise ValueError("Unexpected pressure expression")
        comprehension = assignments["pressures"].generators
        if (len(comprehension) != 1 or comprehension[0].target.id != "t"
                or comprehension[0].iter.id != "times" or comprehension[0].ifs):
            raise ValueError("Unexpected sample comprehension")
        if not all(math.isfinite(value) for value in [*times, distance, speed]):
            raise ValueError("Nonfinite example input")
        if distance <= 0 or speed <= 0 or set(polarities) != {-1.0, 1.0}:
            raise ValueError("Unsupported example inputs")
    except (AttributeError, IndexError, KeyError, StopIteration, TypeError,
            ValueError) as error:
        raise ValueError("The public example changed; its basis needs inspection") from error
    return {"distance_m": distance, "speed_m_s": speed, "time_s": times,
            "exponent_power": exponent, "polarities": polarities}


def compute_example(source: Path | None = None) -> dict:
    """Call the module for both documented polarities; retain unrounded values."""
    source = source or ROOT / LOCAL_SOURCES["B"]
    inputs = fixture_inputs(source)
    reference = inputs["distance_m"] / inputs["speed_m_s"]
    cases = []
    for polarity in inputs.pop("polarities"):
        pressures = [polarity * math.exp(-((inputs["distance_m"]
                     - inputs["speed_m_s"] * time) ** inputs["exponent_power"]))
                     for time in inputs["time_s"]]
        peak = pulse_peak_time(inputs["time_s"], pressures)
        cases.append({"name": "positive" if polarity > 0 else "negative",
                      "polarity": polarity, "pressure": pressures,
                      "peak_time_s": peak, "reference_peak_time_s": reference,
                      "difference_percent": 100 * (peak - reference) / reference})
    return {"inputs": inputs, "cases": cases,
            "source_digest": hashlib.sha256(source.read_bytes()).hexdigest(),
            "engineering_limit": {"state": "NOT_EVALUATED",
                                  "reason": "No inspection acceptance limit is supplied."}}


def format_number(value: float) -> str:
    """Keep nonzero small values visible; use one decimal for exact integers."""
    if isinstance(value, int):
        return str(value)
    text = f"{value:.1f}" if float(value).is_integer() else f"{value:.3g}"
    return text.replace("-", "−")


def example_metrics(data: dict) -> dict:
    inputs = data["inputs"]
    metrics = {"distance": inputs["distance_m"], "speed": inputs["speed_m_s"],
               "start": inputs["time_s"][0], "end": inputs["time_s"][-1],
               "samples": len(inputs["time_s"]), "power": inputs["exponent_power"]}
    for case in data["cases"]:
        for field in ("peak_time_s", "reference_peak_time_s", "difference_percent"):
            metrics[f'{case["name"]}.{field}'] = case[field]
    return metrics


def metric(key: str, values: dict) -> str:
    return f'<span data-metric="{key}">{format_number(values[key])}</span>'


def house_style() -> str:
    """Inline the canonical brand stylesheet and the existing report layout."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    exemplar = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    layout = re.search(r"<style>.*?</style>", exemplar, re.S)[0]
    return f"<style>{brand}</style>\n{layout}"


def pulse_chart(data: dict) -> str:
    """Plot real input samples with keyboard and pointer inspection."""
    times = data["inputs"]["time_s"]
    x = lambda time: 70 + 650 * (time - times[0]) / (times[-1] - times[0])
    groups = []
    for case in data["cases"]:
        points = []
        coordinates = []
        for index, (time, pressure) in enumerate(zip(times, case["pressure"])):
            cx, cy = x(time), 160 - 115 * pressure
            coordinates.append(f"{cx:.3f},{cy:.3f}")
            tip = (f'Case: {case["name"]} pulse; component: receiver; '
                   f'time: {format_number(time)} s; demand: '
                   f'{format_number(pressure)} relative units; limit: not specified; '
                   f'analytical target: {format_number(case["reference_peak_time_s"])} s')
            points.append(
                f'<circle data-case="{case["name"]}" data-index="{index}" '
                f'data-time="{time}" data-pressure="{pressure}" '
                f'cx="{cx:.3f}" cy="{cy:.3f}" r="6" tabindex="0" '
                f'aria-label="{escape(tip)}"><title>{escape(tip)}</title></circle>')
        hidden = ' hidden=""' if case["name"] == "negative" else ""
        groups.append(f'<g id="trace-{case["name"]}"{hidden}>'
                      f'<polyline points="{" ".join(coordinates)}" fill="none" '
                      f'stroke="currentColor"/>{"".join(points)}</g>')
    ticks = "".join(f'<text x="{x(time):.3f}" y="307" text-anchor="middle">'
                    f'{format_number(time)}</text>' for time in times)
    target = x(data["cases"][0]["reference_peak_time_s"])
    levels = (min(min(case["pressure"]) for case in data["cases"]), 0,
              max(max(case["pressure"]) for case in data["cases"]))
    pressure_ticks = "".join(
        f'<text x="62" y="{165 - 115 * pressure:.3f}" text-anchor="end">'
        f'{format_number(pressure)}</text>' for pressure in levels)
    return f'''<figure id="pulse-figure"><label for="polarity">Pulse polarity</label>
<select id="polarity"><option value="positive">Positive pulse</option>
<option value="negative">Negative pulse</option></select>
<svg viewBox="0 0 800 350" role="group" aria-label="Receiver pressure against time">
<path d="M70 30V285H735 M70 160H735" fill="none" stroke="currentColor"/>
<path d="M{target:.3f} 30V285" stroke="var(--caution)" stroke-dasharray="6 4"/>
<text x="400" y="342" text-anchor="middle">Receiver time (s)</text>
<text transform="translate(18 160) rotate(-90)" text-anchor="middle">Pressure (relative units)</text>
<text x="{target + 8:.3f}" y="28">Analytical peak target</text>
{pressure_ticks}{ticks}{"".join(groups)}</svg>
<output id="pulse-detail" aria-live="polite">Point to a sample or use the tab key for its values.</output>
<figcaption>Receiver pressure samples and the analytical peak-time target. The line joins samples;
it does not represent interpolation by the timing function. No inspection acceptance limit is supplied.
<a href="#ref-B">[B]</a></figcaption></figure>'''


def source_records() -> str:
    version = tomllib.loads((ROOT / "pyproject.toml").read_text())["project"]["version"]
    records = []
    for label, path in LOCAL_SOURCES.items():
        digest = hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
        records.append(f'<li id="ref-{label}">[{label}] Digitalmodel, package edition '
                       f'{escape(version)}: <a href="../../../../{path}">{path}</a>. '
                       f'Source digest: <code>{digest}</code>.</li>')
    return "\n".join(records)


def render_report() -> str:
    data = compute_example()
    values = example_metrics(data)
    m = lambda key: metric(key, values)
    context = {
        "style": house_style(), "chart": pulse_chart(data), "records": source_records(),
        "contents": "".join(f'<li><a href="#s{i}">{i}. {title}</a></li>'
                            for i, title in enumerate(SECTIONS, 1)),
        "section_options": "".join(f'<option value="s{i}">{title}</option>'
                                   for i, title in enumerate(SECTIONS, 1)),
        "example": json.dumps(data, ensure_ascii=False, allow_nan=False),
        "changes": json.dumps(CHANGES, ensure_ascii=False, allow_nan=False),
        "script": UI_SCRIPT,
    }
    context.update({key: m(key) for key in values})
    for case in data["cases"]:
        name = case["name"]
        context[name] = SimpleNamespace(**{
            field: m(f"{name}.{field}") for field in (
                "peak_time_s", "reference_peak_time_s", "difference_percent")})
    # Substitution fills explicitly named values only; prose is never rewritten.
    return REPORT.format_map(context)


def build_report(destination: Path | None = None) -> None:
    destination = (destination or REPORT_DIR).resolve()
    document = render_report()
    changes = json.dumps(CHANGES, ensure_ascii=False, indent=2, allow_nan=False) + "\n"
    destination.mkdir(parents=True, exist_ok=True)
    (destination / "nde-module-analysis.html").write_text(document, encoding="utf-8")
    (destination / "changes.json").write_text(changes, encoding="utf-8")


REPORT = '''<!doctype html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="description" content="Sampled acoustic pulse timing, its analytical example, and the evidence required for inspection use.">
<title>Nondestructive evaluation module analysis</title>
{style}
<style>
.hs-doc svg{{display:block;width:100%;height:auto}}
.hs-doc svg [hidden]{{display:none}}
.hs-doc circle{{fill:var(--brand);stroke:var(--panel);stroke-width:2;cursor:crosshair}}
.hs-doc circle:focus,.hs-doc circle:hover{{fill:var(--caution);stroke:var(--ink)}}
.hs-doc textarea{{display:block;width:100%;min-height:6em}}
.hs-doc output{{display:block;min-height:3em;text-align:left}}
.hs-doc pre,.hs-doc code{{white-space:pre-wrap;overflow-wrap:anywhere}}
.hs-doc caption{{caption-side:bottom;text-align:left;color:var(--mut)}}
.hs-doc .review-controls{{margin-top:2em;padding:1em;border:1px solid var(--line)}}
@media print{{.review-controls,button,select{{display:none}}}}
</style></head><body class="hs-doc"><div class="wrap">
<header><p class="kicker">Acoustic inspection</p>
<h1>Nondestructive evaluation module analysis</h1>
<p>Draft for client review</p><p class="sub">22 October 2026 · Revision A</p>
<p class="source">Prepared by: analysis team. Checked by: pending. Approved by: pending.</p>
</header>
<nav aria-label="Contents"><p>Contents</p><ol>{contents}</ol></nav>
<main>
<section id="s1"><h2>1. Executive Summary</h2>
<p>The nondestructive evaluation module is a set of acoustic data utilities and a sampled pulse-timing
function. Analysis is performed to determine which timing results are supported by the available
evidence, and the key results are summarized below.</p>
<ul><li>Use the timing function to identify the unique largest absolute pressure sample in an isolated pulse.
<ul><li>The synthetic example returns {positive.peak_time_s} s for either polarity, matching the
constant-speed target of {positive.reference_peak_time_s} s with a {positive.difference_percent} %
difference relative to that target. This example establishes the sampled peak convention only.
<a href="#s6">Worked example</a></li></ul></li>
<li>Supply an inspection basis before interpreting the time as a material or defect measurement.
<ul><li>Measured accuracy and inspection acceptance remain NOT_EVALUATED because calibration records,
material properties and an acceptance criterion are absent from the example.
<a href="#s8">Findings and recommendations</a></li></ul></li></ul>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>Change and cause</th></tr></thead>
<tbody><tr><td>A</td><td>22 October 2026</td><td>Initial analysis to distinguish demonstrated pulse timing
from the evidence required for inspection use.</td></tr></tbody>
<caption>Revision history for this analysis.</caption></table></section>

<section id="s2"><h2>2. Introduction</h2>
<p>The acoustic timing capability and its data-loading interface are described for an inspection
engineer selecting tools for a calculation. The available analytical evidence is examined before
any interpretation of the result as a physical measurement.</p>
<p>Nondestructive evaluation, abbreviated NDE, examines a component without intentional damage.
The present calculation accepts a receiver trace; it does not simulate a transducer or classify a
defect. The acoustic data loader is described separately because its tests use substitute datasets.
<a href="#ref-A">[A]</a> <a href="#ref-C">[C]</a> <a href="#ref-D">[D]</a></p>
</section>

<section id="s3"><h2>3. Summary of Results</h2>
<p>The sampled receiver pressure is displayed against time, and its peak is compared with a
constant-speed analytical target. Positive and negative pulses are evaluated using the same
timestamps and the same magnitude convention.</p>
<p>The returned time is {positive.peak_time_s} s for either polarity. The target is
{positive.reference_peak_time_s} s, and the difference is {positive.difference_percent} % of the target.
This agreement is specific to a pulse whose peak is sampled exactly. <a href="#ref-B">[B]</a></p>
{chart}
</section>

<section id="s4"><h2>4. Data and Basis</h2>
<p>The public synthetic example is used as the calculation basis. Its parameters are numerical
test inputs, and no measured material or inspection acceptance data are assigned.</p>
<h3>4.1 Inputs and outputs</h3>
<p>The timing function is supplied with finite timestamps in seconds and signed, baseline-corrected
pressure samples in consistent units. The sequences are required to be nonempty and equal in
length; timestamps are required to increase strictly. The pressure requires a nonzero, unique
absolute maximum. A timestamp in seconds is returned. Invalid or ambiguous input raises a value
error. <a href="#ref-A">[A]</a></p>
<p>The loader accepts a dataset variant and a requested sample count. Its returned mapping contains
the collected frames, variant, dataset name, actual count and attribution. The frames are passed
through without a receiver-coordinate or pressure-unit conversion. The optional dataset package
is required for access to real data. The requested count is not validated; reject non-positive
requests before calling the loader. <a href="#ref-C">[C]</a></p>
<h3>4.2 Analytical basis and criteria</h3>
<p>For an isolated, nondispersive pulse emitted with its peak at the time origin, the ideal peak time
is the one-way travel distance divided by constant speed. This relation is obtained by rearranging
the velocity definition in College Physics, second edition, Time, Velocity, and Speed.
<a href="#ref-G">[G]</a></p>
<div class="eq">t<sub>reference</sub> = L / c</div>
<p>The symbol L denotes the one-way path length in metres; c denotes the assumed constant speed
in metres per second; t<sub>reference</sub> denotes the analytical peak time in seconds.
The example supplies L = {distance} m and c = {speed} m/s. The speed is a numerical fixture value,
not a sound speed assigned to any material. <a href="#ref-B">[B]</a></p>
<p>The target benchmark is agreement with the analytical peak when that peak lies on the sample
grid. The rationale is to check time selection independently of material properties. This is an
algorithm check, not an engineering acceptance criterion. The inspection limit remains NOT_EVALUATED.
<a href="#ref-A">[A]</a> <a href="#ref-B">[B]</a></p>
</section>

<section id="s5"><h2>5. Methodology</h2>
<p>The timestamp of the unique largest absolute sample is selected by the timing function.
The straight propagation path is used only to provide an independent analytical comparison.</p>
<div class="eq">k = arg max<sub>i</sub> |p<sub>i</sub>|; t<sub>peak</sub> = t<sub>k</sub></div>
<p>The index i identifies a sample; p<sub>i</sub> is its signed pressure; k is the unique index with
the largest magnitude; t<sub>i</sub> is its timestamp in seconds; t<sub>peak</sub> is the returned
timestamp. Taking the absolute value makes the result independent of polarity. This is a sampled
peak convention, not a first-arrival detector. <a href="#ref-A">[A]</a></p>
<p>The schematic identifies the source, the ideal one-way path and the receiver record.</p>
<figure><svg viewBox="0 0 800 240" role="img" aria-labelledby="scheme-title scheme-desc">
<title id="scheme-title">Ideal source-to-receiver acoustic timing path</title>
<desc id="scheme-desc">A cylindrical emitter and a receiver face one another across a straight path.
The source imposes a pulse and the receiver records pressure against time. This is a numerical
idealization with no specified hardware dimensions.</desc>
<defs><marker id="arrow" markerWidth="10" markerHeight="10" refX="9" refY="5" orient="auto">
<path d="M0 0L10 5L0 10Z" fill="currentColor"/></marker></defs>
<g fill="var(--panel-2)" stroke="var(--brand)" stroke-width="2">
<path d="M70 75Q90 60 110 75V145Q90 160 70 145Z"/>
<ellipse cx="110" cy="110" rx="12" ry="35"/>
<path d="M670 75Q690 60 710 75V145Q690 160 670 145Z"/>
<ellipse cx="670" cy="110" rx="12" ry="35"/></g>
<path d="M150 110H630" stroke="var(--brand)" stroke-width="2" marker-end="url(#arrow)"/>
<path d="M220 90Q245 110 220 130M270 80Q305 110 270 140M330 70Q375 110 330 150"
fill="none" stroke="var(--accent)"/>
<g fill="currentColor" text-anchor="middle"><text x="100" y="45">Emitter: prescribed pulse</text>
<text x="670" y="45">Receiver: sampled pressure</text>
<text x="395" y="185">One-way distance L; constant speed c</text>
<text x="395" y="215">No reflections, attenuation or material model is solved</text></g>
</svg><figcaption>Ideal timing arrangement used for the synthetic comparison; hardware dimensions
and boundary reflections are not represented. <a href="#ref-B">[B]</a></figcaption></figure>
<p>The example's Gaussian is evaluated at the recorded sample times. In dimensionally explicit form,
its exponent is the negative square of (L − ct) divided by a length scale, with relative amplitude
and length scale set by the synthetic fixture. These choices describe the test waveform only.
<a href="#ref-B">[B]</a></p>
<p>The difference is calculated as the returned time minus the reference time, divided by the reference
time and expressed as a percentage. A different emission reference requires subtraction of that
reference from the returned timestamp. <a href="#ref-A">[A]</a> <a href="#ref-G">[G]</a></p>
</section>

<section id="s6"><h2>6. Worked Example</h2>
<p>The public synthetic pulse example is evaluated by calling the timing function for both
polarities. Every input and result below is inserted from that calculation.</p>
<p>The one-way distance is {distance} m and the numerical speed is {speed} m/s. The record contains
{samples} samples from {start} s to {end} s. The Gaussian exponent has power {power}; these are
fixture inputs rather than engineering coefficients. <a href="#ref-B">[B]</a></p>
<p>The positive pulse returns {positive.peak_time_s} s against a reference of
{positive.reference_peak_time_s} s. Its difference is {positive.difference_percent} % of the
analytical reference. The negative pulse returns {negative.peak_time_s} s against
{negative.reference_peak_time_s} s, with a difference of {negative.difference_percent} %.
The result establishes peak selection for the documented sample grid only.
<a href="#ref-A">[A]</a> <a href="#ref-B">[B]</a></p>
</section>

<section id="s7"><h2>7. Validation</h2>
<p>The available validation evidence is separated into analytical timing checks and interface checks
with substitute datasets. Inspection performance is not established by either evidence category.</p>
<p>The timing checks cover both polarities, an irregular timestamp record with a shifted emission
reference, and rejection of empty, mismatched, unordered, nonfinite, zero-signal and tied-peak
inputs. A single sample with a negative time origin is also checked. The analytical target is
computed from the fixture distance, speed and emission reference. <a href="#ref-B">[B]</a></p>
<p>The loader tests check construction, variant selection, attribution, returned mapping and optional
dependency behaviour using substitute data objects. These checks establish the tested interface
behaviour, not compatibility with a real release of the external dataset package.
<a href="#ref-D">[D]</a> <a href="#ref-E">[E]</a></p>
<p>The published loader example demonstrates the intended access pattern but contains a
defect-detection placeholder. No measured receiver benchmark or accepted defect result is supplied
by that example. A real-data validation requires a versioned receiver trace and a calibration
reference with stated uncertainty. <a href="#ref-F">[F]</a></p>
</section>

<section id="s8"><h2>8. Findings and Recommendations</h2>
<p>The demonstrated calculation and the evidence gaps governing inspection use are identified
below. The recommendations are tied to the documented function contract and available tests.</p>
<ul><li>The synthetic peak is selected at the analytical target for either polarity.
<ul><li>Retain the analytical example as a regression check when changing the timing calculation.
<a href="#ref-B">[B]</a></li></ul></li>
<li>Timing accuracy between samples is not established; interpolation is absent.
<ul><li>Provide a calibrated timing reference and a sampling-resolution assessment before assigning
a measurement uncertainty. <a href="#ref-A">[A]</a></li></ul></li>
<li>Noise, multiple echoes, baseline offset and dispersion are not resolved by the timing function.
<ul><li>Supply an isolated, baseline-corrected pulse and a known emission reference. Reassess the
record when a maximum is tied or a pulse cannot be isolated. <a href="#ref-A">[A]</a></li></ul></li>
<li>Measured accuracy and inspection acceptance remain NOT_EVALUATED because calibration records,
material properties and an acceptance criterion are absent from the example.
<ul><li>Obtain those inputs and the governing inspection procedure before accepting a material or
defect result. <a href="#ref-A">[A]</a> <a href="#ref-F">[F]</a></li></ul></li>
<li>Real dataset compatibility is not established by the substitute-data tests.
<ul><li>Check the installed dataset version, constructor arguments, frame schema, receiver selection,
units and timestamps against an actual sample before coupling the loader to timing.
<a href="#ref-C">[C]</a> <a href="#ref-D">[D]</a> <a href="#ref-E">[E]</a></li></ul></li></ul>
</section>

<section id="s9"><h2>9. Appendices and References</h2>
<p>The data status, symbols, source records and calculation checks are provided to support
independent interpretation of this analysis.</p>
<h3>9.1 Data register, acronyms and symbols</h3>
<p>The synthetic inputs are distinguished from the information required for an inspection result.</p>
<table><thead><tr><th>Item</th><th>Status and intended use</th><th>Source</th></tr></thead><tbody>
<tr><td>Distance, speed, times, relative pressures</td><td>Public synthetic inputs for an algorithm
example. ASSUMED - to be confirmed for any physical application; not assigned to a real material.</td><td><a href="#ref-B">[B]</a></td></tr>
<tr><td>Emission reference and isolated pulse</td><td>Fixture assumptions; a calibrated reference and
window selection are required for inspection data.</td><td><a href="#ref-A">[A]</a></td></tr>
<tr><td>Material speed and its uncertainty</td><td>Required input for a physical distance interpretation;
NOT_EVALUATED.</td><td><a href="#ref-A">[A]</a></td></tr>
<tr><td>Acceptance criterion and detection threshold</td><td>Required inspection basis;
NOT_EVALUATED. No numerical limit is assigned.</td><td><a href="#ref-A">[A]</a></td></tr>
<tr><td>Measured trace and reference instrument</td><td>Required validation data;
NOT_EVALUATED.</td><td><a href="#ref-B">[B]</a></td></tr>
<tr><td>Real acoustic dataset schema</td><td>Compatibility is NOT_EVALUATED; substitute-data tests
do not establish a real receiver trace.</td><td><a href="#ref-C">[C]</a> <a href="#ref-E">[E]</a></td></tr>
</tbody><caption>Data register for the stated calculation and its inspection-use boundary.</caption></table>
<p>NDE means nondestructive evaluation. L is path length in metres; c is constant speed in metres
per second; t is time in seconds; p is signed receiver pressure in relative units in this example;
i is a sample index; k is the index of the unique largest magnitude. The symbols t<sub>peak</sub>
and t<sub>reference</sub> are the returned timestamp and analytical target respectively.
Relative units carry no pressure calibration.</p>
<h3>9.2 Sources and calculation access</h3>
<p>The source locators identify the implementation and published examples used for this edition.
No external dataset or licensed numerical model is consumed by the calculation.</p>
<ul class="refs">{records}
<li id="ref-G">[G] OpenStax, College Physics, second edition, 2022, section 2.3,
Time, Velocity, and Speed, equations 2.4 and 2.5. The elapsed-time and velocity definitions
are used as a kinematic basis; no textbook case data or figures are reproduced.
<a href="https://openstax.org/books/college-physics-2e/pages/2-3-time-velocity-and-speed">Public source</a>.</li>
<li>Layout exemplar: <a href="../../../api/cfd/turbulent-flat-plate-verification.html">existing technical
report layout</a>, with <a href="../../../api/_assets/brand.css">canonical brand stylesheet</a>.
Both stylesheets are inlined for standalone reading.</li></ul>
<p>The calculation is rebuilt from the repository root with the following command.</p>
<pre>PYTHONPATH=src uv run python docs/reports/modules/nde/build_report.py</pre>
<p>The direct timing entry point is documented in source A.</p>
<pre>from digitalmodel.nde.pulse_timing import pulse_peak_time
peak_time_s = pulse_peak_time(time_s, pressure)</pre>
<p>The report regression checks are executed with the existing NDE checks.</p>
<pre>PYTHONPATH=src uv run python -m pytest tests/nde tests/test_acoustic_scattering_loader.py</pre>
<h3>9.3 Quality assurance pre-flight evidence</h3>
<p>The rebuild checks compare the saved report with its reconstruction and compare the calculation
with a direct call of the timing function.</p>
<p>Every displayed worked-example value is checked against the computed inputs or results. The
chart samples are checked against the public fixture, and the signed percentage difference is
calculated from unrounded times. The printed pressure values retain nonzero small samples.
Internal section links, source paths and standalone delivery are checked by the rebuild tests.</p>
<p>The analytical peak falls exactly on a sample in this example. Sampling sensitivity away from that
grid point, measured noise and calibration uncertainty are not evaluated. No finite-element mesh,
structural load or buoyancy calculation is involved.</p>
</section>

<section id="s10"><h2>10. Not Covered</h2>
<p>The excluded capabilities are listed to bound the timing result and prevent its use as an
inspection acceptance decision.</p>
<p>Real-data execution of the basic, maze and inclusions loader variants is not performed. Scattering
field solution, receiver extraction, material characterization, defect detection, echo discrimination,
wall-thickness calculation and probability of detection are not evaluated. The calculation supports
selection of a sampled peak from a supplied trace only. <a href="#ref-A">[A]</a>
<a href="#ref-C">[C]</a> <a href="#ref-F">[F]</a></p>
</section>
</main>
<aside class="review-controls" aria-label="Document changes and comments">
<details><summary>Changes in this revision</summary><div id="change-panel"></div></details>
<details><summary>Comments on this analysis</summary>
<p>Choose the folder containing this report to save comments beside it. If folder access is unavailable,
save the downloaded comments file in that folder. Existing comments can be loaded before adding more.</p>
<button id="choose-folder" type="button">Choose report folder</button>
<label>Load comments <input id="load-comments" type="file" accept="application/json,.json"></label>
<form id="comment-form"><label for="comment-section">Section</label>
<select id="comment-section">{section_options}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" maxlength="6000" required></textarea>
<button type="submit">Add comment</button></form>
<ol id="comment-list"></ol><button id="save-comments" type="button">Save comments</button>
<output id="comment-status" aria-live="polite">Comments are kept in this page until saved.</output>
</details></aside>
<footer>Acoustic pulse timing · Numerical example only · Inspection acceptance is not evaluated.</footer>
</div><script id="example-data" type="application/json">{example}</script>
<script id="changes-data" type="application/json">{changes}</script>
<script>{script}</script></body></html>
'''


UI_SCRIPT = r'''"use strict";
const example = JSON.parse(document.getElementById("example-data").textContent);
const changes = JSON.parse(document.getElementById("changes-data").textContent);
const numberText = value => (Number.isInteger(value) ? value.toFixed(1) :
  value.toPrecision(3)).replaceAll("-", "−");
const detail = document.getElementById("pulse-detail");
document.querySelectorAll("circle[data-case]").forEach(point => {
  const inspect = () => { detail.textContent = point.getAttribute("aria-label"); };
  point.addEventListener("pointerenter", inspect);
  point.addEventListener("focus", inspect);
});
document.getElementById("polarity").addEventListener("change", event => {
  example.cases.forEach(item => {
    const group = document.getElementById(`trace-${item.name}`);
    group.toggleAttribute("hidden", item.name !== event.target.value);
  });
  const item = example.cases.find(item => item.name === event.target.value);
  detail.textContent = `${item.name} pulse; receiver peak time: ${numberText(item.peak_time_s)} s; ` +
    `analytical target: ${numberText(item.reference_peak_time_s)} s; inspection limit: not specified.`;
});
const panel = document.getElementById("change-panel");
changes.changes.forEach(change => {
  const paragraph = document.createElement("p");
  const link = document.createElement("a");
  link.href = `#${change.section}`;
  link.textContent = change.summary;
  paragraph.append(link, document.createTextNode(` ${change.reason}`));
  panel.append(paragraph);
});
let comments = [];
let directory = null;
let saved = true;
const status = document.getElementById("comment-status");
const sectionSelect = document.getElementById("comment-section");
const knownSections = new Set(Array.from(sectionSelect.options, item => item.value));
const decodeComments = incoming => {
  if (incoming.document !== changes.document || incoming.revision !== changes.revision ||
      !Array.isArray(incoming.comments) || incoming.comments.some(comment => !comment ||
        !knownSections.has(comment.section) || typeof comment.text !== "string" ||
        !comment.text.trim() || comment.text.length > 6000)) {
    throw new Error("The file does not contain comments for this analysis and revision.");
  }
  return incoming.comments.map(({section, text}) => ({section, text}));
};
const refresh = () => {
  const list = document.getElementById("comment-list");
  list.replaceChildren();
  comments.forEach(comment => {
    const item = document.createElement("li");
    const label = Array.from(sectionSelect.options).find(option => option.value === comment.section).text;
    item.textContent = `${label}: ${comment.text}`;
    list.append(item);
  });
};
document.getElementById("comment-form").addEventListener("submit", event => {
  event.preventDefault();
  const field = document.getElementById("comment-text");
  const text = field.value.trim();
  if (!text) return;
  comments.push({section: sectionSelect.value, text});
  field.value = "";
  saved = false;
  refresh();
  status.textContent = "Comment added. Save comments before closing the page.";
});
document.getElementById("choose-folder").addEventListener("click", async () => {
  try {
    if (!window.showDirectoryPicker) {
      status.textContent = "Folder access is unavailable. Save the downloaded comments file beside this report.";
      return;
    }
    directory = await window.showDirectoryPicker({mode: "readwrite"});
    status.textContent = "Report folder selected. Save comments to write the comments file there.";
  } catch (error) { status.textContent = `Folder selection was not completed: ${error.message}`; }
});
document.getElementById("load-comments").addEventListener("change", async event => {
  try {
    const file = event.target.files[0];
    if (!file) return;
    comments = [...comments, ...decodeComments(JSON.parse(await file.text()))];
    saved = false;
    refresh();
    status.textContent = "Comments loaded. Save to retain the combined comments.";
  } catch (error) { status.textContent = `Comments were not loaded: ${error.message}`; }
});
document.getElementById("save-comments").addEventListener("click", async () => {
  const name = "nde-module-analysis.comments.json";
  try {
    if (directory) {
      let existing = [];
      try {
        const handle = await directory.getFileHandle(name);
        existing = decodeComments(JSON.parse(await (await handle.getFile()).text()));
      } catch (error) { if (error.name !== "NotFoundError") throw error; }
      comments = Array.from(new Map([...existing, ...comments].map(comment =>
        [JSON.stringify([comment.section, comment.text]), comment])).values());
      refresh();
    }
    const text = JSON.stringify({document: changes.document, revision: changes.revision, comments}, null, 2) + "\n";
    if (directory) {
      const handle = await directory.getFileHandle(name, {create: true});
      const stream = await handle.createWritable();
      await stream.write(text);
      await stream.close();
      saved = true;
      status.textContent = "Comments saved in the selected report folder.";
    } else {
      const url = URL.createObjectURL(new Blob([text], {type: "application/json"}));
      const link = document.createElement("a");
      link.href = url;
      link.download = name;
      link.click();
      setTimeout(() => URL.revokeObjectURL(url), 1000);
      status.textContent = "Comments download requested. Save the file beside this report; unsaved comments remain in this page.";
    }
  } catch (error) { status.textContent = `Comments were not saved: ${error.message}`; }
});
window.addEventListener("beforeunload", event => {
  if (!saved) { event.preventDefault(); event.returnValue = ""; }
});
'''


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=REPORT_DIR)
    build_report(parser.parse_args().output_dir)
