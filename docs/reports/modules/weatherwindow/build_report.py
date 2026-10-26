# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the self-contained weather-window module report from its public example.

Rebuild: PYTHONPATH=src python docs/reports/modules/weatherwindow/build_report.py
Dependencies: numpy, pandas, pydantic, scipy and PyYAML, used by the public imports.
"""

from __future__ import annotations

import argparse
import ast
import html
import json
import re
from pathlib import Path

import yaml

from digitalmodel.weather_window.workflow import assess_weather_window, window_lengths_hours

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = "examples/workflows/weather-window/input.yml"
EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
BRAND = "docs/api/_assets/brand.css"


def read_utf8(path: Path) -> str:
    """Read UTF-8 with an explicit LF convention on every platform."""
    with path.open(encoding="utf-8", newline="\n") as stream:
        return stream.read()


def lookup(record: dict, locator: str):
    """Resolve a dotted binding into the calculated record."""
    value = record
    for part in locator.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return value


def display(value) -> str:
    """Keep counts integral, statistics to a decimal and sub-hour inputs exact."""
    if isinstance(value, bool):
        return "Yes" if value else "No"
    if value is None:
        return "Not specified"
    if isinstance(value, float):
        if value.is_integer():
            return str(int(value))
        return f"{value:.2f}" if abs(value) < 1 else f"{value:.1f}"
    return str(value)


def test_inventory(relative: str) -> int:
    """Count test definitions as inventory, without importing their dependencies."""
    tree = ast.parse(read_utf8(ROOT / relative))
    return sum(isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
               and node.name.startswith("test_") for node in ast.walk(tree))


def calculate() -> dict:
    """Call the module using the existing public, explicitly illustrative example."""
    settings = yaml.safe_load(read_utf8(ROOT / EXAMPLE))["weather_window"]
    operation = settings["operation"]
    metocean = settings["metocean"]
    heights = metocean["hs_timeseries_m"]
    interval = metocean["time_step_hours"]
    result = assess_weather_window(
        heights, operation["hs_limit_m"], operation["required_duration_hours"],
        interval, operation["required_workability_pct"],
    )
    windows = window_lengths_hours(heights, operation["hs_limit_m"], interval)
    # Diagnostic inputs are copied from the existing knowledge pack's boundary example.
    diagnostic_result = assess_weather_window([1], 2, 1, 0.96)
    return {
        "hs_m": heights,
        "sample_count": len(heights),
        "sample_start_hours": [i * interval for i in range(len(heights))],
        "windows_hours": windows,
        "result": result,
        "diagnostic": {
            "hs_m": 1, "hs_limit_m": 2, "required_hours": 1, "interval_hours": 0.96,
            "raw_longest_hours": window_lengths_hours([1], 2, 0.96)[0],
            "result": diagnostic_result,
        },
        "upstream_test_count": test_inventory("tests/orcaflex/test_weather_window.py"),
        "report_test_count": test_inventory("tests/reporting/test_weatherwindow_module_report.py"),
    }


def table(headers: list[str], rows: list[list[str]]) -> str:
    """Render escaped headers and already escaped cells."""
    head = "".join(f'<th scope="col">{html.escape(v)}</th>' for v in headers)
    body = "".join("<tr>" + "".join(f"<td>{v}</td>" for v in row) + "</tr>" for row in rows)
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>"


def render(record: dict) -> str:
    """Write the engineering narrative with all example values bound to results."""
    def value(key: str) -> str:
        return f'<span data-value="{key}">{html.escape(display(lookup(record, key)))}</span>'

    def result(key: str) -> str:
        return value("result." + key)

    def cite(key: str) -> str:
        return f'<a href="#ref-{key}">[{key}]</a>'

    revisions = json.loads(read_utf8(HERE / "changes.json"))
    history = table(["Revision", "Date", "Change and reason"], [
        [html.escape(row["revision"]), html.escape(row["date"]), html.escape(" ".join(row["changes"]))]
        for row in revisions["history"]
    ])
    revision_options = "".join(
        f'<option value="{html.escape(row["revision"])}">Revision {html.escape(row["revision"])}</option>'
        for row in revisions["history"]
    )
    css = re.search(r"<style>.*?</style>", read_utf8(ROOT / EXEMPLAR), re.S).group(0)
    brand = read_utf8(ROOT / BRAND)
    # Preserve the canonical CSS rules; its repository guidance is not client text.
    brand = re.sub(r"/\*.*?\*/", "", brand, flags=re.S)
    brand = "\n".join(line.rstrip() for line in brand.splitlines()) + "\n"
    outputs = [
        ("record_hours", "Record exposure", "h"),
        ("workability_pct", "Accepted exposure", "%"),
        ("longest_window_hours", "Longest spell", "h"),
        ("mean_window_hours", "Mean spell", "h"),
        ("p10_window_hours", "Lower spell percentile", "h"),
        ("p90_window_hours", "Upper spell percentile", "h"),
        ("num_windows", "Accepted spells", "count"),
        ("num_viable_windows", "Spells meeting duration", "count"),
        ("operation_fits_window", "Duration fit", "Yes / No"),
        ("screening_status", "Screening status", "pass / fail"),
    ]
    results_table = table(["Output", "Value", "Unit", "Source"], [
        [label, result(key), unit, cite("E")] for key, label, unit in outputs
    ])
    samples_table = table(["Sample start (h)", "Significant wave height (m)", "Source"], [
        [value(f"sample_start_hours.{i}"), value(f"hs_m.{i}"), cite("E")]
        for i in range(record["sample_count"])
    ])
    window_values = ", ".join(value(f"windows_hours.{i}") for i in range(len(record["windows_hours"])))
    charts = {
        "heights": [float(display(v)) for v in record["hs_m"]],
        "times": [float(display(v)) for v in record["sample_start_hours"]],
        "interval": float(display(record["result"]["time_step_hours"])),
        "heightLimit": float(display(record["result"]["hs_limit_m"])),
        "required": float(display(record["result"]["required_duration_hours"])),
        "windows": [float(display(v)) for v in record["windows_hours"]],
        "heightAccepted": [v <= record["result"]["hs_limit_m"] for v in record["hs_m"]],
        "windowAccepted": [v >= record["result"]["required_duration_hours"] for v in record["windows_hours"]],
    }
    source_table = table(["Item", "Repository locator"], [
        ["Public illustrative input", f"<code>{EXAMPLE}</code>"],
        ["Public screening functions and router", "<code>src/digitalmodel/weather_window/workflow.py</code>"],
        ["Persistence dependency", "<code>src/digitalmodel/orcaflex/weather_window.py</code>"],
        ["Existing direct calculation test", "<code>tests/workflows/test_durable_workflows.py::test_weather_window_persistence_and_fit_closed_form</code>"],
        ["Existing configured example test", "<code>tests/workflows/test_durable_workflows.py::test_workflow_registry[weather-window]</code>"],
        ["Dependency tests", "<code>tests/orcaflex/test_weather_window.py</code>"],
        ["Known input and rounding gaps", "<code>src/digitalmodel/weather_window/docs/agent-pack.md</code>, input contract and decision-boundary discussion"],
        ["Report checks", "<code>tests/reporting/test_weatherwindow_module_report.py</code>"],
        ["Report rebuild", "<code>PYTHONPATH=src python docs/reports/modules/weatherwindow/build_report.py</code>"],
        ["Layout exemplar", f"<code>{EXEMPLAR}</code>; <code>{BRAND}</code>"],
    ])
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light">
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Weather-window module analysis</title>
<style>{brand}</style>
{css}
<style>
svg{{width:100%;height:auto}} svg text{{font-family:inherit;fill:var(--ink)}}
svg .demand{{fill:var(--brand)}} svg .limit{{stroke:var(--nogo);stroke-width:2;stroke-dasharray:7 4}}
svg .fail{{fill:var(--nogo)}} svg .pass{{fill:var(--go)}}
button,select,textarea{{font:inherit;padding:.4em;max-width:100%}}
textarea{{display:block;width:100%;min-height:6em}} .controls{{display:flex;gap:1em;flex-wrap:wrap}}
.chart-readout{{min-height:3em}} .review-note{{white-space:pre-wrap;border-bottom:1px solid var(--line);padding:1em 0}}
@media print{{.review-controls{{display:none}}}}
</style></head>
<body class="hs-doc"><main class="wrap">
<header><p class="kicker">Marine operations · Module analysis</p>
<h1>Weather-window screening</h1><p class="sub">Draft for maintainer review</p>
<p>A fixed-interval significant-wave-height record is assessed for available continuous operating time.</p></header>

<section id="executive"><h2>1. Executive summary</h2>
<p>The weather-window module is a record-based screening tool for marine operations.
Analysis is performed to determine whether a supplied wave-height record contains sufficient continuous operating time;
key results are summarized below.</p>
<ul><li>The illustrative record fails the duration screen.
<ul><li>The longest accepted spell is {result('longest_window_hours')} h against a required {result('required_duration_hours')} h.
An operation start is not supported by this record under the supplied duration criterion.</li></ul></li>
<li>Accepted exposure alone does not establish a usable weather window.
<ul><li>Accepted exposure is {result('workability_pct')}% against an input threshold of {result('required_workability_pct')}%,
but {result('num_viable_windows')} spells meet the required duration.</li></ul></li></ul>
<p>These are results for a synthetic public repository example, not a forecast or a location-specific installation envelope.
Heading, wave period, structural response and a live-support trigger are NOT_EVALUATED.</p>
<h3>Revision history</h3>{history}
</section>

<section id="introduction"><h2>2. Introduction</h2>
<p>The main screening workflow is described from supplied environmental samples to the returned decision.
The result is intended for preliminary planning by an engineer who supplies the operational limits.</p>
<p>Significant wave height, denoted Hs, is accepted in metres. A fixed interval is assigned to every sample.
No wave transformation, response analysis or environmental data retrieval is performed by this workflow.
The public example is used unchanged for calculation; its descriptive case identifier and output directory do not affect the calculation. {cite('E')}</p>
<p>The Python entry point is <code>assess_weather_window</code>; <code>window_lengths_hours</code> returns the continuous spells,
and <code>router</code> accepts a configuration mapping and writes a summary. Their exact locators are provided in Section 9.2.</p>
</section>

<section id="summary"><h2>3. Operating summary</h2>
<p>The wave-height and duration criteria are displayed separately so that the governing restriction is visible.
The duration restriction governs this illustrative assessment.</p>
<p>Input wave-height limit: {result('hs_limit_m')} m. Required continuous duration: {result('required_duration_hours')} h.
The screen returns <span data-value="result.screening_status">{html.escape(record['result']['screening_status'])}</span>.
The longest accepted spell is {result('longest_window_hours')} h; accepted exposure is {result('workability_pct')}%.</p>
<figure><svg id="height-chart" viewBox="0 0 860 320" role="img" tabindex="0" aria-labelledby="height-caption"></svg>
<figcaption id="height-caption">Significant wave height against elapsed time. The dashed line is the supplied wave-height limit.
Each step represents an interval, not an interpolated measurement. Hover or focus a step for its demand, limit and status.</figcaption></figure>
<p id="height-readout" class="chart-readout" aria-live="polite">Select a wave-height interval for details.</p>
<figure><svg id="window-chart" viewBox="0 0 860 320" role="img" tabindex="0" aria-labelledby="window-caption"></svg>
<figcaption id="window-caption">Accepted spell duration against spell index. The dashed line is the required continuous duration.
Hover or focus a bar for the duration criterion. Period-dependent and heading-dependent envelopes are NOT_EVALUATED.</figcaption></figure>
<p id="window-readout" class="chart-readout" aria-live="polite">Select a spell for details.</p>
<p>Legend: Green means the plotted criterion is met. Red means the plotted criterion is not met.
Amber means an assessment is not available; no amber data series is computed here.
A green wave-height interval does not establish that the complete operation fits.</p>
<p>The record is rejected for a continuous operation requiring {result('required_duration_hours')} h.
Use a separately qualified forecast and a demonstrated continuous window before an operation start is accepted.
The response, heading tolerance and forecast-support trigger remain required engineering inputs. {cite('E')}</p>
</section>

<section id="basis"><h2>4. Design data and criteria</h2>
<p>The supplied example is treated as illustrative design data. Its limits are input choices rather than values derived from a standard.</p>
<h3>Input contract</h3>
<p>The required inputs are a nonempty list of Hs samples in metres, a positive Hs limit in metres and a positive operation duration in hours.
A positive time step in hours and an optional required workability in percent are also accepted.
The default interval is overridden explicitly here by {result('time_step_hours')} h, as supplied by the example. {cite('E')}</p>
<p>The example contains {value('sample_count')} samples representing {result('record_hours')} h.
Its Hs limit is {result('hs_limit_m')} m and its workability threshold is {result('required_workability_pct')}%.
All sample values and their elapsed-time coordinates are provided in the appendix. {cite('E')}</p>
<h3>Criteria and rationale</h3>
<p>A sample is accepted when Hs is less than or equal to the input limit. A spell is viable when its unrounded duration is at least the required duration.
The overall screen requires a duration fit and, when supplied, the workability threshold.
The separate duration criterion prevents a fragmented calm record from being accepted solely on its exposure percentage. {cite('E')}</p>
<p>ASSUMED - to be confirmed: each sample describes its complete following interval and all intervals are equally spaced.
No interpolation across a threshold is applied. A physical Hs limit, direction tolerance, wave-period limit, wind limit,
current limit and forecast uncertainty allowance require an operation-specific basis; none is inferred here.</p>
<h3>System boundary</h3>
<p>The represented system is the environmental screening calculation. Geometry, crane capacity, rigging, structural response,
hydrodynamic response functions and installation stages are outside its inputs, so installation acceptance is NOT_EVALUATED.</p>
<figure><svg viewBox="0 0 860 180" role="img" aria-labelledby="system-caption">
<path d="M20 55 Q40 20 60 55 T100 55 T140 55 T180 55" fill="none" stroke="var(--brand)" stroke-width="3"/>
<path d="M195 55 H245 M235 45 L245 55 L235 65 M410 55 H460 M450 45 L460 55 L450 65 M650 55 H700 M690 45 L700 55 L690 65" fill="none" stroke="var(--ink)"/>
<text x="20" y="105">Hs record</text><text x="20" y="132">Fixed-interval samples</text>
<text x="255" y="50">Hs threshold</text><text x="255" y="78">Inclusive comparison</text>
<text x="470" y="50">Contiguous spells</text><text x="470" y="78">Interval counting</text>
<text x="710" y="50">Duration fit</text><text x="710" y="78">Exposure check</text>
</svg><figcaption id="system-caption">Environmental screening boundary. A time record is reduced to threshold decisions and continuous spells;
no physical load path or structural mesh is represented.</figcaption></figure>
</section>

<section id="method"><h2>5. Theory and methodology</h2>
<p>Accepted exposure is calculated by relative frequency, and persistence is calculated by counting consecutive accepted samples.
The elementary statistical definitions are sourced below; the acceptance inequalities are supplied screening conventions. {cite('S1')} {cite('S2')}</p>
<h3>Discrete exposure and persistence</h3>
<p>Let N be the sample count, Δt the input interval in hours, Hs(n) the wave height at sample index n in metres and L the input wave-height limit in metres.
The indicator b(n) is unity when Hs(n) ≤ L and zero otherwise. Define T as represented exposure in hours and W as accepted exposure in percent.</p>
<div class="eq">T = N Δt; W = 100 Σ b(n) / N.</div>
<p>The relative-frequency definition is given in the cited statistics textbook; multiplication by 100 expresses a fraction as percent.
Assigning Δt to each sample is the declared interval convention, not an estimate of the measured transition time. {cite('S1')}</p>
<p>A run is a consecutive sequence of identical classifications. Let k(j) be the number of accepted samples in run j,
d(j) its duration in hours and M the number of accepted runs. Run terminology follows the cited handbook. {cite('S2')}</p>
<div class="eq">d(j) = k(j) Δt; mean duration = Σ d(j) / M; longest duration = max d(j).</div>
<p>The mean is the arithmetic mean defined in the statistics textbook. The longest duration is the largest observed run, not a prediction of future availability.
For an all-rejected record the implementation returns no runs and zero duration statistics. {cite('S3')} {cite('E')}</p>
<h3>Percentiles and the returned decision</h3>
<p>For ordered durations y, the linear percentile convention uses rank r = 1 + p(M − 1), k = floor(r), f = r − k,
and Q(p) = y(k) + f[y(k+1) − y(k)]. Here p is the requested fractional percentile and Q is in hours;
at the last rank the largest duration is used. This is the R7 alternative listed in the handbook;
the handbook's default method is R6. {cite('S4')}</p>
<p>The reported lower and upper percentiles use p = 0.1 and p = 0.9 respectively.
They describe the sampled spells; neither is a confidence bound nor a forecast exceedance probability. {cite('S4')} {cite('E')}</p>
<p>Let D be required duration in hours and Wmin the optional input workability threshold in percent.
The intended duration decision is max d(j) ≥ D; viability is counted separately with d(j) ≥ D.
The screen passes only when duration fits and W ≥ Wmin, with the exposure test omitted when Wmin is absent. {cite('E')}</p>
<p>The implementation rounds persistence statistics and total accepted hours to a decimal place before calculating fit and exposure.
The viable count instead uses unrounded durations. This distinction matters near a decision boundary and is demonstrated in the appendix. {cite('E')}</p>
<p>No standards-derived coefficient is used in this calculation. The software's standards label lacks an edition and clause-qualified criterion;
standards compliance is NOT_EVALUATED. A cited statistical definition does not qualify a marine operating limit.</p>
</section>

<section id="results"><h2>6. Worked example and outputs</h2>
<p>The existing public example is calculated by calling the documented function with its supplied values.
The example is synthetic and provides arithmetic demonstration rather than site or field validation. {cite('E')}</p>
<p>The accepted spell durations are {window_values} h. The longest is {result('longest_window_hours')} h,
so an operation requiring {result('required_duration_hours')} h has {result('num_viable_windows')} viable windows.
Accepted exposure is {result('workability_pct')}%, which exceeds its supplied {result('required_workability_pct')}% threshold;
the failed duration criterion still gives the overall result <span data-value="result.screening_status">{html.escape(record['result']['screening_status'])}</span>.</p>
{results_table}
<p>The public function returns a dictionary with these results and the input echoes, plus a descriptive standards string.
The configuration router adds the result and screening status to the configuration, writes a UTF-8 JSON summary and returns its location.
The location is added after the file is written, so it is present in the returned mapping rather than inside the saved summary. {cite('E')}</p>
<p>To use the interface, import <code>assess_weather_window</code> from the screening workflow and pass
<code>hs_timeseries</code>, <code>hs_limit</code>, <code>required_duration_hours</code>, <code>time_step_hours</code>
and, when required, <code>required_workability_pct</code>. No solver invocation is needed for this calculation.</p>
</section>

<section id="validation"><h2>7. Validation evidence</h2>
<p>The available evidence is composed of executable arithmetic tests and illustrative examples.
Agreement with measured operations or an independently published marine benchmark is NOT_EVALUATED.</p>
<p>The dependency test file contains {value('upstream_test_count')} test definitions covering synthetic scatter normalization,
monotonic operability, all-accepted and all-rejected persistence, alternating spells, accepted exposure,
seasonal summaries and waiting-on-weather arithmetic. Only persistence is used by the main workflow described here. {cite('E')}</p>
<p>The existing direct calculation test checks separated accepted runs, workability, fit and rejection for a longer duration.
The configured-example test checks the public illustrative input and its failed duration screen.
These executable records are identified in Section 9.2; their presence is not an execution claim. {cite('E')}</p>
<p>The report is accompanied by {value('report_test_count')} test definitions.
Hand-worked literal anchors check record exposure, accepted exposure, longest spell, mean and percentiles.
Consistency checks compare the result dictionary, every displayed numeric binding and the chart inputs with a fresh function call.
The rebuild comparison checks the complete artifact byte for byte, including all full-precision audit metadata.</p>
<p>The report tests also check the configured router and its saved summary, local reference targets, the inlined exemplar stylesheet,
comment controls and the rounding diagnostic.
No field dataset, metocean timestamp quality check, forecast skill test or structural installation validation is supplied by these checks.</p>
</section>

<section id="findings"><h2>8. Findings, limitations and recommendations</h2>
<p>The demonstrated calculation returns the duration rejection for the supplied illustrative record.
Engineering acceptance remains dependent on qualified data and unrounded decision quantities.</p>
<h3>Limits of use</h3>
<p>Negative and non-finite wave samples are not explicitly rejected. The workability threshold is not constrained to a physical percentage range.
No timestamps are supplied, so gaps, irregular sampling and duplicate observations are undetected.
Record-edge spells are counted in full even though their continuation outside the record is unknown. {cite('E')}</p>
<p>Rounded persistence maxima and accepted exposure can disagree with unrounded viability or exceed a physical percentage for short intervals.
The diagnostic in the appendix demonstrates that a passing status is not sufficient at such boundaries. {cite('E')}</p>
<p>The example represents a short illustrative record. Its exposure fraction is not annual operability, a start-time distribution,
a waiting-time estimate or a probability of completing an operation. Forecast uncertainty, safe suspension,
contingency duration and recovery criteria are not represented.</p>
<h3>Recommendations</h3>
<p>For the failed illustrative duration screen, retain the input Hs limit and require a qualified continuous forecast interval meeting the required duration before a start decision.
A revised operation duration requires a separately justified execution and recovery sequence; accepted exposure alone does not remove the duration restriction. {cite('E')}</p>
<p>Before operational use, calculate decisions from unrounded durations and exposure, and add boundary tests requiring fit to agree with viable count.
Reject non-finite and negative samples, require consistent timestamps and constrain any percentage threshold to its defined range.
These actions address the observed input and rounding limitations. {cite('E')} {cite('S1')}</p>
<p>Supply approved limits for heading, wave period, wind, current and structural response before installation acceptance is assessed.
Define the forecast validity interval, uncertainty allowance, suspension criteria and forecast-support trigger from the approved operation basis.
No numerical installation envelope is assigned in their absence.</p>
</section>

<section id="references"><h2>9. References and calculation records</h2>
<p>The statistical definitions are supported by open textbook and handbook sources.
Repository calculation records provide the input values and implemented conventions.</p>
<h3>9.1 Open sources</h3>
<ul class="refs">
<li id="ref-S1">[S1] OpenStax, Introductory Statistics, second edition, 2023,
<a href="https://openstax.org/books/introductory-statistics-2e/pages/1-3-frequency-frequency-tables-and-levels-of-measurement">Section 1.3, Frequency, Frequency Tables, and Levels of Measurement</a>.
Relative frequency and percent representation; no environmental limit is taken from this source.</li>
<li id="ref-S2">[S2] NIST/SEMATECH e-Handbook of Statistical Methods, online edition accessed 22 October 2026,
<a href="https://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm">Section 1.3.5.13, Runs Test for Detecting Non-randomness</a>.
Only the definition of a run is used; no randomness test is performed.</li>
<li id="ref-S3">[S3] OpenStax, Introductory Statistics, second edition, 2023,
<a href="https://openstax.org/books/introductory-statistics-2e/pages/2-5-measures-of-the-center-of-the-data">Section 2.5, Measures of the Center of the Data</a>.
Arithmetic mean.</li>
<li id="ref-S4">[S4] NIST/SEMATECH e-Handbook of Statistical Methods, online edition accessed 22 October 2026,
<a href="https://www.itl.nist.gov/div898/handbook/prc/section2/prc262.htm">Section 7.2.6.2, Percentiles</a>.
Alternative percentile methods, R7 interpolation.</li>
</ul>
<h3>9.2 Repository locators</h3>
<p id="ref-E">[E] The input example, calculation sources and tests below form the repository evidence for this report.
They establish implemented behaviour rather than a standard-qualified installation criterion.</p>
{source_table}
</section>

<section id="appendix"><h2>10. Appendices</h2>
<p>The complete illustrative record, missing-data register and decision-boundary evidence are provided below.
Coverage is bounded to the main fixed-interval screening workflow.</p>
<h3>Input record and data register</h3>
<p>Every row below is a sample from the public illustrative example. The final row represents a complete following interval under the declared convention. {cite('E')}</p>
{samples_table}
{table(['Data item', 'Basis and disposition'], [
    ['Hs samples, interval, wave-height limit, required duration and workability threshold', 'Public repository example; illustrative inputs, not measured data or approved operating limits. [E]'],
    ['Sample interpretation', 'ASSUMED - to be confirmed: equally spaced, complete following intervals; no interpolation.'],
    ['Site, timestamps, datum and measurement quality', 'Required inputs for operational assessment; NOT_EVALUATED.'],
    ['Heading, period, wind, current, response limits and uncertainty allowance', 'Required operation-specific inputs; NOT_EVALUATED.'],
    ['Structure, crane, rigging and recovery sequence', 'Required for installation acceptance; NOT_EVALUATED.'],
    ['Live forecast and support trigger', 'Required operation-specific inputs; NOT_EVALUATED.'],
    ['Rounding diagnostic', 'Constructed diagnostic already documented with the module; not an environmental observation. [E]'],
])}
<h3>Decision-boundary diagnostic</h3>
<p>The existing rounding diagnostic uses a single Hs sample of {value('diagnostic.hs_m')} m, a limit of {value('diagnostic.hs_limit_m')} m,
an interval of {value('diagnostic.interval_hours')} h and a required duration of {value('diagnostic.required_hours')} h.
The raw spell is {value('diagnostic.raw_longest_hours')} h, but the rounded longest spell is {value('diagnostic.result.longest_window_hours')} h.
The returned fit is {value('diagnostic.result.operation_fits_window')}, the viable count is {value('diagnostic.result.num_viable_windows')},
and accepted exposure is {value('diagnostic.result.workability_pct')}%. The returned status is {value('diagnostic.result.screening_status')}.
This inconsistent result is rejected as an operating basis; decisions require unrounded quantities and consistent counts. {cite('E')}</p>
<h3>Coverage and not covered</h3>
<p>Covered: the public assessment function, continuous-spell extraction, the imported persistence statistics,
the illustrative input and the router's documented output contract.
Not covered: scatter-based operability, seasonal statistics, waiting-on-weather estimates,
atlas interpolation, multi-parameter operating envelopes and any licensed simulation interface.</p>
<h3>Quality pre-flight evidence</h3>
<p>Arithmetic anchors, module consistency, displayed-input recomputation, reference resolution,
portable UTF-8 rebuilding and self-contained resources are checked by the accompanying report tests.
The source metadata is embedded below the page as calculation data; visible statistics and chart arrays use displayed precision.
Chart acceptance flags are computed from unrounded wave heights, durations and limits before display rounding.
Source-page retrieval records are separate from engineering acceptance evidence.</p>
<p>Comment storage is provided below. In compatible browsers, selecting the report folder enables adjacent saving;
otherwise the comment file is downloaded and placed beside this HTML. Comments are local review annotations and do not change calculation results.</p>
<p>Only comment files labelled with the current report revision can be loaded.
Keep older files with their corresponding report and review those notes separately before adding current comments.</p>
</section>

<aside class="review-controls" aria-label="Report review">
<h3>Engineering changes</h3><label for="last-revision">Last revision read</label>
<select id="last-revision"><option value="none">No earlier revision</option>{revision_options}</select>
<p id="change-list"></p>
<h3>Comments</h3><label for="comment-section">Report section</label><select id="comment-section"></select>
<label for="comment-text">Comment text</label><textarea id="comment-text"></textarea>
<div class="controls"><button id="add-comment" type="button">Add comment</button>
<button id="open-folder" type="button">Open report folder</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label>Load comments <input id="load-comments" type="file" accept="application/json,.json"></label></div>
<p id="comment-status" aria-live="polite">Comments are unsaved until the comment file is saved.</p><div id="comment-list"></div>
</aside>
<footer>Weather-window module analysis · Illustrative fixed-interval screening · Draft for maintainer review</footer>
</main>
<script id="calculation" type="application/json">{json.dumps(record, ensure_ascii=False, allow_nan=False)}</script>
<script id="chart-data" type="application/json">{json.dumps(charts, ensure_ascii=False, allow_nan=False)}</script>
<script id="engineering-changes" type="application/json">{json.dumps(revisions, ensure_ascii=False, allow_nan=False)}</script>
{INTERACTION}
</body></html>
'''


INTERACTION = r'''<script>
"use strict";
const chart = JSON.parse(document.getElementById("chart-data").textContent);
const ns = "http://www.w3.org/2000/svg";
function element(parent, tag, attrs, text) {
  const node = document.createElementNS(ns, tag);
  Object.entries(attrs).forEach(([key, value]) => node.setAttribute(key, value));
  if (text !== undefined) node.textContent = text;
  parent.appendChild(node); return node;
}
function axes(svg, xmax, ymax, xlabel, ylabel, indexed=false) {
  element(svg, "path", {d:"M65 25 V260 H825", fill:"none", stroke:"var(--ink)"});
  element(svg, "text", {x:340,y:310}, xlabel);
  element(svg, "text", {x:65,y:18}, ylabel);
  for (let i=0;i<=4;i++) {
    if (!indexed) element(svg,"text",{x:65+i*190,y:280},String(Math.round(xmax*i/4*10)/10));
    element(svg,"text",{x:15,y:265-i*55},String(Math.round(ymax*i/4*10)/10));
  }
}
function hover(node, message, readout) {
  node.setAttribute("tabindex","0"); node.setAttribute("aria-label",message);
  element(node,"title",{},message);
  for (const event of ["mouseenter","focus","click"]) node.addEventListener(event,()=>{
    document.getElementById(readout).textContent=message;
  });
}
function waveChart() {
  const svg = document.getElementById("height-chart");
  const xmax = chart.times.at(-1)+chart.interval;
  // Multiples of 2 m give quarter-axis ticks on exact 0.5 m increments.
  const ymax = Math.ceil(Math.max(chart.heightLimit,...chart.heights)*1.2/2)*2;
  axes(svg,xmax,ymax,"Elapsed time (h)","Significant wave height (m)");
  const y = v=>260-v/ymax*220, x = v=>65+v/xmax*760;
  chart.heights.forEach((height,i)=>{
    const start=chart.times[i], end=start+chart.interval;
    const accepted = chart.heightAccepted[i];
    const line=element(svg,"path",{d:`M${x(start)} ${y(height)} H${x(end)}`,
      fill:"none",stroke:accepted?"var(--go)":"var(--nogo)","stroke-width":5});
    hover(line,`Public illustrative case; interval ${i+1}; wave-height criterion; time ${start}–${end} h; demand ${height} m; limit ${chart.heightLimit} m; ${accepted?"Criterion is met.":"Criterion is not met."}`,"height-readout");
  });
  element(svg,"line",{x1:65,x2:825,y1:y(chart.heightLimit),y2:y(chart.heightLimit),class:"limit","data-limit":chart.heightLimit});
  element(svg,"text",{x:620,y:y(chart.heightLimit)-9},`Hs limit ${chart.heightLimit} m`);
}
function windowChart() {
  const svg = document.getElementById("window-chart");
  const ymax = Math.max(chart.required,...chart.windows)*1.2;
  axes(svg,chart.windows.length,ymax,"Accepted spell index (count)","Continuous duration (h)",true);
  const y=v=>260-v/ymax*220, spacing=760/chart.windows.length;
  chart.windows.forEach((duration,i)=>{
    const accepted=chart.windowAccepted[i];
    element(svg,"text",{x:65+(i+.5)*spacing,y:280},String(i+1));
    const bar=element(svg,"rect",{x:65+i*spacing+spacing*.2,y:y(duration),width:spacing*.6,
      height:260-y(duration),class:accepted?"pass":"fail"});
    hover(bar,`Public illustrative case; spell ${i+1}; duration criterion; demand ${chart.required} h; available duration ${duration} h; required limit ${chart.required} h; ${accepted?"Criterion is met.":"Criterion is not met."}`,"window-readout");
  });
  element(svg,"line",{x1:65,x2:825,y1:y(chart.required),y2:y(chart.required),class:"limit","data-limit":chart.required});
  element(svg,"text",{x:540,y:y(chart.required)-9},`Required duration ${chart.required} h`);
}
waveChart(); windowChart();
const changes=JSON.parse(document.getElementById("engineering-changes").textContent);
const currentRevision=changes.history[changes.history.length-1].revision;
function showChanges() {
  const last=document.getElementById("last-revision").value;
  const selected=changes.history.findIndex(row=>row.revision===last);
  const later=changes.history.slice(selected+1).flatMap(row=>row.changes);
  document.getElementById("change-list").textContent = later.length
    ? later.join(" ") : "No engineering changes since the selected revision.";
}
document.getElementById("last-revision").addEventListener("change",showChanges); showChanges();
const sections = [...document.querySelectorAll("section[id]")];
const select = document.getElementById("comment-section");
for (const section of sections) {
  const option=document.createElement("option"); option.value=section.id;
  option.textContent=section.querySelector("h2").textContent; select.appendChild(option);
}
let comments=[], directory=null, savedText="[]";
const status=document.getElementById("comment-status");
function validComments(payload) {
  if (payload.report!=="weather-window-module" || !Array.isArray(payload.comments)) throw new Error("Comment file does not match this report revision.");
  if (payload.revision!==currentRevision) {
    const older=changes.history.slice(0,-1).some(row=>row.revision===payload.revision);
    throw new Error(older
      ? "Comments belong to an older report revision. Keep the original file and review those notes separately before adding current comments."
      : "Comment file does not match this report revision.");
  }
  if (!payload.comments.every(c=>typeof c.text==="string" && c.text.trim() && sections.some(s=>s.id===c.section))) throw new Error("Comment data is invalid.");
  return payload.comments.map(c=>({section:c.section,text:c.text}));
}
function showComments() {
  const list=document.getElementById("comment-list"); list.replaceChildren();
  for (const comment of comments) {
    const p=document.createElement("p"); p.className="review-note";
    p.textContent=`${document.getElementById(comment.section).querySelector("h2").textContent}: ${comment.text}`;
    list.appendChild(p);
  }
}
document.getElementById("add-comment").addEventListener("click",()=>{
  const input=document.getElementById("comment-text"); if (!input.value.trim()) return;
  comments.push({section:select.value,text:input.value.trim()}); input.value="";
  showComments(); status.textContent="Comments have unsaved changes.";
});
document.getElementById("load-comments").addEventListener("change",async event=>{
  try {
    if (!event.target.files.length) return;
    const loaded=validComments(JSON.parse(await event.target.files[0].text()));
    if (JSON.stringify(comments)!==savedText) throw new Error("Save current comments before loading another file.");
    comments=loaded; savedText=JSON.stringify(comments); showComments(); status.textContent="Comment file loaded.";
  } catch(error) {status.textContent=error.message;}
});
document.getElementById("open-folder").addEventListener("click",async()=>{
  try {
    if (!window.showDirectoryPicker) throw new Error("Folder access is unavailable. Save downloads the comment file; place it beside this HTML.");
    const chosen=await window.showDirectoryPicker({mode:"readwrite"});
    const report=await (await chosen.getFileHandle("report.html")).getFile();
    if (!(await report.text()).includes("<title>Weather-window module analysis</title>")) throw new Error("Select the folder containing this report.html.");
    if (JSON.stringify(comments)!==savedText) throw new Error("Save current comments before opening a folder.");
    let loaded=[];
    try {loaded=validComments(JSON.parse(await (await (await chosen.getFileHandle("report.comments.json")).getFile()).text()));}
    catch(error) {if(error.name!=="NotFoundError") throw error;}
    directory=chosen; comments=loaded; savedText=JSON.stringify(comments); showComments(); status.textContent="Report folder opened; adjacent saving is enabled.";
  } catch(error) {status.textContent=error.message;}
});
document.getElementById("save-comments").addEventListener("click",async()=>{
  try {
    const payload=JSON.stringify({report:"weather-window-module",revision:currentRevision,comments},null,2)+"\n";
    if(directory) {
      const handle=await directory.getFileHandle("report.comments.json",{create:true});
      const writable=await handle.createWritable(); await writable.write(payload); await writable.close();
      savedText=JSON.stringify(comments); status.textContent="Comments saved beside report.html.";
    } else {
      const url=URL.createObjectURL(new Blob([payload],{type:"application/json"}));
      const link=document.createElement("a"); link.href=url; link.download="report.comments.json"; link.click();
      setTimeout(()=>URL.revokeObjectURL(url),1000); savedText=JSON.stringify(comments);
      status.textContent="Comment file downloaded. Place report.comments.json beside this HTML and load it when reopening.";
    }
  } catch(error) {status.textContent=error.message;}
});
window.addEventListener("beforeunload",event=>{
  if(JSON.stringify(comments)!==savedText) {event.preventDefault(); event.returnValue="";}
});
</script>'''


def build(output: Path) -> None:
    """Write one portable HTML artifact; leave all review annotations untouched."""
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(render(calculate()), encoding="utf-8", newline="\n")


def main() -> None:
    """Accept a scratch destination for a clean rebuild check."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=HERE / "report.html")
    args = parser.parse_args()
    build(args.output)


if __name__ == "__main__":
    main()
