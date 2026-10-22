# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the supplied-response fatigue report using the documented public example.

Use the source checkout on PYTHONPATH. Both output and calculation scratch
directories are explicit so rebuilding never writes into the example folder.
"""

from __future__ import annotations

import argparse
import copy
import csv
import hashlib
import html
import json
import math
import re
from pathlib import Path

import yaml

from digitalmodel.riser_fatigue import (
    RiserSection,
    TouchdownFatigueInput,
    assess_touchdown_fatigue,
)
from digitalmodel.riser_fatigue.workflow import SECONDS_PER_YEAR, router


ROOT = Path(__file__).resolve().parents[4]
EXAMPLE = ROOT / "examples/workflows/riser-fatigue/input.yml"
CHANGES = {
    "report": "Riser fatigue module analysis",
    "revision": "Initial",
    "date": "2026-10-21",
    "changes": [
        {
            "section": "Supplied-response fatigue screening",
            "change": "Wave and vortex-induced vibration damage, the governing location, "
                      "and the fatigue-factor criterion are presented for the public synthetic example.",
            "reason": "The calculation boundary and the evidence needed for design use are made explicit.",
        }
    ],
}
SOURCE_PATHS = [
    "examples/workflows/riser-fatigue/input.yml",
    "examples/workflows/riser-fatigue/README.md",
    "src/digitalmodel/riser_fatigue/__init__.py",
    "src/digitalmodel/riser_fatigue/workflow.py",
    "src/digitalmodel/riser_fatigue/touchdown.py",
    "src/digitalmodel/fatigue/sn_curves.py",
    "src/digitalmodel/fatigue/damage.py",
    "src/digitalmodel/fatigue/c203_sn_tables.py",
    "docs/api/_assets/brand.css",
    "docs/api/cfd/turbulent-flat-plate-verification.html",
    "docs/reports/modules/riserfatigue/generate_report.py",
]


def build_payload(scratch_dir: Path) -> dict:
    """Call the real router and touchdown API; retain their unrounded results."""
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    inputs = copy.deepcopy(cfg["riser_fatigue"])
    inputs.pop("output_dir")
    cfg["riser_fatigue"]["output_dir"] = str(scratch_dir.resolve())
    result = router(cfg)["riser_fatigue"]
    with Path(result["results_csv"]).open(encoding="utf-8") as stream:
        bins = []
        for row in csv.DictReader(stream):
            for key in row.keys() - {"segment_id", "contribution"}:
                value = float(row[key])
                row[key] = value if math.isfinite(value) else None
            bins.append(row)
    # The public touchdown API exposes the allowable damage explicitly.
    touchdown = []
    for segment in inputs["segments"]:
        wave = segment["wave"]
        inp = TouchdownFatigueInput(
            section=RiserSection(segment["outer_diameter_mm"], segment["wall_thickness_mm"]),
            stress_ranges_mpa=wave["stress_ranges_MPa"],
            cycles=wave["cycles"],
            sn_class=inputs["sn_curve"]["curve"],
            environment=inputs["sn_curve"]["environment"],
            scf=wave["scf"],
            design_life_years=inputs["design_life_years"],
            dff=inputs["dff"],
            histogram_period_years=wave["histogram_period_years"],
        )
        assessed = assess_touchdown_fatigue(inp)
        touchdown.append({
            "thickness_ref_mm": inp.thickness_ref_mm,
            "period_damage": assessed.period_damage,
            "annual_damage": assessed.annual_damage,
            "design_life_damage": assessed.design_life_damage,
            "allowable_damage": assessed.allowable_damage,
        })
    allowable = touchdown[0]["allowable_damage"]
    result = {k: v for k, v in result.items() if k not in {"results_csv", "summary_csv"}}
    payload = {
        "inputs": inputs,
        "result": result,
        "bins": bins,
        "touchdown": touchdown,
        "allowable_damage": allowable,
        "seconds_per_year": SECONDS_PER_YEAR,
        "chart": [
            {"component": seg["segment_id"], "demand": seg["total_damage"], "limit": allowable}
            for seg in result["segments"]
        ],
        "source_digests": {
            path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in SOURCE_PATHS
        },
    }
    return payload


def lookup(record: dict, key: str):
    """Resolve a numeric metric locator in the calculation record."""
    value = record
    for part in key.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    # YAML can retain scientific notation without an explicit exponent sign as
    # a string; the workflow itself converts these same numeric input fields.
    return float(value) if isinstance(value, str) else value


def format_number(value: float, spec: str) -> str:
    """Use engineering precision without printing small nonzero values as zero."""
    rendered = format(value, spec)
    if value != 0 and float(rendered.replace(",", "")) == 0:
        rendered = format(value, ".1e")
    return rendered.replace("-", "−")


def safe_json(value) -> str:
    """Embed data without allowing a string to end its script element."""
    return json.dumps(value, ensure_ascii=False, allow_nan=False, sort_keys=True).replace("<", "\\u003c")


def render(record: dict) -> str:
    """Write the complete engineering narrative and all figures in one template."""
    if record["result"]["screening_status"] != "pass":
        raise ValueError("The example verdict changed; an engineering narrative update is required")
    def metric(key, spec=".1f"):
        value = lookup(record, key)
        return (f'<span data-metric="{key}" data-value="{value}" data-format="{spec}">'
                f'{format_number(value, spec)}</span>')

    def cite(number):
        return f'<a href="#ref{number}">[{number}]</a>'

    def table(headers, rows, caption):
        return ('<div style="overflow-x:auto"><table><caption>' + caption + '</caption><thead><tr>'
                + ''.join(f'<th scope="col">{cell}</th>' for cell in headers)
                + '</tr></thead><tbody>'
                + ''.join('<tr>' + ''.join(f'<td>{cell}</td>' for cell in row) + '</tr>' for row in rows)
                + '</tbody></table></div>')

    labels = {record["inputs"]["segments"][0]["id"]: "Touchdown zone",
              record["inputs"]["segments"][1]["id"]: "Hang-off"}
    governing = labels[record["result"]["governing_segment"]]
    governing_index = next(index for index, segment in enumerate(record["result"]["segments"])
                           if segment["segment_id"] == record["result"]["governing_segment"])
    required_life_key = f"result.segments.{governing_index}.required_life_years"
    input_rows, result_rows, wave_rows, viv_rows, bin_rows = [], [], [], [], []
    summary_fields = ["wave_damage", "viv_damage", "total_damage", "fatigue_life_years",
                      "required_life_years", "dff_margin"]
    for index, segment in enumerate(record["inputs"]["segments"]):
        prefix = f"inputs.segments.{index}"
        input_rows.append([
            labels[segment["id"]], metric(prefix + ".outer_diameter_mm"),
            metric(prefix + ".wall_thickness_mm"), metric(prefix + ".wave.scf", ".2f"),
            metric(prefix + ".wave.histogram_period_years"), cite(1),
        ])
        for bin_index, _ in enumerate(segment["wave"]["cycles"]):
            wave_rows.append([
                labels[segment["id"]], metric(prefix + f".wave.stress_ranges_MPa.{bin_index}"),
                metric(prefix + f".wave.cycles.{bin_index}", ".1e"), cite(1),
            ])
        for case_index, case in enumerate(segment["viv_cases"]):
            base = prefix + f".viv_cases.{case_index}"
            hours = "exposure_hours_per_year" in case
            viv_rows.append([
                labels[segment["id"]], metric(base + ".stress_range_MPa"),
                metric(base + ".frequency_hz", ".2f"),
                "Not supplied" if hours else metric(base + ".exposure_fraction", ".3f"),
                metric(base + ".exposure_hours_per_year") if hours else "Not supplied", cite(1),
            ])
        summary = record["result"]["segments"][index]
        result_rows.append([
            labels[segment["id"]],
            *[metric(f"result.segments.{index}.{key}", ".1e" if "damage" in key else ".1f")
              for key in summary_fields],
            "The screening criterion is met." if summary["passes_dff"]
            else "The screening criterion is exceeded.", cite(2),
        ])
    for index, row in enumerate(record["bins"]):
        fields = ["bin_index", "stress_range_MPa", "stress_corrected_MPa", "frequency_hz",
                  "cycles", "allowable_cycles", "damage"]
        bin_rows.append([
            labels[row["segment_id"]], "Wave" if row["contribution"] == "wave" else "Vibration",
            *["Not applicable" if row[field] is None else metric(
                f"bins.{index}.{field}", ".0f" if field == "bin_index"
                else ".2f" if field == "frequency_hz"
                else ".1e" if field in {"cycles", "allowable_cycles", "damage"} else ".1f")
              for field in fields], cite(2),
        ])
    section_table = table(
        ["Location", "Outer diameter (mm)", "Wall (mm)", "Stress concentration factor (dimensionless)",
         "Histogram period (years)", "Source"], input_rows,
        "Table A. Example section properties and wave correction inputs.")
    results_table = table(
        ["Location", "Wave damage (dimensionless)", "Vibration damage (dimensionless)",
         "Total damage (dimensionless)", "Unfactored life (years)", "Required life (years)",
         "Fatigue-factor margin (dimensionless)", "Criterion", "Source"], result_rows,
        "Table B. Combined screening results from the module; each damage covers the design life.")
    wave_table = table(["Location", "Stress range (MPa)", "Cycles per period", "Source"], wave_rows,
                       "Table C. Supplied wave histograms; no structural response is calculated.")
    viv_table = table(["Location", "Stress range (MPa)", "Frequency (Hz)",
                       "Exposure fraction (dimensionless)", "Exposure (hours per year)", "Source"],
                      viv_rows, "Table D. Supplied narrow-band vibration cases.")
    bins_table = table(
        ["Location", "Contribution", "Bin index", "Supplied range (MPa)", "Evaluated range (MPa)",
         "Frequency (Hz)", "Design-life cycles", "Allowable cycles", "Damage (dimensionless)", "Source"],
        bin_rows, "Table E. Complete per-bin output. Bin indices retain the module's zero-based convention.")
    # Reuse the report suite's exact existing CSS rather than designing a new theme.
    canonical = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    report_style = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    limit = record["allowable_damage"]
    ymax = max(limit, *(row["demand"] for row in record["chart"])) * 1.25

    def ypos(value):
        return 290 - 240 * value / ymax

    bars = []
    for index, row in enumerate(record["chart"]):
        label = labels[row["component"]]
        text = (f'Case: public synthetic example; component: {label}; demand: '
                f'{format_number(row["demand"], ".1e")}; limit: {format_number(row["limit"], ".1e")}; '
                'units: dimensionless design-life damage.')
        bars.append(
            f'<g class="damage-point" tabindex="0" role="img" aria-label="{text}" '
            f'data-description="{text}"><title>{text}</title>'
            f'<rect x="{160 + index * 250}" y="{ypos(row["demand"]):.2f}" width="90" '
            f'height="{290 - ypos(row["demand"]):.2f}" fill="var(--brand)"/>'
            f'<text x="{205 + index * 250}" y="320" text-anchor="middle">{label}</text></g>')
    chart = (
        '<figure id="governing-chart"><svg viewBox="0 0 700 355" style="width:100%;height:auto" role="img" '
        'aria-label="Design-life fatigue damage by location against the allowable damage">'
        '<text x="25" y="25">Design-life damage (dimensionless)</text>'
        '<path d="M80 45 V290 H660" fill="none" stroke="currentColor"/>'
        '<text x="65" y="295">0</text>'
        f'<line id="damage-limit" x1="80" x2="660" y1="{ypos(limit):.2f}" '
        f'y2="{ypos(limit):.2f}" stroke="var(--nogo)" stroke-width="2" stroke-dasharray="8 5"/>'
        f'<text x="90" y="{ypos(limit) - 10:.2f}">Allowable design-life damage</text>'
        + ''.join(bars) + '</svg><figcaption>Figure A. Supplied-response fatigue screening. '
        'The dashed line is the allowable damage from the explicit example fatigue factor. '
        'Hover over or focus on a location for demand, limit and units. '
        + cite(1) + cite(2) + '</figcaption><p id="chart-readout" role="status">'
        'The chart compares total fatigue damage with its allowable value.</p></figure>'
    )
    document_key = hashlib.sha256(safe_json(record).encode()).hexdigest()
    changes_rows = [[html.escape(item["section"]), html.escape(item["change"]), html.escape(item["reason"])]
                    for item in CHANGES["changes"]]
    changes_table = table(["Subject", "Engineering change", "Reason"], changes_rows,
                          "Engineering changes since the previous issue.")
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light">
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Riser fatigue module analysis</title>
<style>{canonical}</style>
<style>{report_style}</style>
</head><body class="hs-doc"><main class="wrap">
<header><div class="kicker">Engineering module analysis</div>
<h1>Riser fatigue module analysis</h1><p class="sub">Draft for client review</p></header>
<nav aria-label="Report sections"><a href="#s1">Summary</a> · <a href="#s3">Design data</a> ·
<a href="#s4">Method</a> · <a href="#s5">Results</a> · <a href="#s8">Recommendations</a> ·
<a href="#appendix">Appendices</a> · <a href="#comments">Comments</a></nav>

<section id="s1"><h2>1. Executive Summary</h2>
<p>The riser fatigue module is a supplied-response fatigue screening calculation for steel riser locations.
Analysis is performed to determine combined wave and vortex-induced vibration fatigue damage,
and the key results are summarized below. {cite(1)}{cite(2)}</p>
<ul><li>The public synthetic example is assessed against its explicit design fatigue factor.
<ul><li>The governing location is the {governing.lower()}, with design-life damage
{metric("result.governing_damage", ".1e")} against allowable damage
{metric("allowable_damage", ".1e")}; the screening criterion is met. {cite(1)}{cite(2)}</li>
<li>The governing unfactored fatigue life is {metric("result.governing_fatigue_life_years")} years
against a required life of {metric(required_life_key)} years;
this is a screening estimate from supplied stresses. {cite(2)}</li></ul></li>
<li>The evidence supports checking damage arithmetic and locating the governing supplied response.
<ul><li>A connected operating envelope and whether a riser stands remain NOT_EVALUATED;
host motion, loads, stack-up and structural response are not established by this calculation. {cite(2)}</li>
<li>Design acceptance requires a qualified stress basis and an independent published benchmark;
neither is established by the synthetic example or the existing regression checks. {cite(3)}</li></ul></li></ul>
{chart}
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>Initial</td><td>{CHANGES["date"]}</td><td>Supplied-response fatigue screening is documented
to distinguish damage arithmetic from riser response and design qualification.</td></tr></tbody></table>
</section>

<section id="s2"><h2>2. Introduction</h2>
<p>A steel catenary or steel lazy-wave riser is represented by supplied stress ranges at selected locations.
Riser fatigue analysis is performed after stresses and cycle exposure have been determined upstream.
Water depth and a site-specific configuration are not defined by this example. {cite(1)}{cite(2)}</p>
<p>The main workflow accumulates wave-histogram damage and narrow-band vortex-induced vibration damage,
selects the largest total damage and returns a screening status. The separate touchdown assessment
accepts a histogram and returns corrected ranges, per-bin damage, annual damage and fatigue life.
A text-report helper is also available. {cite(2)}</p>
</section>

<section id="s3"><h2>3. Design Data</h2>
<p>The public repository example is used as the calculation basis. Its section properties,
stress histograms and vibration exposure are synthetic inputs rather than measurements. {cite(1)}</p>
<h3>3.1 Field, host and riser</h3>
<p>Field, water depth, host class, response amplitude operators, offsets and a dimensional stack-up
are not supplied. The schematic shows physical locations only; curvature, component length,
soil restraint and support stiffness are not determined. {cite(1)}{cite(2)}</p>
<figure id="physical-system"><svg viewBox="0 0 820 340" style="width:100%;height:auto" role="img"
aria-label="Conceptual steel catenary riser with hang-off and touchdown assessment locations">
<path d="M30 95 H790 M30 295 H790" stroke="currentColor" fill="none"/>
<text x="35" y="82">Water surface</text><text x="35" y="320">Seabed; soil response not supplied</text>
<path d="M120 50 H245 L230 90 H135 Z" fill="var(--bg)" stroke="currentColor"/>
<text x="260" y="65">Host; motions and support stiffness not supplied</text>
<path d="M190 90 C200 240 300 295 490 295 H750" stroke="var(--brand)" stroke-width="7" fill="none"/>
<circle cx="190" cy="102" r="8" fill="var(--accent)"/>
<circle cx="485" cy="294" r="8" fill="var(--accent)"/>
<path d="M195 110 L300 140 M485 285 L555 220" stroke="currentColor" fill="none"/>
<text x="305" y="145">Hang-off assessment location</text>
<text x="500" y="205">Touchdown assessment location</text>
<text x="45" y="220">Span and water depth: not supplied</text>
</svg><figcaption>Figure B. Physical interpretation of the example. Schematic only;
no structural mesh or boundary-condition solution is included. {cite(1)}{cite(2)}</figcaption></figure>
<p>Section diameters and wall thicknesses are retained in millimetres because the fatigue interface
uses millimetres and megapascal stress ranges. The outer diameter and material label are informational
in this workflow; wall thickness affects only the wave thickness correction. No stress is derived
from pressure, curvature or material strength. {cite(2)}</p>
<h3>3.2 Contents, metocean and supplied responses</h3>
<p>Contents density, pressure, current profiles, waves and host-motion histories are not held.
The supplied histograms replace the upstream response calculation; there is no current-dominated
or wave-dominated operating envelope. Tables A, C and D in the appendix hold all numeric example inputs.
{cite(1)}{cite(2)}</p>
<h3>3.3 Criteria and rationale</h3>
<p>The design life is {metric("inputs.design_life_years")} years and the explicit example design fatigue factor
is {metric("inputs.dff")}, giving allowable design-life damage {metric("allowable_damage", ".1e")}.
These are example choices, marked ASSUMED - to be confirmed for any design application.
The factor reduces permissible accumulated damage to reserve fatigue life; it is not a calculated
material property or a demonstrated site-specific requirement. {cite(1)}{cite(2)}</p>
<p>The selected stress–life detail category is {html.escape(record["inputs"]["sn_curve"]["curve"])}
in seawater with cathodic protection. This is an example selection, not a verified weld classification.
The implemented acceptance criterion is design-life damage at or below the inverse of the fatigue factor.
The same condition is expressed as unfactored life at or above required life. {cite(1)}{cite(2)}</p>
<p>The load-case matrix consists of supplied wave bins and narrow-band vibration cases at each location.
Their exposure and stress ranges are stated in the appendix; no sea-state probabilities or host-motion
cases are inferred from them. {cite(1)}</p>
</section>

<section id="s4"><h2>4. Analysis Methodology</h2>
<p>Fatigue damage is calculated from supplied stresses through the shared stress–life and linear
damage engine. Geometry and loading are not solved by the fatigue module. {cite(2)}{cite(4)}</p>
<h3>4.1 Calculation boundary and theory</h3>
<figure><svg viewBox="0 0 820 165" style="width:100%;height:auto" role="img" aria-label="Calculation boundary from supplied responses to fatigue damage">
<rect x="10" y="25" width="235" height="105" fill="var(--bg)" stroke="currentColor"/>
<text x="20" y="55">Upstream: geometry, supports,</text><text x="20" y="78">tension, soil and host motions</text>
<text x="20" y="105">Structural response not evaluated</text>
<path d="M245 75 H285 M535 75 H575" stroke="currentColor" fill="none"/>
<rect x="285" y="25" width="250" height="105" fill="var(--bg)" stroke="currentColor"/>
<text x="295" y="55">Input: wave stress histogram;</text><text x="295" y="78">vibration range and exposure</text>
<text x="295" y="105">Input qualification required</text>
<rect x="575" y="25" width="235" height="105" fill="var(--bg)" stroke="currentColor"/>
<text x="585" y="55">Fatigue: corrections, cycle</text><text x="585" y="78">capacity, damage and life</text>
<text x="585" y="105">No mesh or support solver</text>
</svg><figcaption>Figure C. Model boundary. Centralisers, lateral supports, tensioners,
buoyancy and stops belong to the upstream structural model and are not inputs to this fatigue calculation.
{cite(2)}</figcaption></figure>
<div class="eq">S<sub>c</sub> = S K<sub>s</sub> max(1, t / t<sub>r</sub>)<sup>k</sup></div>
<p>For wave bins, S is the supplied stress range, K<sub>s</sub> is the stress concentration factor,
t is wall thickness, t<sub>r</sub> is reference thickness and k is the class-specific thickness exponent.
S<sub>c</sub> is the evaluated stress range. The correction is neutral below reference thickness.
The reference thickness used by the touchdown interface is
{metric("touchdown.0.thickness_ref_mm")} mm. The exponent is selected from the repository tables
for the detail category unless explicitly supplied to the touchdown interface. {cite(2)}{cite(4)}</p>
<div class="eq">N(S<sub>c</sub>) = N<sub>k</sub> (S<sub>c</sub> / S<sub>k</sub>)<sup>−m</sup>;
D<sub>p</sub> = Σ n<sub>j</sub> / N<sub>j</sub></div>
<p>N is allowable cycles, N<sub>k</sub> is knee cycles, S<sub>k</sub> is knee stress and m is the active
branch slope. The shared curve implementation anchors both branches at the knee; the second branch
is continuous there rather than independently anchored to a rounded tabular intercept.
n<sub>j</sub> is applied cycles in bin j, N<sub>j</sub> is its capacity and D<sub>p</sub> is damage
over the histogram period. Standard coefficients are consumed from the implementation and are not
reproduced here as independently checked standard values. {cite(4)}</p>
<div class="eq">D<sub>a</sub> = D<sub>p</sub> / H;
D<sub>w</sub> = T D<sub>a</sub>;
n<sub>v</sub> = f e Y T;
D = D<sub>w</sub> + D<sub>v</sub></div>
<p>H is histogram period in years, T is design life in years, D<sub>a</sub> is annual wave damage,
D<sub>w</sub> and D<sub>v</sub> are wave and vibration damage over the same design life,
f is vibration frequency in hertz, e is the annual exposure fraction, Y is seconds per year,
and n<sub>v</sub> is vibration cycles. The implementation uses
{metric("seconds_per_year", ".1e")} seconds per year as a calendar conversion, not an environmental exposure.
Exposure given in hours is converted to a year fraction by the same convention. {cite(2)}</p>
<div class="eq">L = T / D;
D<sub>allow</sub> = 1 / F;
U = D F;
M = L / (T F)</div>
<p>L is unfactored life, F is the design fatigue factor, D<sub>allow</sub> is allowable damage,
U is fatigue usage and M is the fatigue-factor life margin. The criterion is U ≤ 1,
equivalently M ≥ 1, for positive damage. The inverse and unity thresholds are definitions of
the implemented screening check, not independently selected environmental limits. {cite(2)}</p>
<h3>4.2 Structural benchmark and dynamic basis</h3>
<p>A static hand calculation followed by still-water, current, then wave-and-current response is
required upstream for a physical riser analysis. That sequence is not performed here.
Effective tension, wall tension, foam extent, base uplift and host response amplitude operators
are NOT_EVALUATED because the corresponding data and solver interface are absent from this example.
The supplied-stress fatigue calculation is a screening estimate; it is not a result on either
a regular-wave proxy or response-operator-driven host motion. {cite(1)}{cite(2)}</p>
<p>Wave stress ranges receive the concentration and thickness corrections. Vibration stress ranges
are passed directly to the stress–life engine. Vibration inputs therefore require a documented basis
stating whether all needed local corrections are already included. {cite(2)}</p>
</section>

<section id="s5"><h2>5. Detailed Results</h2>
<p>The combined fatigue screening results are returned by the module for the public synthetic example.
Only supplied-response fatigue is evaluated; results are not established for the upstream structural stages.
{cite(1)}{cite(2)}</p>
<h3>5.1 Statics, current and operating envelopes</h3>
<p>Statics against a hand calculation, statics with current and connected operating envelopes are
NOT_EVALUATED. Geometry, weight, buoyancy, pressure, tension and motion inputs are required.
No allowable offset is established. {cite(1)}{cite(2)}</p>
<h3>5.2 Dynamics, drift-off, recoil and weak point</h3>
<p>Dynamic stroke, tension and angles are NOT_EVALUATED. Stop strikes and travel limits are not assessed.
Drift-off in the 1-year current and in the 10-year current is NOT_EVALUATED; current histories,
a watch circle and a drift model are required. Time available is not established. Without a watch circle,
do not connect. Recoil is not evaluated: data needed: closure curves. Weak point analysis is not performed.
These statements describe absent assessments, not fatigue failures. {cite(1)}{cite(2)}</p>
<h3>5.3 Vortex-induced vibration and fatigue</h3>
<p>The {governing.lower()} governs with wave damage
{metric("result.governing_wave_damage", ".1e")} and vibration damage
{metric("result.governing_viv_damage", ".1e")}, giving total design-life damage
{metric("result.governing_damage", ".1e")} against allowable damage
{metric("allowable_damage", ".1e")}. The screening criterion is met. {cite(2)}</p>
<p>The governing unfactored life is {metric("result.governing_fatigue_life_years")} years
and its fatigue-factor margin is {metric("result.governing_dff_margin")} against the unity minimum.
These are screening estimates from supplied stresses, not predictions of actual service life.
Table B gives each location and Table E gives every damage contribution. {cite(2)}</p>
<h3>5.4 Validation evidence held</h3>
<p>Existing tests check a single-bin hand expression, fatigue-factor verdict changes, thickness effects,
detail-category and environment selection, lowercase-category normalization, report fields,
unequal input lengths, explicit and cited fatigue factors, and combination of wave and vibration damage.
The documented example is also exercised through the public engine. {cite(3)}</p>
<p>The single-bin test checks allowable cycles and damage against an expression using the repository's
encoded parameters. This is useful arithmetic regression evidence, but the constants share the
implementation's source basis. No independent published benchmark or measured riser response is
established by these checks. Test execution evidence is stated separately from engineering qualification
in the appendix. {cite(3)}{cite(4)}</p>
</section>

<section id="s6"><h2>6. Key Findings</h2>
<p>The governing supplied response is identified and its combined damage is below the explicit example
limit. The conclusion is confined to the provided stress and exposure basis. {cite(1)}{cite(2)}</p>
<p>The governing location is the {governing.lower()}, with design-life damage
{metric("result.governing_damage", ".1e")} against allowable damage
{metric("allowable_damage", ".1e")}; the screening criterion is met. The governing unfactored fatigue life is
{metric("result.governing_fatigue_life_years")} years against a required life of
{metric(required_life_key)} years; this is a screening estimate from supplied stresses.
Whether a production riser stands and how far a host remains connected are NOT_EVALUATED. {cite(2)}</p>
</section>

<section id="s7"><h2>7. Limitations</h2>
<p>The calculation is limited to damage accumulation from supplied responses. Qualification of geometry,
weld detail, exposure and loading remains outside the module's demonstrated evidence. {cite(1)}{cite(2)}</p>
<p>The standards basis in the source is mixed: the assessment metadata names the 2021 edition of
DNV-RP-C203, while the shared numeric table provenance and class thickness lookup cite the 2011 edition.
The edition mapping is documented in the repository, but no licensed clause text is checked here.
The fatigue factor in this example is explicit; its selection is not a standard-derived requirement
for an actual riser. {cite(2)}{cite(4)}{cite(5)}</p>
<p>The direct histogram interface checks matching array shapes and positive histogram period and fatigue
factor, but a complete contract for finite, nonnegative, nonempty inputs and valid geometry is not enforced
locally. All external input data require qualification before interpretation of the screening status.
The material designation is informational. {cite(2)}</p>
<h3>7.1 Not covered</h3>
<p>Stress extraction, cycle counting from histories, vortex-induced response prediction, spectral response,
host motion, soil interaction, installation fatigue, corrosion progression and crack growth are not covered.
The text-report helper and fatigue-factor citation resolution are described as interfaces, not used as
substitutes for independent engineering evidence. An omitted fatigue factor in the combined workflow
requires a resolvable citation context; the direct touchdown interface has a default instead. {cite(2)}</p>
</section>

<section id="s8"><h2>8. Recommendations</h2>
<p>Design use is conditional on resolving the input basis and standards applicability. The arithmetic
screening result alone is insufficient to establish operational acceptance. {cite(2)}{cite(3)}{cite(4)}</p>
<ul><li>Establish a dimensioned stack-up and a static hand benchmark, then supply qualified current,
wave and host-motion stress histories with support, stop and soil models before setting an operating envelope.
The module accepts only the resulting stress basis. {cite(2)}</li>
<li>Document stress-range units, cycle-period basis, local corrections, weld category, environment and
fatigue-factor selection. Confirm that vibration ranges include the corrections omitted by their adapter.
The synthetic example is retained only as a regression case. {cite(1)}{cite(2)}</li>
<li>Reconcile the governing standard edition and clause applicability against an authorized source, then
establish an independent published or measured benchmark for the intended application. Existing tests
remain arithmetic regression evidence. {cite(3)}{cite(4)}{cite(5)}</li>
<li>For any future result exceeding allowable design-life damage, withhold acceptance and reduce the
qualified stress or exposure basis through an assessed design or operating change. Recalculate the combined
damage until it satisfies the same fatigue-factor criterion; no generic stress-reduction percentage or
inspection interval is inferred here. {cite(2)}</li></ul>
</section>

<section id="s9"><h2>9. References</h2>
<p>The calculation basis is identified by the public example and the repository implementation.
Standard titles and editions are recorded without reproducing licensed tables. {cite(1)}{cite(4)}{cite(5)}</p>
<h3>9.1 Standards basis and evidence status</h3>
<p>DNV-RP-C203, Fatigue Design of Offshore Steel Structures, 2011 and 2021 editions:
stress–life curves, thickness effects and cumulative damage. The repository identifies the 2011
Tables 2-1 to 2-3 as its numeric provenance and maps the 2021 table numbering separately.
DNV-OS-F201, Dynamic Risers: design fatigue factors. The applicable edition and clause requirements
for a physical design remain required inputs; they are not established by this report. {cite(4)}{cite(5)}</p>
<h3>9.2 Public source locators</h3>
<ol class="refs">
<li id="ref1">Public synthetic example and documented invocation:
<code>examples/workflows/riser-fatigue/input.yml</code> and
<code>examples/workflows/riser-fatigue/README.md</code>.
The values are example inputs, not an independent published engineering benchmark.</li>
<li id="ref2">Complete public calculation interfaces:
<code>src/digitalmodel/riser_fatigue/__init__.py</code>,
<code>workflow.py: router, resolve_dff, _wave_damage, _viv_damage</code> and
<code>touchdown.py: RiserSection, TouchdownFatigueInput, assess_touchdown_fatigue, report_markdown</code>.
Stress corrections, period scaling, calendar conversion and acceptance definitions are implementation evidence.</li>
<li id="ref3">Existing regression evidence:
<code>tests/riser_fatigue/test_touchdown.py</code>,
<code>test_resolve_dff.py</code> and <code>test_riser_fatigue_workflow.py</code>.
The single-bin hand expression and synthetic examples are not independent physical benchmarks.</li>
<li id="ref4">Shared calculation engine and encoded table provenance:
<code>src/digitalmodel/fatigue/damage.py</code>, <code>sn_curves.py</code>,
<code>c203_sn_tables.py</code> and <code>c203_editions.py</code>.
The stress–life backend is pyLife; both branches are anchored at the knee.</li>
<li id="ref5">Fatigue-factor citation interface:
<code>src/digitalmodel/riser_database/getters.py: get_riser_dff</code>.
The default-factor getter names the design fatigue factors section; clause applicability is not qualified here.</li>
<li id="ref6">Report reproduction:
<code>docs/reports/modules/riserfatigue/generate_report.py</code> and
<code>tests/riser_fatigue/test_module_report.py</code>.
The generator calls the public router and touchdown interface and reads their numeric outputs.</li>
<li id="ref7">Report layout and stylesheet:
<code>docs/api/cfd/turbulent-flat-plate-verification.html</code> and
<code>docs/api/_assets/brand.css</code>. Their existing styles are inlined in this report.</li>
</ol></section>

<section id="s10"><h2>10. Acronyms and Symbols</h2>
<p>The abbreviations and symbols used in the calculation are defined below.
All damage, usage, exposure-fraction and margin quantities are dimensionless. {cite(2)}{cite(4)}</p>
<p>VIV means vortex-induced vibration. SCF means stress concentration factor. DFF means design fatigue factor.
S–N means stress range versus allowable cycle count. RAO means response amplitude operator.
MPa means megapascal; mm means millimetre; Hz means cycles per second.
DNV identifies the standards publisher. API is the material-specification publisher named in the example;
the grade label is not used to calculate stress–life capacity. QA means quality assurance.</p>
<p>S is supplied stress range; S<sub>c</sub> is evaluated range; K<sub>s</sub> is the concentration factor;
t and t<sub>r</sub> are wall and reference thickness; k is the thickness exponent;
N and n are allowable and applied cycles; subscript j identifies a bin; subscript k identifies a knee;
m is stress–life slope; H and T are histogram period and design life; f is vibration frequency;
e is exposure fraction; Y is seconds per year; D is combined design-life damage;
subscripts p, a, w and v identify period, annual, wave and vibration quantities;
L is unfactored life; F is the fatigue factor; U is usage; M is fatigue-factor margin.
NOT_EVALUATED means that the required assessment has not been performed, rather than a zero demand.</p>
</section>

<aside id="appendix"><h2>Appendices</h2>
<p>The complete calculation data and evidence boundaries are retained for engineering checks.
The main text presents only the governing supplied-response conclusion. {cite(1)}{cite(2)}{cite(6)}</p>
<h3>A. Input and data register</h3>
{section_table}{wave_table}{viv_table}
<table><caption>Table F. Data availability and required confirmation.</caption>
<thead><tr><th>Data</th><th>Status</th><th>Required disposition</th><th>Source</th></tr></thead><tbody>
<tr><td>Histograms, vibration ranges, frequency and exposure</td><td>ASSUMED - to be confirmed</td>
<td>Replace synthetic example values with qualified response data for design use.</td><td>{cite(1)}</td></tr>
<tr><td>Section sizes, weld category, concentration factor, design life and fatigue factor</td>
<td>ASSUMED - to be confirmed</td><td>Confirm geometry, weld classification and governing design basis.</td><td>{cite(1)}{cite(2)}</td></tr>
<tr><td>Water depth, stack-up lengths, relative component stiffness, contents and pressure</td><td>Not supplied</td>
<td>Establish the structural model and its static benchmark.</td><td>{cite(1)}</td></tr>
<tr><td>Adjacent component diameters, added mass, soil, supports, travel stops and host motion</td><td>Not supplied</td>
<td>Establish the dynamic stress basis; no structural values are inferred.</td><td>{cite(1)}{cite(2)}</td></tr>
<tr><td>Standard edition, clause applicability and independent benchmark</td><td>Not established</td>
<td>Qualify before interpreting the screening status as a design result.</td><td>{cite(3)}{cite(4)}{cite(5)}</td></tr>
</tbody></table>
<h3>B. Complete fatigue results and output contract</h3>
{results_table}{bins_table}
<p>The combined interface returns governing and per-location damage, unfactored life, required life,
fatigue-factor margin and screening status. It also writes a per-bin comma-separated file and a
per-location summary file. The touchdown interface returns a result object and a text-report helper
returns a string. A missing wave frequency in its bin output is not an evaluated vibration frequency.
Numeric data behind the figures are embedded without rounding; displayed values use engineering precision.
{cite(2)}{cite(6)}</p>
<h3>C. Calculation reproduction and QA pre-flight</h3>
<p>The report is rebuilt by calling the documented combined workflow on the public example with only its
output destination redirected. The touchdown interface is called separately on the same wave inputs
to obtain the allowable damage and period basis. The rebuild test compares all numeric output rows
and per-location results against direct module calls, checks every displayed numeric field against the
embedded record, and checks deterministic reproduction of this HTML and its change record. {cite(6)}</p>
<p>The chart plots total design-life damage by location with the allowable damage returned by the
touchdown interface. Its ordinate starts at zero and its upper extent includes both demands and the limit.
No operating boundary, stress limit or service-life probability is inferred from the chart. {cite(2)}{cite(6)}</p>
<p>The structural QA pre-flight is incomplete: no still-water hand benchmark, effective-tension comparison,
current-response benchmark, mesh convergence or host-motion check is available in this example.
Existing arithmetic tests and report rebuild tests are distinct from those missing engineering checks.
No independent published benchmark is established. {cite(1)}{cite(3)}{cite(6)}</p>
</aside>

<aside id="changes"><h2>Engineering changes</h2>{changes_table}</aside>
<aside id="comments"><h2>Report comments</h2>
<p>Comments are associated with this report revision. Save the comments file beside the HTML where the
browser supports a file picker. Otherwise a download is provided; place it beside the HTML and load it
to check the saved copy. Existing comments are retained when a matching file is loaded.</p>
<label>Section <select id="comment-section"></select></label>
<label>Comment <textarea id="comment-text" rows="4" maxlength="10000"></textarea></label>
<button id="comment-add" type="button">Add comment</button>
<button id="comment-save" type="button">Save comments beside report</button>
<label>Load saved comments <input id="comment-load" type="file" accept=".json"></label>
<p id="comment-status" role="status">Comments are held in this page until saved.</p>
<ol id="comment-list"></ol></aside>
<footer>Supplied-response screening only. Independent design qualification remains required.</footer>
</main>
<script type="application/json" id="calculation-record">{safe_json(record)}</script>
<script type="application/json" id="changes-record">{safe_json(CHANGES)}</script>
<script type="application/json" id="comment-config">{safe_json({"document_key": document_key, "revision": CHANGES["revision"]})}</script>
<script>{INTERACTIONS}</script>
</body></html>
'''


INTERACTIONS = r"""
(() => {
  'use strict';
  const el = id => document.getElementById(id);
  const config = JSON.parse(el('comment-config').textContent);
  const sections = [...document.querySelectorAll('section[id]')];
  const validSections = new Set(sections.map(section => section.id));
  let comments = [];
  for (const section of sections) {
    const option = document.createElement('option');
    option.value = section.id;
    option.textContent = section.querySelector('h2').textContent;
    el('comment-section').append(option);
  }
  function drawComments() {
    el('comment-list').replaceChildren();
    for (const comment of comments) {
      const item = document.createElement('li');
      item.textContent = `${comment.section}: ${comment.text}`;
      el('comment-list').append(item);
    }
  }
  el('comment-add').addEventListener('click', () => {
    const text = el('comment-text').value.trim();
    if (!text) {
      el('comment-status').textContent = 'Enter a comment before adding it.';
      return;
    }
    comments.push({section: el('comment-section').value, text});
    el('comment-text').value = '';
    drawComments();
    el('comment-status').textContent = 'Comment added. Save comments to retain them.';
  });
  el('comment-load').addEventListener('change', async event => {
    try {
      const file = event.target.files[0];
      if (!file) return;
      const incoming = JSON.parse(await file.text());
      if (incoming.document_key !== config.document_key || incoming.revision !== config.revision ||
          !Array.isArray(incoming.comments) || incoming.comments.some(comment =>
            !comment || !validSections.has(comment.section) || typeof comment.text !== 'string' ||
            !comment.text.trim() || comment.text.length > 10000)) {
        throw new Error('The file does not match this report or its comment format.');
      }
      for (const comment of incoming.comments) {
        if (!comments.some(existing => existing.section === comment.section && existing.text === comment.text)) {
          comments.push({section: comment.section, text: comment.text});
        }
      }
      drawComments();
      el('comment-status').textContent = 'Matching comments loaded; existing comments retained.';
    } catch (error) {
      el('comment-status').textContent = `Comments not loaded: ${error.message}`;
    }
  });
  el('comment-save').addEventListener('click', async () => {
    const data = JSON.stringify({...config, comments}, null, 2) + '\n';
    try {
      if (typeof window.showSaveFilePicker === 'function') {
        const handle = await window.showSaveFilePicker({suggestedName: 'report.comments.json',
          types: [{description: 'Report comments', accept: {'application/json': ['.json']}}]});
        const stream = await handle.createWritable();
        await stream.write(data);
        await stream.close();
        el('comment-status').textContent = 'Comments saved. Load the saved file to check the copy.';
      } else {
        const url = URL.createObjectURL(new Blob([data], {type: 'application/json'}));
        const link = document.createElement('a');
        link.href = url;
        link.download = 'report.comments.json';
        link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        el('comment-status').textContent = 'Download requested. Place it beside the HTML and load it to check the copy.';
      }
    } catch (error) {
      el('comment-status').textContent = error.name === 'AbortError'
        ? 'Save cancelled. Comments remain in this page.' : `Comments not saved: ${error.message}`;
    }
  });
  for (const point of document.querySelectorAll('.damage-point')) {
    const show = () => { el('chart-readout').textContent = point.dataset.description; };
    point.addEventListener('pointerenter', show);
    point.addEventListener('focus', show);
  }
})();
"""


def main() -> None:
    """Write the report and change panel; direct calculation files to scratch."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--scratch-dir", type=Path, required=True)
    args = parser.parse_args()
    record = build_payload(args.scratch_dir)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "report.html").write_text(render(record), encoding="utf-8")
    (args.output_dir / "changes.json").write_text(
        json.dumps(CHANGES, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )


if __name__ == "__main__":
    main()
