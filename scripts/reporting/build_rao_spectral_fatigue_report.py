# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the offline module report by evaluating the published example.

Use the repository Python environment with PYTHONPATH=src and supply
--scratch-dir, or export TMPDIR. Numerical CSV files remain in scratch storage.
The source example and calculation implementation are never modified.
"""

from __future__ import annotations

import argparse
import copy
import csv
import hashlib
import html
import json
import os
import re
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any

import yaml

from digitalmodel.rao_spectral_fatigue.workflow import router, _stress_psd_from_rao

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = ROOT / "docs/reports/modules/raospectralfatigue"
EXAMPLE = "examples/workflows/rao-spectral-fatigue/input.yml"
STYLE = "docs/api/cfd/turbulent-flat-plate-verification.html"
BRAND = "docs/api/_assets/brand.css"
UI = "scripts/reporting/rao_spectral_fatigue_ui.js"
SOURCES = [
    EXAMPLE, STYLE, BRAND, UI,
    "scripts/reporting/build_rao_spectral_fatigue_report.py",
    "src/digitalmodel/rao_spectral_fatigue/__init__.py",
    "src/digitalmodel/rao_spectral_fatigue/workflow.py",
    "src/digitalmodel/spectral_fatigue/workflow.py",
    "src/digitalmodel/fatigue/spectral_fatigue.py",
    "src/digitalmodel/hydrodynamics/wave_spectra.py",
    "tests/workflows/test_durable_workflows.py",
    "tests/fatigue/test_spectral_fatigue.py",
]
CHANGES = {
    "report": "rao-spectral-fatigue", "revision": "A", "date": "2026-10-22",
    "changes": [
        {"section": "Purpose and basis", "description":
         "Initial assessment of the linear wave-to-stress fatigue calculation and its input requirements."},
        {"section": "Detailed results", "description":
         "Published illustrative inputs are evaluated with the module; life criteria and unresolved calculation limits are stated."},
        {"section": "Recommendations", "description":
         "Engineering use is conditioned on corrected fatigue estimation, qualified stress transfers and an independent benchmark."},
    ],
}
LABELS = {"riser-splash-zone": "Splash-zone detail", "riser-keel-joint": "Keel-joint detail"}


def encode(value: Any) -> str:
    """Serialize finite report data without permitting a closing script tag."""
    return json.dumps(value, ensure_ascii=False, sort_keys=True,
                      separators=(",", ":"), allow_nan=False).replace("<", "\\u003c")


def evaluate(scratch_dir: Path) -> dict[str, Any]:
    """Call the documented router and retain only portable numerical evidence."""
    original = yaml.safe_load((ROOT / EXAMPLE).read_text(encoding="utf-8"))
    cfg = copy.deepcopy(original)
    scratch_dir.mkdir(parents=True, exist_ok=True)
    with TemporaryDirectory(prefix="rao-report-", dir=scratch_dir) as temporary:
        cfg["rao_spectral_fatigue"]["output_dir"] = temporary
        result = router(cfg)["rao_spectral_fatigue"]
        with Path(result["results_csv"]).open(encoding="utf-8") as stream:
            rows = list(csv.DictReader(stream))
        with Path(result["summary_csv"]).open(encoding="utf-8") as stream:
            csv_summaries = list(csv.DictReader(stream))
        locations = []
        for source, summary, csv_summary in zip(
            original["rao_spectral_fatigue"]["locations"], result["locations"],
            csv_summaries, strict=True,
        ):
            location = {key: summary[key] for key in (
                "annual_damage", "fatigue_life_years", "required_life_years", "margin", "passes")}
            for key in ("annual_damage", "fatigue_life_years", "required_life_years", "margin"):
                if float(csv_summary[key]) != location[key]:
                    raise ValueError("Summary CSV differs from the returned calculation")
            location["label"] = LABELS[source["id"]]
            location["rao_frequency_Hz"] = source["rao_frequency_Hz"]
            location["rao_stress_MPa_per_m"] = source["rao_stress_MPa_per_m"]
            location["sea_states"] = []
            detail_rows = [row for row in rows if row["location_id"] == source["id"]]
            for index, (sea, row) in enumerate(zip(source["sea_states"], detail_rows, strict=True)):
                frequency, psd = _stress_psd_from_rao(
                    source["rao_frequency_Hz"], source["rao_stress_MPa_per_m"],
                    sea["wave_spectrum"], source["id"],
                )
                state = dict(sea["wave_spectrum"])
                state.update({"label": ("First sea state", "Second sea state")[index],
                              "occurrence_fraction": sea["occurrence_fraction"],
                              "frequency_Hz": frequency, "stress_psd_MPa2_Hz": psd})
                for key in ("rms_stress_MPa", "bandwidth_parameter", "peak_rate_Hz",
                            "damage_per_year_full", "damage_per_year_weighted"):
                    state[key] = float(row[key])
                location["sea_states"].append(state)
            locations.append(location)
    return {"design_life_years": result["design_life_years"], "dff": result["dff"],
            "locations": locations}


def number(value: float) -> str:
    """Format engineering quantities without suppressing small nonzero values."""
    return f"{value:.3g}" if 0 < abs(value) < 0.1 else f"{value:.1f}"


def value(data: dict[str, Any], key: str) -> str:
    """Bind a displayed number to the full precision evidence record."""
    item: Any = data
    for part in key.split("."):
        item = item[int(part)] if isinstance(item, list) else item[part]
    formatted = f"{item:.2f}" if key.endswith("occurrence_fraction") else number(item)
    return f'<span data-value="{key}">{formatted}</span>'


def result_tables(data: dict[str, Any]) -> tuple[str, str, str]:
    """Present the called module's input and output quantities without literals."""
    inputs, results, details = [], [], []
    for index, location in enumerate(data["example"]["locations"]):
        prefix = f"example.locations.{index}"
        results.append("<tr><td>" + location["label"] + "</td>" + "".join(
            f"<td>{value(data, prefix + '.' + key)}</td>" for key in (
                "annual_damage", "fatigue_life_years", "required_life_years", "margin")) +
            "<td>" + ("The arithmetic criterion is met." if location["passes"] else
                         "The arithmetic criterion is not met.") + "</td><td>[2]</td></tr>")
        for sea_index, sea in enumerate(location["sea_states"]):
            sp = f"{prefix}.sea_states.{sea_index}"
            label = location["label"] + "; " + sea["label"].lower()
            inputs.append("<tr><td>" + label + "</td>" + "".join(
                f"<td>{value(data, sp + '.' + key)}</td>" for key in (
                    "Hs", "Tp", "gamma", "occurrence_fraction")) + "<td>[1]</td></tr>")
            details.append("<tr><td>" + label + "</td>" + "".join(
                f"<td>{value(data, sp + '.' + key)}</td>" for key in (
                    "rms_stress_MPa", "bandwidth_parameter", "peak_rate_Hz",
                    "damage_per_year_full", "damage_per_year_weighted")) + "<td>[2]</td></tr>")
    return "".join(inputs), "".join(results), "".join(details)


def render(data: dict[str, Any]) -> str:
    """Write the assessment as one consistent narrative with cited limits."""
    inputs, results, details = result_tables(data)
    def v(key: str) -> str:
        return value(data, "example." + key)
    source_rows = "".join(
        f"<tr><td><code>{html.escape(path)}</code></td><td><code>{digest}</code></td></tr>"
        for path, digest in data["sources"].items())
    style_match = re.search(r"<style>(.*?)</style>", (ROOT / STYLE).read_text(encoding="utf-8"), re.S)
    if style_match is None:
        raise ValueError("The house-style exemplar has no stylesheet")
    style = style_match.group(1)
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Response amplitude operator spectral fatigue module analysis</title>
<style>{(ROOT / BRAND).read_text(encoding="utf-8")}</style><style>{style}</style>
</head><body class="hs-doc"><main class="wrap">
<header><p class="kicker">Fatigue analysis</p>
<h1>Response amplitude operator spectral fatigue module analysis</h1>
<p>Draft for client review</p>
<p class="sub">Illustrative public example; screening arithmetic with unresolved method limitations.</p></header>

<section id="s1"><h2>1. Executive Summary</h2>
<p>The response amplitude operator spectral fatigue module is a frequency-domain calculation for structural hot spots subjected to wave loading. Analysis is performed to determine the available calculation capability and the limits of the published illustrative example; the key results are summarized below.</p>
<ul><li>The public example can be reproduced from its supplied wave spectra and scalar stress transfers.
<ul><li>The splash-zone detail gives a screening arithmetic life of {v('locations.0.fatigue_life_years')} years against the configured {v('locations.0.required_life_years')} years. The arithmetic criterion is not met.</li>
<li>The keel-joint detail gives a screening arithmetic life of {v('locations.1.fatigue_life_years')} years against the configured {v('locations.1.required_life_years')} years. The arithmetic criterion is met; engineering acceptance is not established.</li></ul></li>
<li>Design or operating decisions are deferred pending calculation qualification.
<ul><li>The cycle-rate convention and the default fatigue estimator require correction and independent comparison. The illustrative stress transfer also has an unresolved amplitude-versus-range convention. [1–4]</li></ul></li></ul>
<figure><div id="life-chart" aria-label="Screening arithmetic fatigue life against configured required life"></div>
<figcaption>Figure 1. Screening arithmetic life by detail. The line is the configured required life; every plotted life retains the unresolved method limitations. [1, 2]</figcaption></figure>
<noscript><p>Interactive charts and comment controls require JavaScript. The calculation values remain available in the tables.</p></noscript>
</section>
<div id="revision-history"><h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and cause</th></tr></thead>
<tbody><tr><td>A</td><td>2026-10-22</td><td>Initial module assessment, issued to distinguish reproducible screening arithmetic from qualified engineering results.</td></tr></tbody></table>
<p>Table 1. Document revision history.</p></div>

<section id="s2"><h2>2. Introduction</h2>
<p>The system is represented by supplied scalar stress responses at a splash-zone detail and a keel-joint detail. Spectral fatigue analysis is performed for the documented illustrative wave environments; water depth, a physical riser stack-up and an actual host response are not supplied. [1]</p>
<p>The intended use is reproducible calculation demonstration. Structural life, allowable offset and permission to remain connected are not established by these inputs.</p>
</section>

<section id="s3"><h2>3. Design Data</h2>
<p>The design basis is read from the public example without alteration of its engineering values. A real structure is not represented by qualified geometry, site data or calibrated stress transfer measurements. [1]</p>
<h3>3.1 System and environmental basis</h3>
<p>The configured design life is {v('design_life_years')} years and the design fatigue factor is {v('dff')}. These are example inputs, rather than prescribed project requirements. The stress-versus-cycle relation is supplied by the example; its material, weld class and environmental applicability remain unverified. [1]</p>
<p>The supplied spectra use the Joint North Sea Wave Project formulation, abbreviated JONSWAP. Significant wave height, peak period, peak enhancement and occurrence are recorded below. The example response amplitude operators are illustrated in Figure 2. [1]</p>
<table><thead><tr><th>Detail and environment</th><th>Wave height (m)</th><th>Peak period (s)</th><th>Peak enhancement (dimensionless)</th><th>Occurrence (dimensionless)</th><th>Source</th></tr></thead><tbody>{inputs}</tbody></table>
<p>Table 2. Published illustrative sea-state inputs. The occurrence is a time fraction at the named detail. [1]</p>
<figure><div id="rao-chart" aria-label="Supplied stress transfer against wave frequency"></div>
<figcaption>Figure 2. Supplied illustrative stress transfer magnitudes against cyclic frequency. Phase and heading are not inputs to this module. A stress-transfer acceptance limit is not supplied. [1, 3]</figcaption></figure>
<h3>3.2 Criteria and required data</h3>
<p>The implemented criterion compares reciprocal annual damage with design life multiplied by the design fatigue factor. The purpose of this criterion is to reserve a fatigue-life margin against the configured exposure; passing the arithmetic test alone does not qualify the physical design. The criterion and damage weighting are implementation definitions, rather than an independently checked standards requirement. [2]</p>
<p>Geometry, stiffness, boundary conditions, stress concentration, material environment, stress amplitude convention and an independently supported stress transfer are required inputs for engineering use. These quantities are NOT_EVALUATED; no substitute field values are introduced.</p>
</section>

<section id="s4"><h2>4. Analysis Methodology</h2>
<p>The wave spectrum is transferred to a stress spectrum and is evaluated by the downstream fatigue library. Linear response, stationary exposure within each sea state and a scalar stress response are assumed by the calculation; applicability to an actual structure is not established. [2–4]</p>
<h3>4.1 Model and boundary conditions</h3>
<figure><svg viewBox="0 0 780 310" role="img" aria-label="Qualitative floating-host riser model with two supplied stress-response locations" style="max-width:100%">
<path d="M80 60 H270 L240 95 H110 Z" fill="var(--soft)" stroke="var(--brand)" stroke-width="3"/>
<path d="M130 60 V30 H220 V60 M175 95 V270" fill="none" stroke="var(--brand)" stroke-width="5"/>
<path d="M20 125 Q60 110 100 125 T180 125 T260 125 T340 125 M70 275 H290" fill="none" stroke="var(--accent)" stroke-width="2"/>
<circle cx="175" cy="125" r="7" fill="var(--nogo)"/><circle cx="175" cy="95" r="7" fill="var(--caution)"/>
<path d="M185 95 H360 M185 125 L300 155 H360 M180 220 H360" fill="none" stroke="var(--mut)"/>
<text x="375" y="45">Floating host; motions not supplied</text>
<text x="375" y="100">Keel-joint stress transfer</text><text x="375" y="160">Splash-zone stress transfer</text>
<text x="375" y="220">Riser; geometry and stiffness not supplied</text>
<text x="375" y="255">Supports, tension and seabed connection:</text><text x="375" y="280">boundary conditions not evaluated</text>
<text x="20" y="150">Waterline</text></svg>
<figcaption>Figure 3. Qualitative physical context of the supplied hot-spot transfers. The sketch is not a structural model or a dimensional stack-up. The module consumes transfers; it does not solve host motions, riser statics or support reactions. [1, 3]</figcaption></figure>
<h3>4.2 Transfer, moments and fatigue equations</h3>
<div class="eq">Sσ(f) = |H(f)|² Sη(f); ω = 2πf; Sη(f) = 2π Sη(ω).</div>
<p>The symbol f is cyclic frequency in hertz and ω is angular frequency in radians per second. H is stress amplitude per unit wave amplitude in megapascals per metre. Sη is wave-elevation power spectral density, per hertz or per angular frequency as indicated. Sσ is stress power spectral density in square megapascals per hertz. The density conversion preserves the integral when the frequency axis changes. [3]</p>
<div class="eq">mₙ = ∫ fⁿ Sσ(f) df; σ = √m₀; α₂ = m₂ / √(m₀m₄); ε = √(1 − α₂²).</div>
<p>The symbol mₙ is a stress spectral moment of order n; σ is root-mean-square stress, α₂ is the irregularity factor and ε is the bandwidth measure. With a cyclic-frequency axis, the zero-upcrossing rate is √(m₂/m₀) and the peak rate is √(m₄/m₂), in cycles per second. The library divides these rates by an additional angular-to-cyclic conversion. Its returned damage and life therefore remain screening arithmetic. [4, 5]</p>
<div class="eq">N = A / Δσᵇ; d = ν E[(Δσ)ᵇ] / A; Dannual = Σ pᵢ dᵢ Tyear; L = 1 / Dannual; Lrequired = Ldesign F; M = L / Lrequired.</div>
<p>The symbol N is cycles to failure, Δσ is stress range in megapascals, b is the supplied stress-versus-cycle slope and A is its intercept in a consistent stress unit. The symbol d is damage per second, ν is the estimator's cycle rate, E is the cycle-range expectation, pᵢ is sea-state occurrence, Tyear is the library's annual duration, Dannual is annual damage, L is life in years, F is the configured design fatigue factor and M is the dimensionless life margin. The annual conversion follows the library convention. [2, 4]</p>
<p>The default estimator represents cycle ranges by an exponential and Rayleigh mixture. Its parameter implementation uses the bandwidth measure where the published method uses the irregularity factor. Agreement with another route sharing this estimator does not establish fatigue accuracy. [4, 5]</p>
</section>

<section id="s5"><h2>5. Detailed Results</h2>
<p>The worked example is calculated by calling the documented module router with the public input. Every reported example number is taken from that input, the returned summary, or the generated detailed output; intermediate stress spectra are obtained from the module transfer function. [1–3]</p>
<h3>5.1 Worked example and configured life criterion</h3>
<p>The splash-zone detail gives a screening arithmetic life of {v('locations.0.fatigue_life_years')} years against the configured {v('locations.0.required_life_years')} years. The arithmetic criterion is not met. The keel-joint detail gives a screening arithmetic life of {v('locations.1.fatigue_life_years')} years against the configured {v('locations.1.required_life_years')} years. The arithmetic criterion is met; engineering acceptance is not established. [1, 2]</p>
<table><thead><tr><th>Detail</th><th>Annual damage (year⁻¹)</th><th>Screening life (years)</th><th>Required life (years)</th><th>Life margin (dimensionless)</th><th>Arithmetic disposition</th><th>Source</th></tr></thead><tbody>{results}</tbody></table>
<p>Table 3. Provisional fatigue arithmetic from the unchanged illustrative example. The known rate and estimator limitations affect every row. [2, 4]</p>
<p>The failure of the splash-zone arithmetic criterion is addressed by withholding engineering acceptance, correcting the estimator and establishing the stress transfer basis before any structural mitigation is selected. Stress reduction at the hot spot is a candidate design measure after recalculation establishes the governing stress and exposure. [2–5]</p>
<h3>5.2 Transferred spectra and existing evidence</h3>
<label for="spectrum-case">Detail and environment</label><select id="spectrum-case"></select>
<figure><div id="spectrum-chart" aria-label="Calculated stress spectral density against cyclic frequency"></div>
<figcaption>Figure 4. Stress power spectral density obtained from the module. Hover or focus a point to inspect detail, environment, demand, limit status and units. A spectral-density acceptance limit is not supplied. [3]</figcaption></figure>
<p id="chart-readout" aria-live="polite">Select or focus a chart point for its values.</p>
<p>The existing registry regression checks the transfer expression, governing detail, arithmetic status and reciprocal-life relation. The constant-transfer regression compares annual damage and life with the fixed-gain route. Both are internal consistency evidence. Neither provides an independent published numerical result for this example. [6]</p>
<p>The library tests exercise spectral moments, positive damage, response scaling and bandwidth behaviour. Passing those assertions does not resolve the cycle-rate or estimator discrepancies described above. A published independently reproduced example with an accepted tolerance is NOT_EVALUATED. [4–6]</p>
</section>

<section id="s6"><h2>6. Key Findings</h2>
<p>The wave-to-stress transfer and occurrence-weighted damage sequence are available through a documented public function. Reproduction of the example is established as calculation-output consistency by the accompanying rebuild test; physical calibration and fatigue-method accuracy are not established. [1–4, 6]</p>
<p>The splash-zone detail governs the current screening arithmetic. Its life is below the configured required life; the other detail's arithmetic pass is insufficient for engineering acceptance while the common estimator remains unqualified. [2, 4]</p>
</section>

<section id="s7"><h2>7. Limitations</h2>
<p>The applicability boundary is set by the supplied scalar transfer and the unresolved numerical conventions. Project design life, inspection intervals and allowable operations cannot be established from the present example. [1–5]</p>
<p>The wave grid is fixed by the spectrum generator because the router does not expose grid controls. The spectrum is normalized over that finite band; this preserves a target variance without demonstrating that omitted tails or resonant responses are insignificant. Transfers are linearly interpolated and clamped at their endpoints outside their supplied frequency band. Frequency convergence and transfer-band coverage are required before engineering use. [3, 7]</p>
<p>The input checks do not comprehensively reject non-finite values or enforce a normalized occurrence distribution. Independent checks of finiteness, units, ordering, amplitude convention and total exposure remain necessary. The public example calls its response a stress range per wave amplitude, while the module describes stress amplitude per wave amplitude; that ambiguity is unresolved. [1–3]</p>
<h3>Not covered</h3>
<p>Alternative wave spectra and fatigue estimators are available but are not assessed here. Complex phase, heading combinations, multiaxial fatigue, nonlinear response, corrosion allowance, thickness effects and local stress concentration are not resolved by the demonstrated scalar transfer. Still-water statics, current loading, offset envelopes, stroke, drift-off, recoil, weak-point strength and vortex-induced vibration are not evaluated by this report. [2, 3]</p>
</section>

<section id="s8"><h2>8. Recommendations</h2>
<p>Engineering acceptance is conditioned on removal of the identified calculation and input-basis gaps. The next assessment is defined by an independent reference calculation and a documented physical stress-transfer basis. [1–5]</p>
<ul><li>Correct and independently test the frequency-unit convention and estimator parameters before using life or arithmetic pass results to establish an inspection or design decision. A single-frequency analytical rate and an independently integrated cycle-range distribution provide separate checks. [4, 5]</li>
<li>Resolve whether the supplied response is stress amplitude or stress range; provide calibrated structural transfers covering the energetic wave band, with material, detail category and environment stated. [1, 3]</li>
<li>Establish wave-grid and response-grid convergence, retain omitted-tail evidence, and enforce finite inputs and a stated exposure normalization. [2, 3, 7]</li>
<li>After correction, recalculate the splash-zone detail against the configured required life. If that criterion remains unmet, assess stress reduction or a revised operating exposure with the same qualified calculation basis. No numerical operating limit or retrofit date is established by this example. [1, 2, 5]</li></ul>
</section>

<section id="s9"><h2>9. References</h2>
<p>The evidence is separated into published method literature and repository implementation records. Unchecked standards references are identified without reproducing licensed coefficient tables.</p>
<h3>9.1 Theory and standards basis</h3>
<p>[5] “Dirlik and Tovo–Benasciutti Spectral Methods in Vibration Fatigue: A Review with a Historical Perspective”, Metals, 2021, volume 11, article 1333, sections 2–4. This open article is used for the method definitions and the need to distinguish cyclic from angular frequency; it is not a numerical benchmark for the repository example.</p>
<p>The library cites DNV-RP-C203, 2021, “Frequency domain fatigue analysis”. The wave generator cites DNV-RP-C205, “Environmental conditions and environmental loads”, without an edition in its spectrum definitions. The module also cites DNV-RP-F204, “Riser fatigue”, without an edition or a specific clause. These attributions do not establish edition or clause alignment; licensed text has not been checked for this report. No standards-derived material coefficient is asserted as qualified.</p>
<h3>9.2 Evidence locators and reproduction</h3>
<ul><li>[1] <code>{EXAMPLE}</code>: public illustrative input, all fields and stress-convention comment.</li>
<li>[2] <code>src/digitalmodel/spectral_fatigue/workflow.py</code>: <code>router</code> and <code>_evaluate_location</code>; returned summary and detailed comma-separated values.</li>
<li>[3] <code>src/digitalmodel/rao_spectral_fatigue/workflow.py</code>: <code>router</code>, <code>_stress_psd_from_rao</code> and input checks; <code>src/digitalmodel/hydrodynamics/wave_spectra.py</code>: spectrum generation and normalization.</li>
<li>[4] <code>src/digitalmodel/fatigue/spectral_fatigue.py</code>: <code>compute_spectral_moments</code>, rate properties and <code>dirlik_damage</code>.</li>
<li>[5] <a href="https://doi.org/10.3390/met11091333">Open method article</a>.</li>
<li>[6] <code>tests/workflows/test_durable_workflows.py</code>: registry case <code>rao-spectral-fatigue</code> and <code>test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening</code>; <code>tests/fatigue/test_spectral_fatigue.py</code>: dependency regression assertions.</li>
<li>[7] <a href="https://numpy.org/doc/1.26/reference/generated/numpy.interp.html">NumPy 1.26 interpolation reference</a>: endpoint fill conventions.</li></ul>
<p>The documented public call is <code>from digitalmodel.rao_spectral_fatigue.workflow import router</code>, followed by <code>router(cfg)</code> for the parsed input mapping. The calculation returns the summary in <code>cfg["rao_spectral_fatigue"]</code>, a screening status and detailed and location-summary CSV destinations. Intermediate spectra are not part of the returned summary.</p>
<p>Reproduction command: <code>PYTHONPATH=src python scripts/reporting/build_rao_spectral_fatigue_report.py --scratch-dir "$TMPDIR"</code>. The rebuild test is <code>tests/reporting/test_rao_spectral_fatigue_report.py</code>. Only the output destination is redirected during evaluation; engineering inputs are retained. Complete unrounded example arrays are embedded in this document for the interactive plots and numerical comparison.</p>
<details><summary>Source integrity record</summary><table><thead><tr><th>Repository source</th><th>SHA-256 digest</th></tr></thead><tbody>{source_rows}</tbody></table><p>Table 4. Source digests at document generation. Source identity is separate from engineering qualification.</p></details>
</section>

<section id="s10"><h2>10. Acronyms</h2>
<p>The terminology is defined for reading the calculation basis and the source records. Symbols are defined beside their equations.</p>
<p>A response amplitude operator is the frequency-dependent stress transfer per unit wave amplitude. Power spectral density describes variance distributed over frequency. JONSWAP means Joint North Sea Wave Project. A stress-versus-cycle curve relates cyclic stress range to cycles to failure. The design fatigue factor multiplies the required design life. CSV means comma-separated values.</p>
</section>

<div id="appendix"><h2>Appendix. Supporting data and quality checks</h2>
<p>The supporting calculation values and missing-data disposition are retained below. Engineering qualification is separate from reproduction of the software output.</p>
<h3>Detailed calculation output</h3>
<table><thead><tr><th>Detail and environment</th><th>Root-mean-square stress (MPa)</th><th>Bandwidth (dimensionless)</th><th>Returned peak rate (Hz)</th><th>Full annual damage (year⁻¹)</th><th>Weighted annual damage (year⁻¹)</th><th>Source</th></tr></thead><tbody>{details}</tbody></table>
<p>Table 5. Detailed outputs from the called module. Peak rates and damage retain the unresolved frequency convention and estimator limitations. [2, 4]</p>
<h3>Data register</h3>
<table><thead><tr><th>Data item</th><th>Status and intended use</th><th>Source</th></tr></thead><tbody>
<tr><td>Wave and stress-transfer example</td><td>Published illustrative data; demonstration only. No field calibration is established.</td><td>[1]</td></tr>
<tr><td>Design life, fatigue factor and stress-versus-cycle relation</td><td>Published example settings; project applicability is NOT_EVALUATED.</td><td>[1]</td></tr>
<tr><td>Geometry, stack-up, material environment and boundary conditions</td><td>Required inputs; NOT_EVALUATED. No values are assumed.</td><td>[1, 3]</td></tr>
<tr><td>Stress amplitude convention and transfer calibration</td><td>Required confirmation; NOT_EVALUATED.</td><td>[1, 3]</td></tr>
<tr><td>Independent fatigue benchmark and convergence tolerance</td><td>Required qualification evidence; NOT_EVALUATED.</td><td>[4–6]</td></tr>
</tbody></table><p>Table 6. Data readiness register. No missing physical value is filled by an assumed number.</p>
<h3>Quality pre-flight evidence</h3>
<p>The rebuild test separately calls the module, reads its detailed CSV and compares every displayed example value and plotted array with the input and returned output. Repeated generation is checked for identical report bytes, and source hashes are read back. These checks establish reproducibility and source preservation, rather than engineering accuracy. [6]</p>
<p>The existing module-specific workflow assertions are recorded as evidence by source inspection. Their top-level engine imports solver adapters; they are not executed for this assessment. Pure numerical dependency tests are executed separately, and their results accompany the document delivery. No licensed solver is required by the report generator.</p>
<h3>Changes and comments</h3>
<details><summary>Engineering changes in this revision</summary><div id="change-panel"></div></details>
<p>Comments are associated with this exact report content. Save comments to the folder containing this HTML file. Where folder access is unavailable, download the comments file and place it beside this document. Unsaved comments are held only while this document is open.</p>
<label for="comment-section">Section</label><select id="comment-section">{''.join(f'<option value="s{i}">Section {i}</option>' for i in range(1, 11))}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="3" style="width:100%"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label for="load-comments">Load comments</label><input id="load-comments" type="file" accept=".json,application/json">
<p id="comment-status" role="status">No comments have been saved.</p><pre id="comment-list" style="white-space:pre-wrap"></pre>
</div></main>
<script id="report-data" type="application/json">__REPORT_DATA__</script>
<script>{(ROOT / UI).read_text(encoding="utf-8")}</script></body></html>
'''


def build(output_dir: Path, scratch_dir: Path) -> None:
    """Build and read back the report and its engineering change panel data."""
    data: dict[str, Any] = {"example": evaluate(scratch_dir), "changes": CHANGES,
            "sources": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
                        for path in SOURCES}}
    page = render(data)
    data["content_id"] = hashlib.sha256((page + encode(data)).encode()).hexdigest()
    page = page.replace("__REPORT_DATA__", encode(data))
    output_dir.mkdir(parents=True, exist_ok=True)
    output = output_dir / "index.html"
    output.write_text(page, encoding="utf-8")
    changes = output_dir / "changes.json"
    changes.write_text(json.dumps(CHANGES, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    if output.read_text(encoding="utf-8") != page or json.loads(changes.read_text(encoding="utf-8")) != CHANGES:
        raise ValueError("Report output readback differs from generated content")


def verify_header_only(before: Path, after: Path) -> None:
    """Reject hook changes beyond the repository's standard license header."""
    header = b"\n".join(b"# " + line for line in
                         (ROOT / ".license-header.txt").read_bytes().rstrip().splitlines()) + b"\n\n"
    if after.read_bytes() != header + before.read_bytes():
        raise ValueError("The hook made a non-header change")


def main() -> None:
    """Parse output locations without selecting an implicit temporary directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    parser.add_argument("--scratch-dir", type=Path, default=os.environ.get("TMPDIR"))
    parser.add_argument("--verify-header-change", nargs=2, type=Path, metavar=("BEFORE", "AFTER"))
    args = parser.parse_args()
    if args.verify_header_change:
        verify_header_only(*args.verify_header_change)
        return
    if args.scratch_dir is None:
        parser.error("--scratch-dir or TMPDIR is required")
    build(args.output_dir, Path(args.scratch_dir))


if __name__ == "__main__":
    main()
