# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the bounded fatigue analysis report into an explicit output directory."""
from __future__ import annotations

import argparse
import hashlib
import html
import json
import re
from fractions import Fraction
from pathlib import Path

import pandas as pd

from digitalmodel.structural.fatigue.damage_accumulation import LinearDamageAccumulation
from digitalmodel.structural.fatigue.sn_curves import PowerLawSNCurve

ROOT = Path(__file__).resolve().parents[4]
REVISION = "00"
DATE = "2026-10-28"  # Fixed document issue date, never the rebuild time.
EXEMPLAR = "docs/reports/pipeline-ffs-assessment.html"
REBUILD = ('PYTHONDONTWRITEBYTECODE=1 MPLCONFIGDIR="$TMPDIR/matplotlib" '
           'PYTHONPATH=src uv run --no-project --python "$FATIGUE_REPORT_PYTHON" '
           'python docs/reports/modules/fatigue/build_report.py '
           '--output-dir "$TMPDIR/fatigue-report"')
CHANGES = {
    "report": "fatigue-synthetic-arithmetic",
    "revision": REVISION,
    "date": DATE,
    "changes": [
        "Added an artificial power-law example to separate arithmetic checks from material qualification.",
        "Mapped implemented methods, counting refusals and unevaluated engineering uses so that numerical scope is explicit.",
        "Added deterministic source and change records, parsed numerical checks and module-local report pointers.",
    ],
}


def calculate() -> dict:
    """Evaluate explicit counted cycles; no signal counting or standard tables."""
    # Synthetic input definition: N*S^2=1,000,000 cycle*MPa^2, no material identity.
    # The 20,000-cycle synthetic cutoff exceeds both independently derived lives.
    basis = {"coefficient": 1_000_000, "exponent": 2, "fatigue_limit": 0, "cutoff": 20_000}
    cycles = pd.DataFrame({"range": [10, 20], "mean": [0, 0], "count": [1000, 500]})
    curve = PowerLawSNCurve("Synthetic arithmetic", A=basis["coefficient"],
                            m=basis["exponent"], fatigue_limit=basis["fatigue_limit"],
                            cutoff_cycles=basis["cutoff"], material=None)
    # The curve base class attaches unused generic properties for material=None.
    # Correction remains disabled; those defaults are not an example material basis.
    raw = LinearDamageAccumulation(mean_stress_correction=None).calculate_damage(cycles, curve)
    levels = [{"range": float(row["stress_range"]), "mean": float(cycles.iloc[i]["mean"]),
               "count": float(row["cycles_applied"]), "allowable": float(row["cycles_allowable"]),
               "damage": float(row["damage_increment"])}
              for i, row in enumerate(raw["damage_contributions"])]
    units = {"basis.coefficient": "cycle*MPa^2", "basis.exponent": "dimensionless",
             "basis.fatigue_limit": "MPa", "basis.cutoff": "cycles",
             "total_damage": "dimensionless", "cycles_processed": "cycles"}
    for i in range(len(levels)):
        for key, unit in (("range", "MPa"), ("mean", "MPa"), ("count", "cycles"),
                          ("allowable", "cycles"), ("damage", "dimensionless")):
            units[f"levels.{i}.{key}"] = unit
    return {"basis": basis, "levels": levels, "total_damage": float(raw["total_damage"]),
            "cycles_processed": float(raw["cycles_processed"]), "units": units}


def lookup(record: dict, path: str):
    for part in path.split("."):
        record = record[int(part)] if isinstance(record, list) else record[part]
    return record


def independent_values() -> dict:
    """Evaluate the hand derivation with exact rational arithmetic, without APIs."""
    # Independent synthetic basis, repeated explicitly rather than read from results.
    lives = [Fraction(1_000_000, stress * stress) for stress in (10, 20)]
    increments = [Fraction(count) / life for count, life in zip((1000, 500), lives)]
    result = {"total_damage": sum(increments), "cycles_processed": Fraction(1000 + 500)}
    for i, (life, increment) in enumerate(zip(lives, increments)):
        result[f"levels.{i}.allowable"] = life
        result[f"levels.{i}.damage"] = increment
    return result


def source_record() -> dict:
    """Pin repository evidence by content, without embedding local locators."""
    entries = [
        ("ref-curve", "Explicit power law and correction interfaces",
         "src/digitalmodel/structural/fatigue/sn_curves.py", ["PowerLawSNCurve", "get_allowable_cycles", "MeanStressCorrection"]),
        ("ref-damage", "Counted-cycle accumulation and derived metrics",
         "src/digitalmodel/structural/fatigue/damage_accumulation.py", ["LinearDamageAccumulation", "calculate_damage"]),
        ("ref-rainflow", "Structural diagnostic counter",
         "src/digitalmodel/structural/fatigue/rainflow.py", ["RainflowCounter", "rainflow_count"]),
        ("ref-analysis", "Time-series damage refusal",
         "src/digitalmodel/structural/fatigue/analysis.py", ["analyze_time_series", "refuse_damage_calculation"]),
        ("ref-contract", "Counting path registry and retained residual contract",
         "src/digitalmodel/fatigue/counting_contract.py", ["COUNTING_PATHS", "ResidualPolicy", "refuse_damage_calculation"]),
        ("ref-frequency", "Spectral methods and frequency convention",
         "src/digitalmodel/structural/fatigue/frequency_domain.py", ["calculate_damage_rate", "_calculate_spectral_moments"]),
        ("ref-examples", "Existing examples, preserved separately",
         "src/digitalmodel/structural/fatigue/worked_examples.py", ["ExampleResult", "LinearDamageAccumulation"]),
        ("ref-pack", "Module knowledge pack; inventory rather than qualification",
         "src/digitalmodel/structural/fatigue/docs/agent-pack.md", ["Purpose and scope"]),
        ("ref-sn-tests", "Existing S-N regression suite",
         "tests/structural/fatigue/test_sn_curves.py", ["PowerLawSNCurve"]),
        ("ref-damage-tests", "Existing damage regression suite",
         "tests/structural/fatigue/test_damage_accumulation.py", ["LinearDamageAccumulation"]),
        ("ref-report-tests", "Independent oracle, parsed values, mutation and rebuild checks",
         "tests/structural/fatigue/test_module_report.py", ["test_worked_values_match_independent_calculation", "test_visible_values_follow_computed_results"]),
        ("ref-builder", "Calculation and rendering bindings",
         "docs/reports/modules/fatigue/build_report.py", ["calculate", "independent_values"]),
        ("ref-layout", "Local report layout exemplar", EXEMPLAR, ["<style>"]),
    ]
    files = [{"id": key, "title": title, "path": path, "symbols": symbols,
              "sha256": hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}
             for key, title, path, symbols in entries]
    return {
        "report": CHANGES["report"], "revision": REVISION,
        "basis": {"id": "synthetic-basis", "classification": "synthetic",
                  "description": "Artificial arithmetic example with no material identity or standard coefficient table.",
                  "equations": ["N*S^2 = 1000000 cycle*MPa^2", "N = A/S^m", "d_i = n_i/N_i", "D = sum(d_i)", "C = sum(n_i)"],
                  "inputs": {"A": "1000000 cycle*MPa^2", "m": "2 dimensionless", "ranges": "10,20 MPa",
                             "means": "0,0 MPa", "counts": "1000,500 cycles", "fatigue_limit": "0 MPa", "cutoff": "20000 cycles"},
                  "hand_derivation": {"allowable": "1000000/100=10000; 1000000/400=2500 cycles",
                                      "damage": "1000/10000=1/10; 500/2500=1/5; total=3/10 dimensionless",
                                      "processed": "1000+500=1500 cycles"}},
        "roundoff": {"id": "roundoff-budget", "relative_tolerance": "32 * 2**(-52)", "absolute_tolerance": "0",
                     "derivation": "Fewer than eight rounding stages; four epsilon per stage allows for reciprocal-square evaluation. Binary64 epsilon is the spacing above unity. Numerical regression budget only; no material uncertainty is represented."},
        "readiness": "Synthetic arithmetic only; engineering qualification NOT_EVALUATED",
        "files": files,
    }


def rounded(value):
    """Embed calculation values at displayed precision; tests keep raw values."""
    if isinstance(value, dict):
        return {key: rounded(item) for key, item in value.items()}
    if isinstance(value, list):
        return [rounded(item) for item in value]
    return round(value, 1) if isinstance(value, float) else value


def json_text(value) -> str:
    return json.dumps(value, indent=2, ensure_ascii=True, allow_nan=False) + "\n"


def embedded_json(value) -> str:
    """Prevent HTML parsing of any JSON string, including a closing script tag."""
    return json_text(value).replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")


def table(headers, rows, caption):
    """Render trusted markup cells; escape data before supplying a cell."""
    return ('<div class="table-wrap"><table><thead><tr>'
            + "".join(f'<th scope="col">{html.escape(h)}</th>' for h in headers)
            + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
            + "</tbody><caption>" + html.escape(caption) + "</caption></table></div>")


def build(record: dict) -> str:
    """Render results, evidence and explicit limits into a standalone page."""
    sources = source_record()
    oracle = independent_values()

    def ref(key, text):
        return f'<a href="#{html.escape(key, quote=True)}">{html.escape(text)}</a>'

    def n(key):
        unit = html.escape(record["units"][key], quote=True)
        source = "ref-damage" if key in oracle else "synthetic-basis"
        return (f'<span data-value="{html.escape(key, quote=True)}" data-unit="{unit}" '
                f'data-source="{source}">{lookup(record, key):.1f} {unit}</span>')

    def reference(key):
        unit = html.escape(record["units"][key], quote=True)
        return (f'<span data-oracle="{key}" data-unit="{unit}" data-source="synthetic-basis">'
                f'{float(oracle[key]):.1f} {unit}</span>')

    inputs = [["Synthetic coefficient A", n("basis.coefficient"), "N*S^m=A"],
              ["Synthetic exponent m", n("basis.exponent"), "Power-law definition"],
              ["Synthetic fatigue limit", n("basis.fatigue_limit"), "No positive fatigue limit"],
              ["Synthetic cutoff control", n("basis.cutoff"), "Above both allowable lives"]]
    for i in range(len(record["levels"])):
        for key, label in (("range", "range S"), ("mean", "mean stress"), ("count", "applied count n")):
            inputs.append([f"Synthetic level {i + 1} {label}", n(f"levels.{i}.{key}"), "Explicit counted input"])
    input_table = table(["Quantity", "Value (unit shown)", "Synthetic basis"], inputs,
                        "Table 1. Artificial inputs; the coefficient has no material identity.")
    comparisons = []
    for key, expected in oracle.items():
        # Compute differences before rounding; no rounded comparison is an oracle.
        difference = abs(lookup(record, key) - float(expected)) / abs(float(expected)) * 100
        comparisons.append([html.escape(key), reference(key), n(key),
                            f'<span data-difference="{key}">{difference:.1f} %</span>'])
    result_table = table(["Quantity", "Exact derivation, displayed (unit shown)",
                          "Module result (unit shown)", "Absolute relative difference (%)"], comparisons,
                         "Table 2. Rational hand calculation compared with unrounded module results before display rounding.")
    methods = [
        [ref("ref-curve", "PowerLawSNCurve.get_allowable_cycles"), "Implemented", "Synthetic single slope at supplied ranges", "INDEPENDENTLY_CHECKED for this example only"],
        [ref("ref-damage", "LinearDamageAccumulation.calculate_damage"), "Implemented", "Explicit counted cycles; each increment, sum and processed count", "INDEPENDENTLY_CHECKED for this example only"],
        [ref("ref-curve", "Standard curves and mean-stress interfaces"), "Implemented", "No coefficient-table or material reference evaluation", "NOT_EVALUATED"],
        [ref("ref-rainflow", "RainflowCounter / rainflow_count"), "Implemented; diagnostic use", "No counting benchmark performed here", "NOT_EVALUATED"],
        [ref("ref-analysis", "FatigueAnalysisEngine.analyze_time_series"), "Refuses damage calculation", "Existing counting contract is preserved", "NOT_EVALUATED"],
        [ref("ref-frequency", "Frequency-domain damage methods"), "Implemented", "No spectral benchmark performed here", "NOT_EVALUATED"],
        [ref("ref-damage", "Physical remaining life"), "Derived cycle estimate is exposed", "No material, duty duration or field evidence", "NOT_EVALUATED"],
    ]
    method_table = table(["Interface / method", "Implementation state", "Independent evidence in this report", "Disposition"], methods,
                         "Table 3. Implementation inventory is separate from numerical qualification.")
    limits = table(["Engineering use", "Status", "Evidence needed"], [
        ["standard S-N coefficients", "NOT_EVALUATED", "A reviewed edition, curve identity, material basis and point checks"],
        ["mean-stress corrections", "NOT_EVALUATED", "Material strengths, correction selection and independent numerical cases"],
        ["rainflow-to-damage qualification", "NOT_EVALUATED", "Hand-counted distributions, declared residual policy and a qualified path"],
        ["frequency-domain methods", "NOT_EVALUATED", "PSD units, spectral-moment checks and independent damage-rate benchmarks"],
        ["physical remaining life", "NOT_EVALUATED", "Qualified material/load basis, service duration and acceptance criterion"],
    ], "Table 4. Unevaluated methods remain outside this arithmetic example.")
    locators = "".join(
        f'<li id="{html.escape(s["id"], quote=True)}">{html.escape(s["title"])}: '
        f'<code>{html.escape(s["path"])}</code>; symbols '
        + ", ".join(f'<code>{html.escape(symbol)}</code>' for symbol in s["symbols"]) + ".</li>"
        for s in sources["files"])
    changes = "".join(f"<li>{html.escape(change)}</li>" for change in CHANGES["changes"])
    revisions = table(["Revision", "Date", "What changed and why"], [[REVISION, DATE,
        "First issue: added a synthetic counted-cycle example and method limits to bound arithmetic evidence."]],
        "Revision history. Each issue retains its original change record.")
    titles = ["Executive Summary", "Introduction", "Data Basis", "Theory and Equations", "Worked Example",
              "Validation Evidence", "Limits of Use", "Recommendations", "References", "Appendix"]
    navigation = "".join(f'<li>{ref(f"section-{i}", title)}</li>' for i, title in enumerate(titles, 1))
    style = re.search(r"<style>(.*?)</style>", (ROOT / EXEMPLAR).read_text(encoding="utf-8"), re.S)[1]
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Structural fatigue module analysis: synthetic counted-cycle arithmetic</title>
<style>{style}</style>
<style>code,pre{{overflow-wrap:anywhere;white-space:pre-wrap}}details{{margin:20px 0;padding:12px;border:1px solid var(--line)}}:focus-visible{{outline:2px solid var(--report-accent)}}</style>
</head><body class="hs-doc" id="fatigue-synthetic-arithmetic" data-revision="{REVISION}">
<header><h1>Structural fatigue module analysis: synthetic counted-cycle arithmetic</h1>
<p>Draft for maintainer review · Revision {REVISION} · {DATE}</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{navigation}</ol></nav><main>
<section id="section-1"><h2>1. Executive Summary</h2>
<p>The structural fatigue package is mapped and an explicitly bounded arithmetic example is checked.
The synthetic counted-cycle block gives total damage {n("total_damage")} and
{n("cycles_processed")} processed. These values agree with the independent rational derivation
under the numerical comparison budget in {ref("roundoff-budget", "Section 6")}.</p>
<p>The synthetic curve has no material identity. Standard coefficients, signal counting, spectral methods
and physical remaining life are not qualified by this example. Broader numerical qualification remains open.</p>
</section>
<div id="revision-history"><h3>Revision history</h3>{revisions}</div>
<section id="section-2"><h2>2. Introduction</h2>
<p>The public interfaces provide S-N curves, cycle counting, damage accumulation and frequency-domain methods.
The {ref("ref-pack", "module knowledge pack")} records alternate implementations and unresolved evidence.
An implemented method is an inventory finding, not a standards-compliance conclusion.</p>
<p>The bounded path is explicit counted rows → artificial curve → per-level damage → total damage.
Existing {ref("ref-examples", "standalone examples")} retain their separate contracts and are unchanged.</p>
{method_table}</section>
<section id="section-3"><h2>3. Data Basis</h2>
<p id="synthetic-basis">All numeric example inputs are synthetic arithmetic controls, supplied for this calculation.
Ranges are stress ranges in MPa, not amplitudes; means are in MPa and counts are in cycles.
No material identity, measured load record, standard table or service interval is assigned.</p>
{input_table}
<p>The configured cutoff is above both hand-derived allowable lives, so neither level is capped.
The fatigue limit is zero and both ranges are positive. Mean-stress correction is disabled.
The base constructor attaches generic properties even for material=None; those defaults are unused here
and do not supply a material identity or an engineering input basis.
{ref("ref-curve", "Curve input contract")} and {ref("ref-damage", "counted-cycle contract")} govern the calls.</p>
</section>
<section id="section-4"><h2>4. Theory and Equations</h2>
<p>The artificial definition is N*S^m = A, with A and m supplied in Table 1.
The independent derivation uses integer multiplication and exact rational division:
N_i = A / S_i^m; d_i = n_i / N_i; D = Σ d_i; C = Σ n_i.
N_i and n_i are cycles; S_i is MPa; d_i and D are dimensionless; C is processed cycles.</p>
<p>The coefficient unit is cycle*MPa^2 for the specified exponent.
Each allowable life is obtained by dividing the coefficient by the squared supplied range.
Each contribution is obtained by dividing its applied count by that allowable life.
The exact quotients and their rational sum are compared with module results in Table 2.
{ref("synthetic-basis", "Synthetic input definition")} supplies the arithmetic basis;
{ref("ref-curve", "get_allowable_cycles")} and {ref("ref-damage", "calculate_damage")} supply the evaluated methods.</p>
</section>
<section id="section-5"><h2>5. Worked Example</h2>
<p>Counted rows are passed directly to the accumulator. No time series is counted and no standard curve factory is called.
The comparison column is computed from unrounded differences; values and percentages are displayed to one decimal place.</p>
{result_table}
<p>The result supports the supplied arithmetic block only. No physical design acceptance or remaining-life estimate is assigned.</p>
</section>
<section id="section-6"><h2>6. Validation Evidence</h2>
<p>The {ref("ref-report-tests", "report tests")} assert every unrounded example quantity and its unit against
an independent hand calculation, then compare parsed visible quantities with that oracle and a fresh module call.
Controlled changes to allowable lives, damage contributions, the total and the processed count exercise visible result bindings.</p>
<p id="roundoff-budget">The numerical regression tolerance is relative 32 epsilon, with zero absolute tolerance;
epsilon is the binary64 spacing above unity, expressed as 2^(-52).
The positive-input path has fewer than eight rounding stages through reciprocal-square evaluation, multiplication,
division, summation and oracle conversion. Four epsilon per stage allows for the power evaluation.
This budget addresses roundoff for this example; it does not represent physical uncertainty.</p>
<p>The {ref("ref-sn-tests", "existing S-N suite")} and {ref("ref-damage-tests", "existing accumulation suite")}
provide regression coverage beyond this example; passing them does not independently qualify their standard data.
Two isolated rebuilds and the delivered records are compared byte for byte.
Reproducibility is not physical validation.</p>
</section>
<section id="section-7"><h2>7. Limits of Use</h2>
{limits}
<p>The {ref("ref-contract", "counting registry")} distinguishes paths and a declared retained residual convention.
The {ref("ref-analysis", "time-series analysis entry point")} refuses damage on its affected structural counting path;
the structural counter remains available for diagnostic use. Other paths have distinct contract records.
Their recorded status is not transferred to this example and no counting refusal is changed or bypassed.</p>
<p>Review findings beyond scope: the single-slope cutoff caps finite life even with a zero fatigue limit;
the synthetic cutoff deliberately avoids that branch. Nonpositive ranges or counts are skipped by accumulation;
this positive-input example does not qualify malformed or non-finite input handling.
The accumulator also exposes a repeated-block remaining-cycle estimate; no physical life follows without a qualified
load basis and duration. These concerns are findings for broader qualification, not behavior changes.</p>
<p>No full-module, standards-compliance or solver-qualification claim is made.</p>
</section>
<section id="section-8"><h2>8. Recommendations</h2>
<h3>Way forward</h3>
<ol><li>Review standard coefficient provenance and material selection separately, with independently derived curve points.</li>
<li>Qualify a counting path against hand-counted range, mean and count distributions under its declared residual policy before damage integration.</li>
<li>Check spectral moments and damage rates against independent benchmarks with explicit frequency and PSD units.</li>
<li>Supply a service-duration basis, material evidence and governing acceptance criterion before reporting physical remaining life.</li></ol>
<p>The parent module-report scope remains open for these numerical qualifications.</p>
</section>
<section id="section-9"><h2>9. References</h2>
<p>Source locators below are repository-relative and are pinned by SHA-256 in the embedded source record and sources.json.
The page requires no network access or surrounding directory layout. No standard coefficient tables are reproduced.</p>
<ol>{locators}</ol>
</section>
<section id="section-10"><h2>10. Appendix</h2>
<h3>Rebuild</h3><p>From the repository root, set FATIGUE_REPORT_PYTHON to a Python interpreter containing the repository dependencies.
The supplied TMPDIR, UV_CACHE_DIR and PRE_COMMIT_HOME remain exported:</p>
<pre>{html.escape(REBUILD)}</pre>
<p>The explicit output directory receives report.html, changes.json and sources.json. Calculation and rendering use memory;
no intermediate files are written. The fixed issue date is a revision record, not a runtime timestamp.</p>
<details id="engineering-changes" open><summary>Change panel · Revision {REVISION}</summary><ul>{changes}</ul></details>
</section>
<script type="application/json" id="calculation-record">{embedded_json(rounded(record))}</script>
<script type="application/json" id="source-record">{embedded_json(sources)}</script>
<script type="application/json" id="change-record">{embedded_json(CHANGES)}</script>
</main></div></body></html>
'''


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    # Rendering and strict JSON validation finish before creating output files.
    record = calculate()
    outputs = {"report.html": build(record), "changes.json": json_text(CHANGES),
               "sources.json": json_text(source_record())}
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for name, text in outputs.items():
        (args.output_dir / name).write_text(text, encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
