# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline, research-only audit of the retained surface-crack spike.

No solver is launched. A reference comparison is not native-output verification
or an API 579 Part 9 Level 3 qualification. All K values use MPa sqrt(m).
"""

import argparse
import hashlib
import html
import json
import math
from pathlib import Path

from surface_crack_post import BENCHMARK, reference_k


def positive(value, name, *, integer=False, zero_allowed=False):
    """Reject absent, nonfinite or physically invalid receipt quantities."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be numeric")
    if not math.isfinite(value) or value < 0 or (value == 0 and not zero_allowed):
        raise ValueError(f"{name} must be finite and positive")
    if integer and (not isinstance(value, int) or value == 0):
        raise ValueError(f"{name} must be a positive integer")
    return value


def compare_run(run, relative_limit):
    """Recompute differences without trusting cached reference/error fields."""
    summary = {
        "refinement": positive(run.get("refinement"), "refinement", integer=True),
        "nodes": positive(run.get("nodes"), "nodes", integer=True),
        "elements": positive(run.get("elements"), "elements", integer=True),
        "wall_seconds": positive(run.get("wall_seconds"), "wall_seconds",
                                 zero_allowed=True),
        "peak_rss_kib": positive(run.get("peak_rss_kib"), "peak_rss_kib"),
        "native_outputs": "NOT_REVALIDATED",
        "native_j": "NOT_EVALUATED",
        "contour_independence": "NOT_EVALUATED",
        "comparison": "NOT_EVALUATED",
        "deepest_difference": None,
        "surface_difference": None,
        "max_abs_front_difference": None,
        "deepest_k_mpa_sqrt_m": None,
    }
    if isinstance(run.get("returncode"), bool) or run.get("returncode") != 0:
        return summary
    front = run.get("front", [])
    differences = {}
    for row in front:
        angle = positive(row.get("phi_deg"), "phi_deg", zero_allowed=True)
        if angle > 90 or angle in differences:
            # NASA TM-83200 (1981), Fig. 2b: quarter-front angles 0 to 90 deg.
            raise ValueError("front angles must be unique and within 0 to 90")
        candidate = positive(row.get("k_mpa_sqrt_m"), "k_mpa_sqrt_m")
        reference = reference_k(angle)
        # Definition of signed relative difference; not a standards equation.
        differences[angle] = (candidate - reference) / reference
    if 0 not in differences or 90 not in differences:
        raise ValueError("front must include both surface and deepest endpoints")
    maximum = max(abs(value) for value in differences.values())
    summary.update(
        deepest_difference=differences[90], surface_difference=differences[0],
        max_abs_front_difference=maximum, front_points=len(front),
        deepest_k_mpa_sqrt_m=next(
            row["k_mpa_sqrt_m"] for row in front if row["phi_deg"] == 90),
        comparison=("WITHIN_RESEARCH_COMPARATOR" if maximum <= relative_limit
                    else "EXCEEDS_RESEARCH_COMPARATOR"),
    )
    return summary


def audit(data, *, relative_limit):
    """Audit the fixed synthetic benchmark using a caller-supplied comparator.

    The comparator is a research choice, never a code acceptance limit. Native
    files, contour sensitivity and a second solver are not supplied by these
    compact historical receipts. Accordingly qualified_results stays zero.
    """
    positive(relative_limit, "relative_limit")
    if data.get("benchmark") != BENCHMARK:
        raise ValueError("benchmark differs from the fixed reference geometry/load")
    runs = data.get("runs")
    if not isinstance(runs, list) or not runs:
        raise ValueError("runs must be a nonempty list")
    compared = [compare_run(run, relative_limit) for run in runs]
    levels = [run["refinement"] for run in compared]
    if levels != sorted(set(levels)):
        raise ValueError("refinement levels must be unique and increasing")
    return {
        "scope": "historical receipt arithmetic; no native files revalidated",
        "benchmark": dict(BENCHMARK),
        "relative_limit": relative_limit,
        "limit_basis": "caller-supplied research comparator; not code acceptance",
        "recorded_runs": len(compared),
        "runs_with_front_comparison": sum(
            run["comparison"] != "NOT_EVALUATED" for run in compared),
        "qualified_results": 0,
        "solver_selection": "NOT_EVALUATED",
        "code_aster": {"comparison": "NOT_EVALUATED",
                       "reason": "no native benchmark receipt supplied"},
        "licensed_comparison": "NOT_EVALUATED",
        "runs": compared,
    }


REPORT = '''<!doctype html>
<html lang="en"><meta charset="utf-8">
<title>Surface-crack solver program benchmarking analysis</title>
<style>body{font:16px system-ui;max-width:1100px;margin:2em auto;padding:1em}
table{border-collapse:collapse;width:100%}td,th{border:1px solid #bbb;padding:.5em}
pre{white-space:pre-wrap}p{line-height:1.5}th{text-align:left}</style>
<h1>Surface-crack solver program benchmarking analysis</h1>
<h2>Summary</h2>
<p>$RECORDED historical CalculiX benchmark runs are recorded; $COMPARED have front
comparisons and 0 are qualified Level 3 results. No new benchmark solve is started
by this audit. Solver selection is NOT_EVALUATED: native Code_Aster results,
contour sensitivity and the licensed comparison are absent. Of the evaluated
front comparisons, $EXCEEDED exceed the $LIMIT% research comparator and $WITHIN
meet it. That comparator is a study choice, not an API 579 limit.</p>
<h2>Revision history</h2>
<table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>
<tr><td>1</td><td>2026-10-20</td><td>Receipt audit and sourced solver recommendation
added to distinguish reference agreement from native qualification.</td></tr></table>
<p>Table 1. Issue history for this benchmarking analysis.</p>
<h2>Worked example: inputs, method and answer</h2>
<p>The existing synthetic plate has a = c = 5 mm, thickness = 10.000 mm, half-width =
25 mm, half-length = 25 mm and tensile membrane stress = 100 MPa. These are
research inputs from surface_crack_post.py, not measured data. The recorded
material inputs are E = 210000 MPa and Poisson ratio = 0.3; K for this prescribed
traction reference does not use those elastic constants. Bending is zero.</p>
<p>NASA TM-83200 (1981), pp. 4, 7, 12, Eqs. (2a), (10)-(11), (16)-(21), supplies
the finite-plate surface-flaw tension solution. With a/c = 1 and a/t = 0.5,
Q = 2.464, M1 = 1.04, M2 = -0.54 + 0.89/1.2, M3 = 0.5 - 1/1.65.
The finite-width factor is sqrt(sec(pi*sqrt(0.5)/10)). The deepest-point K is
100*(M1 + M2/4 + M3/16)*fw*sqrt(pi*0.005/Q), giving 8.8 MPa sqrt(m).
The surface multiplier g is 1 + 0.1 + 0.35*(0.5)^2, giving 10.4 MPa sqrt(m).
The test docstring records the independent arithmetic with unrounded values.</p>
<p>The finest recorded deepest K is $KDEEP MPa sqrt(m). Signed relative difference
is defined as 100*(candidate-reference)/reference and is computed before rounding:
$DEEP%. The surface difference is $SURFACE%. Every front station is recomputed
against the existing reference implementation; cached differences and reference
columns in the old receipt are not trusted.</p>
<h2>Recorded mesh cost and reference comparison</h2>
<table><tr><th>Refinement</th><th>Elements</th><th>Nodes</th><th>Time (s)</th>
<th>Peak RSS (KiB)</th><th>Deepest difference (%)</th>
<th>Surface difference (%)</th><th>Max absolute front difference (%)</th></tr>
$ROWS</table>
<p>Table 2. Historical costs from the 2026-10-11 receipt; these are not measurements
of the current environment. Differences are calculated from unrounded K values.</p>
<p>The original campaign uses linear C3D8/C3D6 elements and nodal-stress
extrapolation, not a native J-integral. Its three meshes are retained as recorded;
no added refinement or singular-element result is implied. The finest maximum
front difference is $MAXIMUM%; its comparison status is $COMPARISON. A trend
alone does not establish convergence. The historical surface trend is nonmonotonic.
The previously reported reaction balance cannot independently establish a correct
crack-driving force. Original native files have not been revalidated in this audit.</p>
<h2>CalculiX versus Code_Aster</h2>
<table><tr><th>Route</th><th>Evidence and disposition</th></tr>
<tr><td>CalculiX 2.21</td><td>The public manual section 6.9.27 and
*CRACK PROPAGATION describe an empirical propagation route using uncracked
stresses and shape factors. The existing cracked-body stress extrapolation
misses its research comparator. A validated native contour-integral route has
not been established here; this is not a claim about all solver versions.</td></tr>
<tr><td>Code_Aster, DocAster v18</td><td>U4.82.03 sections 1 and 3 document CALC_G
energy release by the theta method, local three-dimensional calculations, and
integration domains controlled by R_INF/R_SUP. The documentation advises comparing
integration domains and smoothing choices. Documented capability supports a next
investigation; no installation, native benchmark result, mesh cost or accuracy
comparison is established here.</td></tr></table>
<p>Table 3. Capability documentation and the limits of the available execution evidence.</p>
<h2>Recommendation and way forward</h2>
<p>The present CalculiX extrapolation route should remain research-only. Code_Aster
CALC_G should be investigated next using the identical geometry, loading, material
and front-angle convention. Solver choice remains NOT_EVALUATED until native
readback establishes front coverage, mesh refinement and integration-domain
sensitivity against a predeclared research comparator. The free-surface endpoint
requires separate interpretation from interior front points. J inferred from K
would only be an elastic identity, not an independent contour check.</p>
<p>No API 579 qualification, plastic-J accuracy, mixed-mode validity, toughness
margin, weld applicability or licensed-solver agreement is established. The linked
licensed comparison is not a matching plate benchmark and requires a common case
and input basis before comparison. No go verdict or follow-on issue is issued.</p>
<h2>Reproduction and traceability</h2>
<pre>python scripts/studies/surface_crack_verdict.py --results docs/domains/asset-integrity/surface-crack-calculix-results.json --relative-limit 0.05 --sources &lt;source-receipts.json&gt; --output-json &lt;audit.json&gt; --output-html &lt;analysis.html&gt;</pre>
<p>The relative limit repeats the historical research input only. The source
receipts can be recovered from the sources field of the committed audit JSON.
The command reads historical evidence and starts no solver or network request.
Tests also cover invalid values, missing endpoints, failed runs and exact
synthetic agreement that still cannot qualify a solver.</p>
<p>Input SHA-256: <code>$DIGEST</code>. Source and receipt digests are in the
companion JSON. Retrieval responses remain external working evidence; local
retention is not backup or publication. No dataset is reallocated. The existing
catalog and domain index were unavailable in the inspected checkout; no replacement
registry is created. The report and code still require independent review.</p>
<h2>Sources</h2><ul>$SOURCES</ul>
<p>Only equations, derived arithmetic and capability summaries are used; no source
prose, figures, licensed standards or raw native solver files are republished.
The first attempted legacy Code_Aster documentation addresses returned HTTP 404;
the current DocAster addresses below returned HTTP 200 on the recorded date.</p>
</html>
'''


def render(document):
    """Render the audited data; report comparisons use one decimal place."""
    from string import Template

    result = document["assessment"]
    rows = []
    for run in result["runs"]:
        values = [str(run[key]) for key in ("refinement", "elements", "nodes")]
        values += [f'{run[key]:.1f}' for key in ("wall_seconds", "peak_rss_kib")]
        values += [("NOT_EVALUATED" if run[key] is None else f'{100*run[key]:.1f}')
                   for key in ("deepest_difference", "surface_difference",
                               "max_abs_front_difference")]
        rows.append("<tr>" + "".join(f"<td>{v}</td>" for v in values) + "</tr>")
    # A missing final run must not be rendered as zero or as a successful result.
    last = result["runs"][-1]
    def percent(key):
        return "NOT_EVALUATED" if last[key] is None else f'{100*last[key]:.1f}'
    sources = "".join(
        f'<li><a href="{html.escape(s["url"], quote=True)}">'
        f'{html.escape(s["title"])}</a>; {html.escape(s["edition"])}; '
        f'retrieved {html.escape(s["retrieved_utc"])}; HTTP {s["http_status"]}. '
        f'{html.escape(s["reuse"])}</li>' for s in document["sources"])
    return Template(REPORT).substitute(
        RECORDED=result["recorded_runs"], COMPARED=result["runs_with_front_comparison"],
        EXCEEDED=sum(r["comparison"] == "EXCEEDS_RESEARCH_COMPARATOR"
                     for r in result["runs"]),
        WITHIN=sum(r["comparison"] == "WITHIN_RESEARCH_COMPARATOR"
                   for r in result["runs"]),
        LIMIT=f'{100*result["relative_limit"]:.1f}', ROWS="\n".join(rows),
        DEEP=percent("deepest_difference"), SURFACE=percent("surface_difference"),
        MAXIMUM=percent("max_abs_front_difference"),
        KDEEP=("NOT_EVALUATED" if last["deepest_k_mpa_sqrt_m"] is None
               else f'{last["deepest_k_mpa_sqrt_m"]:.1f}'),
        COMPARISON=last["comparison"],
        DIGEST=html.escape(document["input_sha256"]), SOURCES=sources,
    )


def main():
    """Write a reproducible audit and HTML report from recorded inputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results", type=Path, required=True)
    parser.add_argument("--relative-limit", type=float, required=True)
    parser.add_argument("--sources", type=Path, required=True)
    parser.add_argument("--output-json", type=Path, required=True)
    parser.add_argument("--output-html", type=Path, required=True)
    args = parser.parse_args()
    inputs = {args.results.resolve(), args.sources.resolve()}
    outputs = {args.output_json.resolve(), args.output_html.resolve()}
    if len(outputs) != 2 or inputs & outputs:
        raise ValueError("input and output paths must be distinct")
    raw = args.results.read_bytes()
    document = {
        "input_sha256": hashlib.sha256(raw).hexdigest(),
        "input_locator": args.results.name,
        "reference_implementation": {
            "path": "src/digitalmodel/asset_integrity/assessment/crack_fad.py",
            "sha256": hashlib.sha256((Path(__file__).resolve().parents[2] /
                "src/digitalmodel/asset_integrity/assessment/crack_fad.py"
            ).read_bytes()).hexdigest(),
        },
        "sources": json.loads(args.sources.read_text()),
        "assessment": audit(json.loads(raw), relative_limit=args.relative_limit),
    }
    args.output_json.write_text(json.dumps(document, indent=2, allow_nan=False) + "\n")
    args.output_html.write_text(render(document))


if __name__ == "__main__":
    main()
