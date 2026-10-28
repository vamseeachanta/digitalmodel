# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build the offshore resilience module analysis from synthetic public calls.

Basis S1: issue 3308 synthetic arithmetic specification. No engineering default
is qualified by this report. Scratch shall resolve outside the checkout.
"""

import argparse
import ast
import hashlib
import html
import json
from dataclasses import asdict, replace
from pathlib import Path
from typing import Any

from digitalmodel.structural.offshore_resilience.installation_checklist import (
    ChecklistItem,
    InstallationChecklist,
)
from digitalmodel.structural.offshore_resilience.minimum_facility import (
    InstallationMethod,
    MinimumFacilityPlatform,
)

Payload = dict[str, Any]

ROOT = Path(__file__).resolve().parents[2]
MODULE_ROOT = "src/digitalmodel/structural/offshore_resilience"
MODULES = ("minimum_facility", "installation_checklist", "lifecycle_planning", "structural_health")
DESTINATION = ROOT / "docs/reports/modules/offshore_resilience"
BUILDER = "scripts/reports/build_offshore_resilience_module_report.py"
SOURCES = (
    BUILDER,
    *(f"{MODULE_ROOT}/{name}.py" for name in MODULES),
    "tests/structural/offshore_resilience/test_module_report_hand_values.py",
    "tests/reporting/test_offshore_resilience_module_report.py",
)
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "Initial module inventory and computed synthetic weight, midpoint and checklist examples, so arithmetic behavior can be assessed without adopting unqualified engineering criteria.",
}]

# S1 specifies the arithmetic inputs and both capacity cases. The other fields
# are explicit synthetic constructor controls selected for software execution;
# no physical design life, slot requirement or installation method is inferred.
FACILITY_INPUTS: Payload = {
    "platform_type": "wellhead", "topsides_weight_tonnes": 120,
    "jacket_weight_tonnes": 80, "water_depth_range_ft": (100, 300),
    "installation_method": InstallationMethod.CRANE_BARGE,
    "max_crane_capacity_tonnes": 200, "well_slots": 1,
    "design_life_years": 1, "expansion_capability": False,
}
CAPACITIES_TONNES = (200, 199)  # S1 inclusive comparison and failing control.
INTERVAL_M = (2, 4)  # S1 supplied synthetic interval; no operational criterion.
MIXED_VALUES_M = (2, 5, None)  # S1 lower bound, outside interval, not entered.
ALL_PASS_VALUES_M = (2, 3, 4)  # S1 within-interval controls, including both bounds.


def checklist_snapshot(checklist: InstallationChecklist, name: str) -> Payload:
    """Retain the returned state before further values are entered."""
    return {
        "name": name,
        "items": [{"parameter": item.parameter, "actual_value": item.actual_value,
                   "status": item.status.value} for item in checklist.items],
        "summary": checklist.summary(),
        "completion_ratio": checklist.completion_ratio,
        "is_complete": checklist.is_complete,
    }


def calculate() -> Payload:
    """Execute only the synthetic facility and direct-item checklist examples."""
    facility = MinimumFacilityPlatform(**FACILITY_INPUTS)
    inputs = asdict(facility)
    inputs["water_depth_range_ft"] = list(facility.water_depth_range_ft)
    inputs["installation_method"] = facility.installation_method.value
    items = [ChecklistItem("synthetic", key, "Synthetic interval control",
                           *INTERVAL_M, "m")
             for key in ("check_a", "check_b", "check_c")]
    checklist = InstallationChecklist("Synthetic interval checks", items)
    for item, value in zip(items, MIXED_VALUES_M, strict=True):
        if value is not None:
            item.set_value(value)
    mixed = checklist_snapshot(checklist, "mixed")
    for item, value in zip(items, ALL_PASS_VALUES_M, strict=True):
        item.set_value(value)
    return {
        "facility": {
            "inputs": inputs,
            "total_weight_tonnes": facility.total_weight_tonnes,
            "water_depth_midpoint_ft": facility.water_depth_midpoint_ft,
            "crane_cases": [{"capacity_tonnes": capacity,
                             "crane_installable": replace(
                                 facility, max_crane_capacity_tonnes=capacity,
                             ).crane_installable} for capacity in CAPACITIES_TONNES],
        },
        "checklist": {"allowed_interval_m": list(INTERVAL_M), "states": [
            mixed, checklist_snapshot(checklist, "all_pass"),
        ]},
    }


def method_assessment(module: str, name: str) -> tuple[str, str]:
    """Restrict hand qualification to the arithmetic actually exercised."""
    hand_checked = {
        "MinimumFacilityPlatform.total_weight_tonnes",
        "MinimumFacilityPlatform.crane_installable",
        "MinimumFacilityPlatform.water_depth_midpoint_ft",
        "ChecklistItem.set_value", "InstallationChecklist.pass_count",
        "InstallationChecklist.fail_count", "InstallationChecklist.pending_count",
        "InstallationChecklist.completion_ratio", "InstallationChecklist.is_complete",
        "InstallationChecklist.summary",
    }
    if name in hand_checked:
        return "HAND_CHECKED_SYNTHETIC", "S1 hand arithmetic and enumeration only; engineering applicability is NOT_EVALUATED."
    limitations = {
        "minimum_facility": "Regulatory category criteria, cost basis and lifecycle scoring calibration are missing. Weight, capacity and control-field domain validation is incomplete; invalid and non-finite inputs are unqualified.",
        "installation_checklist": "Factory intervals lack an approved installation basis. Direct items do not validate interval ordering or finiteness; empty-checklist semantics and manual status mutation are unqualified.",
        "lifecycle_planning": "Decommissioning criteria, score weights, score boundaries and tieback calibration require an approved basis. Nonnegative slot counts and removal-strategy validation are not enforced.",
        "structural_health": "Sensor locations, sampling and alert criteria require calibration and an approved fatigue basis. Timezone awareness, finite values and positive design life are not fully enforced. Delegated sensor helpers are outside this review.",
    }
    return "NOT_EVALUATED", limitations[module]


def method_inventory() -> list[dict[str, str]]:
    """Inventory public functions, classes and methods in the four source files.

    Classes include dataclass construction and enum interfaces; explicit
    post-initialization validators are included even though their names are private.
    """
    rows = []
    for module in MODULES:
        source = f"{MODULE_ROOT}/{module}.py"
        tree = ast.parse((ROOT / source).read_text(encoding="utf-8"))
        for node in tree.body:
            if not isinstance(node, (ast.ClassDef, ast.FunctionDef)) or node.name.startswith("_"):
                continue
            names = [node.name]
            if isinstance(node, ast.ClassDef):
                names.extend(f"{node.name}.{method.name}" for method in node.body
                             if isinstance(method, ast.FunctionDef) and (
                                 not method.name.startswith("_") or method.name == "__post_init__"))
            for name in names:
                status, limitation = method_assessment(module, name)
                rows.append({"method": f"{module}.{name}", "source": source,
                             "status": status, "limitation": limitation})
    return rows


def text(value: object) -> str:
    """Escape a visible cell value without introducing rounded decisions."""
    if isinstance(value, list):
        value = ", ".join(str(item) for item in value)
    elif isinstance(value, float) and value.is_integer():
        value = int(value)
    return html.escape(str(value))


def cell(field: str, value: object) -> str:
    """Identify computed display fields for independent readback tests."""
    return f'<td id="{text(field)}">{text(value)}</td>'


def input_rows(inputs: Payload) -> str:
    """Display all committed synthetic constructor fields with units and basis."""
    units = {
        "topsides_weight_tonnes": "t", "jacket_weight_tonnes": "t",
        "water_depth_range_ft": "ft", "max_crane_capacity_tonnes": "t",
        "well_slots": "count", "design_life_years": "years",
    }
    controls = {"platform_type", "installation_method", "well_slots",
                "design_life_years", "expansion_capability"}
    return "".join(
        f'<tr><td>{text(key)}</td>{cell(f"input-{key}", value)}'
        f'<td>{units.get(key, "dimensionless")}</td><td>'
        f'{"Synthetic constructor control; not a design basis [S1]." if key in controls else "Synthetic arithmetic input [S1]."}</td></tr>'
        for key, value in inputs.items()
    )


def checklist_rows(states: list[Payload]) -> tuple[str, str]:
    """Format counts, passed fraction and every item from public return values."""
    summaries, items = [], []
    for state in states:
        name = state["name"]
        counts = state["summary"]
        ratio = f'{counts["pass"]}/{counts["total"]} ({state["completion_ratio"]:.1%})'
        summaries.append(f'<tr><td>{name}</td>' + "".join(
            cell(f"{name}-{key}", value) for key, value in counts.items()
        ) + cell(f"{name}-ratio", ratio) + cell(f"{name}-is-complete", state["is_complete"])
            + '<td>[S1, S3]</td></tr>')
        for item in state["items"]:
            prefix = f'{name}-{item["parameter"]}'
            value = "NOT_ENTERED" if item["actual_value"] is None else item["actual_value"]
            items.append(f'<tr><td>{name}</td><td>{text(item["parameter"])}</td>'
                         + cell(f"{prefix}-value", value)
                         + cell(f"{prefix}-status", item["status"])
                         + '<td>[S1, S3]</td></tr>')
    return "".join(summaries), "".join(items)


def summary_text(data: Payload) -> str:
    """Bind summary conclusions to the computed results and supplied criteria."""
    facility = data["facility"]
    capacity = "; ".join(
        f'{row["capacity_tonnes"]} t: {"PASS" if row["crane_installable"] else "FAIL"}'
        for row in facility["crane_cases"]
    )
    mixed, final = data["checklist"]["states"]
    fractions = [f'{state["summary"]["pass"]}/{state["summary"]["total"]} '
                 f'({state["completion_ratio"]:.1%})' for state in (mixed, final)]
    return (f'The synthetic combined mass is {text(facility["total_weight_tonnes"])} t '
            f'and depth midpoint is {text(facility["water_depth_midpoint_ft"])} ft. '
            f'The supplied inclusive capacity comparisons are {capacity}. '
            f'The mixed passed fraction is {fractions[0]}; '
            f'the final passed fraction is {fractions[1]} [S1–S3, S6].')


def render(data: Payload) -> str:
    """Render the established ten-section report around computed result cells."""
    facility, checklist = data["facility"], data["checklist"]
    inputs = facility["inputs"]
    summaries, items = checklist_rows(checklist["states"])
    capacity_rows = "".join(
        f'<tr><td>Crane comparison at {row["capacity_tonnes"]} t</td>'
        + cell(f'crane-{row["capacity_tonnes"]}', "PASS" if row["crane_installable"] else "FAIL")
        + f'<td>t</td><td>{facility["total_weight_tonnes"]} ≤ {row["capacity_tonnes"]}</td>'
        '<td>[S1, S2]</td></tr>' for row in facility["crane_cases"]
    )
    inventory = "".join(
        f'<tr><td><code>{row["method"]}</code></td><td>{row["status"]}</td>'
        f'<td>{text(row["limitation"])}</td><td><code>{row["source"]}</code></td></tr>'
        for row in method_inventory()
    )
    revisions = "".join(f'<tr><td>{row["revision"]}</td><td>{row["date"]}</td>'
                        f'<td>{text(row["change"])}</td></tr>' for row in CHANGES)
    sources = "".join(f'<li><code>{source}</code>; SHA-256: '
                      f'{hashlib.sha256((ROOT / source).read_bytes()).hexdigest()}</li>'
                      for source in SOURCES)
    lower, upper = checklist["allowed_interval_m"]
    mixed_values = ", ".join("NOT_ENTERED" if item["actual_value"] is None
                             else str(item["actual_value"])
                             for item in checklist["states"][0]["items"])
    page = REPORT_TEMPLATE.format(
        revision=CHANGES[-1]['revision'],
        date=CHANGES[-1]['date'],
        summary=summary_text(data),
        revisions=revisions,
        data_rows=input_rows(inputs),
        lower=lower,
        upper=upper,
        mixed_values=mixed_values,
        weight_cell=cell('total-weight', facility['total_weight_tonnes']),
        topsides=inputs['topsides_weight_tonnes'],
        steel=inputs['jacket_weight_tonnes'],
        midpoint_cell=cell('depth-midpoint', facility['water_depth_midpoint_ft']),
        depth_lower=inputs['water_depth_range_ft'][0],
        depth_upper=inputs['water_depth_range_ft'][1],
        capacity_rows=capacity_rows,
        summaries=summaries,
        items=items,
        inventory=inventory,
        sources=sources,
        change=text(CHANGES[-1]['change']),
    )
    return "\n".join(line.rstrip() for line in page.splitlines()) + "\n"


REPORT_TEMPLATE = '''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Offshore resilience module analysis</title><style>
:root {{ color-scheme:light; --brand:#164e63; --line:#b9cbd1; }}
body {{ margin:0; color:#18323d; background:#f3f6f7; font:16px/1.55 system-ui,sans-serif; }}
main {{ max-width:1120px; margin:auto; padding:2rem; background:white; }}
h1,h2,h3 {{ color:var(--brand); }} h2 {{ border-bottom:2px solid var(--line); padding-top:1rem; }}
table {{ width:100%; border-collapse:collapse; margin:1rem 0 .25rem; }}
th,td {{ border:1px solid var(--line); padding:.5rem; text-align:left; vertical-align:top; overflow-wrap:anywhere; }}
th {{ background:#e8f0f3; }} code {{ font-size:.85em; overflow-wrap:anywhere; }}
.notice,details {{ border-left:4px solid var(--brand); padding:1rem; background:#eef5f7; }}
textarea {{ width:100%; box-sizing:border-box; }} button {{ padding:.6rem; }}
@media print {{ main {{ padding:0; }} button,textarea {{ display:none; }} }}
</style></head><body class="hs-doc"><main>
<h1>Offshore resilience module analysis</h1>
<p>Revision {revision} · {date} · Synthetic arithmetic only [S1]</p>
<h2 id="s1">1. Executive Summary</h2>
<p>{summary}</p>
<p>The facility weight, depth midpoint, inclusive capacity comparison and checklist state arithmetic are calculated through public functions [S2, S3]. The worked results below are checked against independent addition, averaging and enumeration [S1, S6]. No operational approval or regulatory qualification is established.</p>
<h3 id="revision-history">Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{revisions}</tbody></table>
<p>Revision history. Each issued revision retains its earlier rows.</p>
<h2 id="s2">2. Scope and Objectives</h2>
<p>The scope is the public interfaces of minimum_facility, installation_checklist, lifecycle_planning and structural_health. The method inventory records arithmetic evidence and unresolved qualification separately. The calculations exercise explicitly supplied synthetic inputs; the checklist factories, classification helper, cost comparison and lifecycle and sensor defaults are not used to select criteria.</p>
<h2 id="s3">3. Design Data</h2>
<p>All inputs are synthetic [S1]. Constructor controls permit software execution and carry no engineering design basis. No client, measured or operational data are used. Readiness is limited to this arithmetic demonstration.</p>
<table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis and intended use</th></tr></thead><tbody>{data_rows}</tbody></table>
<p>Facility input register. Supporting steel is supplied through jacket_weight_tonnes [S2].</p>
<p>The directly constructed checklist items share the supplied allowed interval [{lower}, {upper}] m. Mixed entries are {mixed_values} m; NOT_ENTERED means pending, not zero [S1]. The later entries are within the same interval. No factory default is substituted.</p>
<h2 id="s4">4. Methodology</h2>
<p>The independent hand basis is addition for combined mass, the arithmetic average of the two depth endpoints for the midpoint, and inclusive comparison for crane capacity [S1]. These are arithmetic definitions, not lifting-design equations. No unit conversion is required.</p>
<p>The interval predicate is lower ≤ entered value ≤ upper. Each item is enumerated as pass, fail or pending. completion_ratio measures passed checks, not the fraction entered: passed count divided by total count. is_complete requires no failed or pending items for this nonempty example [S1, S3].</p>
<p>Integer counts and exactly representable integer-valued results are checked by exact comparison. The hand test permits an absolute dimensionless roundoff allowance of 1e-15 for the mixed-state ratio only [S6]; this is a test allowance for floating-point division, not an engineering tolerance. Display percentages are rounded to one decimal place; decisions use returned values before rounding.</p>
<h2 id="s5">5. Detailed Results</h2>
<table><thead><tr><th>Quantity</th><th>Returned result</th><th>Unit</th><th>Independent hand operation or criterion</th><th>Source</th></tr></thead><tbody>
<tr><td>Combined weight</td>{weight_cell}<td>t</td><td>{topsides} + {steel}</td><td>[S1, S2]</td></tr>
<tr><td>Depth midpoint</td>{midpoint_cell}<td>ft</td><td>({depth_lower} + {depth_upper}) / 2</td><td>[S1, S2]</td></tr>
{capacity_rows}</tbody></table>
<p>Facility arithmetic. PASS and FAIL refer solely to the supplied inclusive mass comparison. Lift radius, dynamic effects, rigging, crane charts, load factors and structural capacity are absent; installation feasibility is NOT_EVALUATED.</p>
<table><thead><tr><th>State</th><th>Total (count)</th><th>Pass (count)</th><th>Fail (count)</th><th>Pending (count)</th><th>Passed fraction (%)</th><th>is_complete</th><th>Source</th></tr></thead><tbody>{summaries}</tbody></table>
<p>Checklist state enumeration. The ratio is the passed fraction. Entered-but-failed checks do not increase it.</p>
<table><thead><tr><th>State</th><th>Parameter</th><th>Entered value (m)</th><th>Returned status</th><th>Source</th></tr></thead><tbody>{items}</tbody></table>
<p>Direct item results against [{lower}, {upper}] m [S1]. The final state exercises the inclusive lower and upper bounds. Passing these synthetic intervals is separate from actual operational approval.</p>
<h2 id="s6">6. Key Findings</h2>
<p>The specified arithmetic agrees with independent hand calculations in the committed tests [S6]. This establishes the demonstrated software behavior only. Existing tests exercise additional branches; their passing status does not supply missing engineering criteria.</p>
<table><thead><tr><th>Public interface</th><th>Qualification status</th><th>Review limitation or missing criterion</th><th>Source</th></tr></thead><tbody>{inventory}</tbody></table>
<p>Public method inventory from the four source modules. Class entries cover construction or enum interfaces; explicit post-initialization validators are retained. HAND_CHECKED_SYNTHETIC is restricted to the displayed cases. NOT_EVALUATED means engineering qualification is not established, not a failed operational assessment [S2–S6].</p>
<h2 id="s7">7. Limitations and Not Covered</h2>
<p>Regulatory classification: NOT_EVALUATED. The approximate screening boundaries have no verified governing edition, clause, jurisdiction or applicability assessment in this analysis. The existing implementation is not used as a regulatory source.</p>
<p>Commercial cost and installation weather assumptions: NOT_EVALUATED. Dated quotations, scope, cost basis and an approved installation method and weather analysis are missing. No replacement cost or weather criterion is selected.</p>
<p>Sensor and alert defaults: NOT_EVALUATED. Calibrated sensor evidence, channel units, sampling requirements, fatigue budget and approved alarm criteria are missing. Template helper contents and real monitoring responses are outside this review.</p>
<p>Lifecycle defaults: NOT_EVALUATED. Approved scoring scales, weights, tieback criteria and jurisdiction-specific decommissioning decisions are missing. Recommendations returned by those methods are not adopted here.</p>
<p>Input validation is incomplete in the reviewed source: non-finite facility inputs, direct-item interval ordering, nonnegative tieback slot counts, timezone awareness and positive alert design life require separate regressions before qualification. Empty checklist semantics are not established by the nonempty examples. These are inspection findings, not executed invalid-input campaigns. No source module is changed.</p>
<h2 id="s8">8. Recommendations</h2>
<p>The way forward is to obtain the missing approved criteria and engineering evidence for each NOT_EVALUATED method, then add independent cases before assigning an operational or regulatory disposition. The input-validation gaps require bounded failing regressions before source fixes. No replacement default, rating or commercial assumption is proposed. The originating issue remains open while methods are unqualified.</p>
<h2 id="s9">9. References and Appendices</h2>
<p>[S1] Issue 3308 synthetic arithmetic specification and the explicit constructor controls committed in this builder. All numerical example inputs originate here; no licensed values are reproduced.</p>
<p>[S2] minimum_facility.py: public facility properties and constructor. [S3] installation_checklist.py: direct item evaluation and checklist summary properties. [S4] lifecycle_planning.py and [S5] structural_health.py: interface inspection only. [S6] test_module_report_hand_values.py: independent arithmetic. [S7] test_offshore_resilience_module_report.py: payload, display, rebuild and digest checks.</p>
<p>The source register below identifies exact source bytes. The manifest binds those sources and the index, changes and README artifacts by SHA-256. It excludes its own hash to avoid self-reference; complete rebuild comparison covers manifest.json itself. No execution timestamp, absolute path or scratch identifier enters a deliverable.</p>
<ol>{sources}</ol>
<details id="changes-panel"><summary>Engineering changes</summary><p>{change}</p></details>
<h2 id="s10">10. Acronyms and Review Notes</h2>
<p>t: metric tonne; ft: foot; m: metre; BSEE: Bureau of Safety and Environmental Enforcement; SHA-256: content digest algorithm. PASS and FAIL are software predicate outcomes. NOT_EVALUATED identifies a missing qualification basis.</p>
<label for="notes">Review notes</label><textarea id="notes" rows="5" placeholder="Enter engineering review comments"></textarea>
<button id="save-notes" type="button">Download review notes</button><p id="save-status" aria-live="polite">Notes remain in this page until a download is requested.</p>
</main><script>
document.getElementById('save-notes').addEventListener('click', () => {{
  const blob = new Blob([document.getElementById('notes').value], {{type:'text/plain'}});
  const url = URL.createObjectURL(blob), link = document.createElement('a');
  link.href = url; link.download = 'review-notes.txt'; link.click();
  URL.revokeObjectURL(url);
  document.getElementById('save-status').textContent = 'Download requested; confirm the notes file is present.';
}});
</script></body></html>
'''


def readme() -> str:
    """Document the exact retained outputs and their limited intended use."""
    return """# Offshore resilience module analysis

[Read the computed module analysis](index.html).

The synthetic basis is issue 3308: combined mass by addition, depth midpoint
by averaging and supplied-interval checklist states by independent enumeration.
Constructor controls are synthetic, not a physical design basis. The completion
ratio measures passed checks, not the fraction entered. Engineering criteria
for regulatory screening, costs, sensors and lifecycle decisions remain
NOT_EVALUATED; the report identifies the missing sources and decisions.

Rebuild from the repository root with the source import path enabled:

```bash
PYTHONPATH=src uv run --no-sync python scripts/reports/build_offshore_resilience_module_report.py --scratch "$TMPDIR/offshore-resilience-build"
```

The scratch argument shall resolve outside the checkout.
The `--output-dir` option selects an independent output directory; by default the four
deliverables are written beside this README. No intermediate files are needed.
Inside the checkout, only the named report directory is permitted as output.
The calculation uses the Python standard library and the source package only.

`index.html` contains the calculated results and public-interface inventory.
`changes.json` retains the revision history. `manifest.json` carries SHA-256
digests for the builder, four reviewed modules, hand and report tests, and the
three other artifacts. Its own hash is excluded to avoid circular hashing;
the rebuild test compares all four delivered files byte for byte.

The independent hand checks are in
`tests/structural/offshore_resilience/test_module_report_hand_values.py`.
The rebuild, payload, display and digest checks are in
`tests/reporting/test_offshore_resilience_module_report.py`.
Passing arithmetic cases do not establish operational approval.
"""


def build(scratch: Path, output_dir: Path = DESTINATION) -> None:
    """Write deterministic UTF-8/LF artifacts and their exact-byte manifest."""
    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    output_dir = Path(output_dir).resolve()
    if (output_dir == ROOT or ROOT in output_dir.parents) and output_dir != DESTINATION:
        raise ValueError("Output inside the checkout must be the named report directory")
    for name in ("index.html", "changes.json", "README.md", "manifest.json"):
        if (output_dir / name).is_symlink():
            raise ValueError("Output targets must not be symlinks")
    outputs = {"index.html": render(calculate()),
               "changes.json": json.dumps(CHANGES, indent=2) + "\n",
               "README.md": readme()}
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, content in outputs.items():
        (output_dir / name).write_text(content, encoding="utf-8", newline="\n")
    manifest = {
        "schema_version": 1,
        "digest_convention": "SHA-256 of exact UTF-8/LF bytes; manifest excludes its own digest to avoid self-reference.",
        "sources": {source: hashlib.sha256((ROOT / source).read_bytes()).hexdigest()
                    for source in SOURCES},
        "artifacts": {name: hashlib.sha256((output_dir / name).read_bytes()).hexdigest()
                      for name in outputs},
    }
    (output_dir / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8", newline="\n",
    )


def main() -> None:
    """Accept explicit scratch and optional report destination arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", required=True, type=Path)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
