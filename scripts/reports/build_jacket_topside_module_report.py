# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the bounded jacket/topside module analysis from synthetic inputs.

Only geometry and zero-demand arithmetic have an independent basis here.
Run with source imports enabled and calculation scratch outside the checkout.
"""

import argparse
import hashlib
import html
import json
import math
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = ROOT / "docs/reports/modules/jacket_topside"
CHANGES = [{
    "revision": "A",
    "date": "2026-10-28",
    "change": "Initial module review and independent geometry calculation; "
              "capacity qualification gaps are retained to bound engineering use.",
}]
SOURCE_ITEMS = [
    ("src/digitalmodel/structural/jacket_topside/docs/worked-tubular-section-geometry.md",
     "Independent radial integrals, property definitions, synthetic dimensions and rounding policy."),
    ("tests/structural/jacket_topside/test_documented_tubular_section_geometry.py",
     "Execution of the documented example and dimensional scaling check."),
    ("src/digitalmodel/structural/jacket_topside/member_checks.py",
     "Public section properties and member checks; implementation evidence only for allowables."),
    ("src/digitalmodel/structural/jacket_topside/joint_checks.py",
     "Public joint ratios, simplified classification and punching shear check."),
    ("src/digitalmodel/structural/jacket_topside/workflow.py",
     "JacketChecksWorkflow.router and result/file adaptation; implementation review only."),
    ("src/digitalmodel/structural/jacket_topside/dented_axial_screen.py",
     "Dimensionless dent-screen applicability and result contract; cited study not reopened here."),
    ("src/digitalmodel/structural/jacket_topside/damaged_member_screen.py",
     "Damaged-member diagnostics, campaign escalation and command entry point."),
    ("tests/structural/jacket_topside/test_jacket_topside.py",
     "Existing member/joint implementation assertions; capacity coefficients are not independent evidence."),
    ("tests/structural/jacket_topside/test_module_report_hand_values.py",
     "Independent annulus expressions, synthetic joint divisions and zero-demand limits."),
    ("tests/structural/jacket_topside/test_tension_scaling.py",
     "Existing synthetic material/length assumptions and stress scaling; no allowable qualification."),
    ("tests/reporting/test_jacket_topside_module_report.py",
     "Complete payload, unit, qualification boundary, rebuild and digest checks."),
    ("scripts/reports/build_jacket_topside_module_report.py",
     "Committed synthetic inputs, public-function evaluation and deterministic rendering."),
    ("tests/structural/jacket_topside/test_dented_axial_screen.py",
     "Existing study-equation and applicability tests; reviewed as implementation evidence."),
    ("tests/structural/jacket_topside/test_damaged_member_screen.py",
     "Existing corrosion arithmetic, escalation, strict JSON and campaign tests."),
    ("src/digitalmodel/structural/jacket_topside/__init__.py",
     "Public exports of member and joint data structures and functions."),
]
SOURCES = [path for path, _ in SOURCE_ITEMS]

# All inputs are synthetic assumptions, not measurements or steel-grade data.
# Geometry: [1, 9]. Positive material, modulus and effective length: [10].
# Joint dimensions and zero demands: independent hand case [9]. Cm=1 is an
# explicit synthetic factor unused by the zero-demand high-axial branch.
INPUTS = {
    "section": {
        "outer_diameter": {"value": 12.0, "unit": "in"},
        "wall_thickness": {"value": 0.5, "unit": "in"},
        "yield_strength": {"value": 40.0, "unit": "ksi"},
        "elastic_modulus": {"value": 28_000.0, "unit": "ksi"},
        "effective_length": {"value": 120.0, "unit": "in"},
    },
    "member_loads": {
        "fa": {"value": 0.0, "unit": "ksi"},
        "fb": {"value": 0.0, "unit": "ksi"},
        "Cm": {"value": 1.0, "unit": "1"},
        "is_tension": {"value": [False, True], "unit": "boolean"},
    },
    "joint": {
        "chord_od": {"value": 12.0, "unit": "in"},
        "chord_wt": {"value": 0.5, "unit": "in"},
        "brace_ods": {"value": [6.0], "unit": "in"},
        "brace_wts": {"value": [0.25], "unit": "in"},
        "brace_angles": {"value": [90.0], "unit": "deg"},
        "gap": {"value": 0.0, "unit": "in"},
        "braces_on_same_side": {"value": True, "unit": "boolean"},
        "chord_fy": {"value": 40.0, "unit": "ksi"},
        "brace_index": {"value": 0, "unit": "index"},
        "chord_utilization": {"value": 0.0, "unit": "1"},
    },
    "joint_loads": {
        "axial_load": {"value": 0.0, "unit": "kip"},
        "ipb_moment": {"value": 0.0, "unit": "kip-in"},
        "opb_moment": {"value": 0.0, "unit": "kip-in"},
    },
}

# Inventory is deliberately broader than the independently checked subset.
METHODS = [
    ("TubularSection: inner_diameter, cross_sectional_area, moment_of_inertia, "
     "section_modulus, radius_of_gyration", "Independent geometry",
     "Annulus integrals and definitions [1, 2, 9]."),
    ("TubularSection: slenderness_ratio, dt_ratio, gamma", "Existing implementation tests",
     "Ratio assertions [8, 10]; no additional qualification claimed here."),
    ("JointGeometry: gamma, beta, tau, num_braces; classify_joint", "Independent geometry",
     "Direct division and single-brace T/Y label only [4, 9]. Multi-brace classification remains simplified."),
    ("axial_tension_uc, axial_compression_uc, bending_uc", "Zero-demand arithmetic",
     "Zero numerator at finite positive returned capacity [9]; nonzero capacities NOT_EVALUATED."),
    ("combined_axial_bending_uc; member_checks (both axial flags and governing selection)",
     "Zero-demand arithmetic", "No nonzero interaction equation qualified [3, 9]."),
    ("punching_shear_uc", "Zero-demand arithmetic",
     "Nondegenerate single-brace geometry [4, 9]; nonzero capacity NOT_EVALUATED."),
    ("MemberLoads, MemberCheckResult, JointLoads, JointCheckResult, JointType",
     "Existing implementation tests", "Data and enum contracts [8, 15]; no independent material basis."),
    ("JacketChecksWorkflow.router", "Implementation review only",
     "Review of adaptation and output semantics [5]; PASS means the implementation threshold only."),
    ("DentedAxialInput, DentedAxialResult, SourceReference; screen_dented_axial",
     "Existing implementation tests", "Study envelope and hand assertions exist [6, 13]; "
     "external study evidence not reopened, engineering acceptance NOT_EVALUATED."),
    ("screen_member; screen_campaign; damaged_member_screen.main",
     "Existing implementation tests", "Corrosion diagnostics, JSON/HTML output and escalation [7, 14]; "
     "strength acceptance NOT_EVALUATED."),
    ("Member and joint nonzero strength, interaction and damage acceptance",
     "Unqualified standard allowables", "NOT_EVALUATED: independent source and applicability review required."),
]


def json_text(value):
    """Serialize strict JSON with stable ordering, indentation and final newline."""
    return json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n"


def raw_values(block):
    """Extract the explicitly specified public-function arguments."""
    return {key: row["value"] for key, row in block.items()}


def zero_row(applied, utilisation, capacity):
    """Retain arithmetic evidence while withholding numeric unqualified capacity."""
    finite_positive = math.isfinite(capacity) and capacity > 0.0
    if not finite_positive:
        raise ValueError("Synthetic zero-demand case requires finite positive capacity")
    return {
        "applied_demand": {"value": applied, "unit": "ksi"},
        "utilisation": {"value": utilisation, "unit": "1"},
        "finite_positive_capacity": finite_positive,
        "status": "ZERO_DEMAND_ARITHMETIC_ONLY",
        "capacity_status": "NOT_EVALUATED",
        "source": "[3, 4, 9]",
    }


def calculate(scratch):
    """Evaluate public member/joint functions and write intermediate strict JSON."""
    from digitalmodel.structural.jacket_topside import (
        JointGeometry,
        JointLoads,
        MemberLoads,
        TubularSection,
        classify_joint,
        member_checks,
        punching_shear_uc,
    )

    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    # Copy inputs so callers cannot mutate the committed assumptions via payloads.
    inputs = json.loads(json_text(INPUTS))
    section = TubularSection(**raw_values(inputs["section"]))
    units = {
        "inner_diameter": "in", "cross_sectional_area": "in^2",
        "moment_of_inertia": "in^4", "section_modulus": "in^3",
        "radius_of_gyration": "in",
    }
    annulus = {name: {"value": getattr(section, name), "unit": unit,
                      "source": "[1, 2, 3]"} for name, unit in units.items()}
    joint_args = raw_values(inputs["joint"])
    chord_fy = joint_args.pop("chord_fy")
    brace_index = joint_args.pop("brace_index")
    chord_utilization = joint_args.pop("chord_utilization")
    geometry = JointGeometry(**joint_args)
    ratios = {
        "gamma": (geometry.gamma, "1"), "beta": (geometry.beta, "1"),
        "tau": (geometry.tau, "1"), "brace_count": (geometry.num_braces, "1"),
        "classification": (classify_joint(geometry).value, "label"),
    }
    joint_geometry = {key: {"value": value, "unit": unit, "source": "[4, 9]"}
                      for key, (value, unit) in ratios.items()}
    zero = {}
    member_loads = raw_values(inputs["member_loads"])
    axial_flags = member_loads.pop("is_tension")
    for flag in axial_flags:
        results = member_checks(section, MemberLoads(**member_loads, is_tension=flag))
        branch = "tension" if flag else "compression"
        for name, result in results.items():
            zero[f"{branch}/{name}"] = zero_row(
                result.applied_stress, result.unity_check, result.allowable_stress
            )
    joint_result = punching_shear_uc(
        geometry, JointLoads(**raw_values(inputs["joint_loads"])),
        chord_fy=chord_fy, brace_index=brace_index, chord_utilization=chord_utilization,
    )
    zero["joint/punching_shear"] = zero_row(
        joint_result.applied_vp, joint_result.unity_check, joint_result.allowable_vp
    )
    data = {
        "inputs": inputs, "annulus": annulus, "joint_geometry": joint_geometry,
        "zero_demand": zero,
        "assessments": dict.fromkeys([
            "member_nonzero_capacity", "joint_nonzero_capacity",
            "damaged_member_acceptance", "standards_compliance",
        ], "NOT_EVALUATED"),
    }
    scratch.mkdir(parents=True, exist_ok=True)
    (scratch / "calculation.json").write_text(json_text(data), encoding="utf-8", newline="\n")
    return data


def display(value):
    """Use one decimal place for numerical displays, retaining exact input fractions."""
    if isinstance(value, bool):
        return str(value).lower()
    if isinstance(value, (int, float)):
        return f"{value:.1f}"
    if isinstance(value, list):
        return "[" + ", ".join(display(item) for item in value) + "]"
    return html.escape(str(value))


def input_display(value):
    """Keep assumed dimensions exact when a one-decimal display would change them."""
    # The quarter-inch brace wall is a synthetic input [9], not a result.
    if value == 0.25 and not isinstance(value, (bool, list)):
        return "1/4"
    if isinstance(value, list):
        return "[" + ", ".join(input_display(item) for item in value) + "]"
    return display(value)


def history_rows():
    """Render the immutable issue history from the same change records as JSON."""
    return "".join(
        f'<tr><td>{html.escape(row["revision"])}</td><td>{row["date"]}</td>'
        f'<td>{html.escape(row["change"])}</td></tr>' for row in CHANGES
    )


def render(scratch):
    """Render the established ten-section report around the computed payload."""
    data = calculate(scratch)
    inputs = "".join(
        f'<tr><td>{group}/{key}</td><td>{input_display(row["value"])}</td>'
        f'<td>{row["unit"]}</td><td>Synthetic assumption [1, 9, 10, 12]</td></tr>'
        for group, block in data["inputs"].items() for key, row in block.items()
    )
    expressions = {
        "inner_diameter": "2a = 11", "cross_sectional_area": "23*pi/4",
        "moment_of_inertia": "6095*pi/64", "section_modulus": "6095*pi/384",
        "radius_of_gyration": "sqrt(265)/4",
    }
    annulus = "".join(
        f'<tr><td>{key}</td><td>{expressions[key]}</td><td>{display(row["value"])}</td>'
        f'<td>{row["unit"]}</td><td>{row["source"]}</td></tr>'
        for key, row in data["annulus"].items()
    )
    joint = "".join(
        f'<tr><td>{key}</td><td>{display(row["value"])}</td><td>{row["unit"]}</td>'
        f'<td>{row["source"]}</td></tr>' for key, row in data["joint_geometry"].items()
    )
    zeros = "".join(
        f'<tr><td>{name}</td><td>{display(row["applied_demand"]["value"])}</td>'
        f'<td>{display(row["utilisation"]["value"])}</td>'
        f'<td>{row["status"]}; capacity {row["capacity_status"]}</td>'
        f'<td>{row["source"]}</td></tr>' for name, row in data["zero_demand"].items()
    )
    methods = "".join(
        f'<tr><td><code>{html.escape(name)}</code></td><td>{category}</td><td>{basis}</td></tr>'
        for name, category, basis in METHODS
    )
    sources = "".join(
        f'<li><code>{path}</code>: {description}<br>SHA-256: '
        f'{hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}</li>'
        for path, description in SOURCE_ITEMS
    )
    payload = json_text(data).replace("<", "\\u003c")
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Jacket and topside module analysis</title>
<style>
:root {{color-scheme:light; font-family:system-ui,sans-serif; color:#172b46; background:#f3f6fa}}
body {{margin:0}} main {{max-width:1100px; margin:auto; padding:2rem; background:white}}
h1,h2,h3 {{line-height:1.3}} p,li {{line-height:1.6}} h2 {{border-bottom:2px solid #bfd4e7; padding-bottom:.5rem}}
table {{border-collapse:collapse; width:100%; margin:1rem 0; font-size:.9rem}}
th,td {{text-align:left; border:1px solid #bfd4e7; padding:.6rem; overflow-wrap:anywhere}}
th {{background:#eaf1f8}} code {{overflow-wrap:anywhere}} .notice,details {{padding:1rem; background:#eaf1f8}}
.scroll {{overflow-x:auto}} a {{color:#134c7d}} li {{margin-bottom:.7rem}}
@media(max-width:600px) {{main {{padding:1rem}} table {{font-size:.8rem}}}}
</style></head><body><main>
<header><h1>Jacket and topside module analysis</h1>
<p>Structural tubular geometry, software limits and evidence required for capacity assessment.</p></header>
<h2 id="s1">1. Executive Summary</h2>
<p>Independent annulus integrals and direct joint ratios support the geometric results.
Zero-demand checks establish demand arithmetic at finite positive returned capacity only.
Nonzero member capacity, joint capacity and damage acceptance remain NOT_EVALUATED.
No design compliance is certified [1–4, 9].</p>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody>{history_rows()}</tbody></table>
<details id="changes-panel"><summary>Change panel</summary>
<p>{html.escape(CHANGES[-1]["change"])}</p><a href="changes.json">Machine-readable changes</a></details>
<h2 id="s2">2. Introduction</h2>
<p>The module exposes tubular section properties, member stress checks, joint ratios and classification,
and a simplified punching shear check [3, 4, 15]. A workflow adapts these outputs to JSON and CSV [5].
Adjacent dent and damage screens expose separate applicability and escalation contracts [6, 7].
The review is bounded to these module files; it does not qualify a structural system or a standard.</p>
<p>Plan: retain synthetic inputs, derive geometry independently, exercise zero demand through public
functions, inventory unresolved methods, then rebuild the complete delivery and compare its bytes [9, 11, 12].
No source-data or standard-derived coefficient changes are made.</p>
<h2 id="s3">3. Design Data and Basis</h2>
<p>Every input is a declared synthetic assumption, not a measurement, material grade or design criterion.
Positive material strength and modulus avoid zero-capacity degeneracy; effective length is supplied explicitly.
Member loads are stresses in ksi; joint loads are actions in kip and kip-in. Ratios use unit 1.
The brace angle is measured from the chord axis. Side and gap inputs are unused for this single-brace case [3, 4, 9, 10].</p>
<div class="scroll"><table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead>
<tbody>{inputs}</tbody></table></div>
<p>The quarter-inch brace wall is displayed as an exact fraction. These assumptions represent a concentric,
uniform annulus and one perpendicular brace. No unit conversion, physical constant, environmental action,
connection weld detail or load combination is inferred. Pi and square root have their mathematical meanings [1].</p>
<h2 id="s4">4. Analysis Methodology</h2>
<p>The evidence classes below separate independent geometry, zero-demand limits, existing implementation tests
and unqualified standard allowables. Existing tests demonstrate the encoded implementation; reuse of its
coefficients in an expected value does not establish an independent standard basis [8, 13, 14].</p>
<div class="scroll"><table><thead><tr><th>Public method or contract</th><th>Evidence class</th><th>Basis and boundary</th></tr></thead>
<tbody>{methods}</tbody></table></div>
<p>Geometry is checked at full precision against independently integrated expressions [1, 2, 9].
The numerical comparison allowance is 1e-12 relative, with zero absolute allowance for these nonzero values,
following the documented rounding policy [1]. This is not an engineering tolerance.
Zero demand divided by finite positive capacity is exactly zero. Only this arithmetic implication is checked;
capacity positivity is a precondition, not independent qualification [9].</p>
<h2 id="s5">5. Detailed Results</h2>
<h3>Independent annulus calculation</h3>
<p>Let R = 6 in and a = 11/2 in [1]. With dA = rho d(rho) d(theta), integration around a full turn gives
A = integral integral rho d(rho) d(theta) = pi(R²-a²) = 23*pi/4 in².
The centroid lies at the common centre by symmetry. Integrating y² dA gives
I = integral integral rho³ sin²(theta) d(rho) d(theta) = pi(R⁴-a⁴)/4 = 6095*pi/64 in⁴.
Then S = I/R = 6095*pi/384 in³ and r = sqrt(I/A) = sqrt(265)/4 in [1, 2, 9].
These expected expressions are derived independently; displayed actual results below come from public properties [3, 12].</p>
<div class="scroll"><table><thead><tr><th>Property</th><th>Independent exact expression</th><th>Computed value</th><th>Unit</th><th>Sources</th></tr></thead>
<tbody>{annulus}</tbody></table></div>
<h3>Independent joint ratios and classification</h3>
<p>Direct division of the declared dimensions gives gamma = (12/2)/(1/2) = 12,
beta = [6/12] = [1/2] and tau = [(1/4)/(1/2)] = [1/2]. One brace gives the simplified T/Y label;
this checks the supplied geometry classification, not a force-balanced load path [4, 9].</p>
<table><thead><tr><th>Quantity</th><th>Computed value</th><th>Unit</th><th>Sources</th></tr></thead><tbody>{joint}</tbody></table>
<h3>Zero-demand limit</h3>
<p>The public orchestrator exercises compression and tension flags, their individual checks and governing
selection. Joint punching shear uses the nondegenerate geometry above. All recorded denominators are finite
and positive. Numeric allowables and implementation PASS verdicts are withheld because they are unqualified [3, 4, 9].</p>
<div class="scroll"><table><thead><tr><th>Call/result</th><th>Applied demand (ksi)</th><th>Utilisation (1)</th><th>Status</th><th>Sources</th></tr></thead><tbody>{zeros}</tbody></table></div>
<p>Displays round returned numerical results to one decimal place; the embedded calculation payload retains
unrounded values for reproducible comparisons. No nonzero capacity result is presented as independently qualified.</p>
<h2 id="s6">6. Key Findings</h2>
<p>Annulus properties and single-brace ratios have a transparent independent basis. A zero result at zero
demand demonstrates arithmetic only; it cannot distinguish a correct allowable from any other positive allowable [1, 9].</p>
<p>Review found no local correctness defect supported by the present independent cases. Production equations
are preserved. Method limitations are recorded rather than converted into a standards-equation rewrite [3–7].</p>
<h2 id="s7">7. Limitations and Not Covered</h2>
<p><strong>Simplified bending treatment:</strong> the bending helper returns a fixed fraction of supplied yield
strength without applying a diameter-to-wall branch. Its docstring discusses compactness but that discussion
does not establish the implemented range. Nonzero bending capacity and interactions remain NOT_EVALUATED [3].</p>
<p>The combined check calculates a compression-based interaction regardless of the tension flag; the flag
selects only the standalone axial check in the orchestrator. Nonzero tension interaction therefore needs
an independent basis and explicit intended-behaviour review [3].</p>
<p>Joint classification is geometric and simplified. The chord-load factor description and implemented
reduction differ in their stated dependencies. The applied shear routine and geometry-factor comments also
need a reconciled angle convention. The perpendicular zero-demand case cannot resolve these questions [4].
No numeric standard coefficients are reproduced or changed here.</p>
<p>The workflow can report PASS for an empty collection, and its PASS status reports an implementation
threshold rather than engineering qualification [5]. Direct dataclass constructors lack comprehensive checks
for non-finite inputs, invalid dimensions and inconsistent brace-array lengths [3, 4]. Robust input validation,
multi-brace loading, fatigue, pressure, weld design and system reserve strength are outside this calculation.</p>
<p>Screening source citations and existing hand tests are inventoried, but their external study was not reopened
for this report. Continuous applicability, experimental qualification, combined damage and engineering acceptance
remain NOT_EVALUATED [6, 7, 13, 14]. No licensed solver is imported, queried or executed by this builder.</p>
<h2 id="s8">8. Recommendations</h2>
<p>Before a nonzero capacity assessment, obtain an openly accessible authoritative worked example with an
identifiable edition, clause title, complete dimensions, material basis, load conventions and applicability.
If such evidence cannot be obtained, retain NOT_EVALUATED and arrange a qualified standards review.
Reconcile bending compactness, axial interaction, chord-load dependence and angle conventions against that basis.
Add independent nonzero and boundary regressions before changing any equation [3, 4].</p>
<p>Review empty-input workflow semantics and constructor validation in a bounded follow-up. For damage
screening, retrieve and independently check the cited study and experimental evidence, then confirm support,
bow, pressure and combined-damage limits before considering acceptance [5–7]. Keep the broader issue open.</p>
<h2 id="s9">9. References and Appendices</h2>
<p>Source numbers below identify repository evidence and exact SHA-256 digests. A repository implementation
is not independent standards evidence. Standard designation recorded by the implementation: API RP 2A-WSD,
21st Edition, member axial/bending checks and tubular joints. No licensed text or numeric coefficient is copied;
standard-capacity source qualification is unresolved [3, 4].</p>
<ol>{sources}</ol>
<p>Rebuild from the repository root with source imports enabled:</p>
<pre>PYTHONPATH=src python scripts/reports/build_jacket_topside_module_report.py --scratch "$TMPDIR/calculation" --output-dir docs/reports/modules/jacket_topside</pre>
<p>Scratch must be supplied outside the checkout. Only the Python standard library and the module are needed
for the builder. Intermediate calculation.json stays in scratch. The delivery contains index.html, changes.json,
README.md and manifest.json. The manifest binds source bytes and the other artifacts; it excludes its own hash
to avoid circularity [11, 12]. Tests compare complete payloads and independently rebuilt delivery bytes.</p>
<details><summary>Unrounded calculation payload</summary>
<p>Machine-readable values, units, source numbers and qualification status are embedded below.</p></details>
<script id="calculation" type="application/json">{payload}</script>
<h2 id="s10">10. Conclusions</h2>
<p>Geometry and zero-demand arithmetic are reproducible within the stated synthetic case.
Nonzero member and joint capacity, damage acceptance and standards compliance remain NOT_EVALUATED.
The way forward is independent source qualification followed by targeted nonzero regression cases.
No design compliance is certified.</p>
</main></body></html>
'''


def readme_text():
    """Generate the small entry point with summary followed by revision history."""
    history = "\n".join(
        f'| {row["revision"]} | {row["date"]} | {row["change"]} |' for row in CHANGES
    )
    return f'''# Jacket and topside module analysis

## Summary

[Read the report](index.html) for independently derived annulus properties,
single-brace joint ratios and zero-demand arithmetic. Nonzero member and joint
capacities, damage acceptance and standards compliance remain `NOT_EVALUATED`.
No design compliance is certified.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
{history}

## Rebuild

From the repository root, use a writable scratch directory outside the checkout:

```sh
PYTHONPATH=src python scripts/reports/build_jacket_topside_module_report.py --scratch "$TMPDIR/calculation" --output-dir docs/reports/modules/jacket_topside
```

The builder uses the Python standard library and public module functions. All
synthetic inputs are committed in the builder and reproduced with units and
source numbers in the report. Temporary calculation JSON remains in scratch.
The [manifest](manifest.json) records source and artifact SHA-256 digests, excluding
its own hash. [Change records](changes.json) use the report revision history.

## Evidence and limits

Independent expectations are the radial integrals in the existing
[worked geometry example](../../../../src/digitalmodel/structural/jacket_topside/docs/worked-tubular-section-geometry.md)
and direct divisions of synthetic joint dimensions. Zero-demand results check
arithmetic at finite positive returned capacity; numeric standard allowables
are withheld. Existing implementation tests are distinguished from independent
source qualification in the method inventory. Simplified bending, interaction
and joint assumptions require independently sourced nonzero cases before use.

Rebuild tests compare every deliverable byte and source/artifact digest across
independent directories. No external solver or standard-capacity qualification
is part of the rebuild. The broader issue remains open for unresolved methods.
'''


def main():
    """Write deterministic report deliverables and their digest manifest."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    args = parser.parse_args()
    scratch, output = args.scratch.resolve(), args.output_dir.resolve()
    if scratch == output or scratch in output.parents or output in scratch.parents:
        parser.error("scratch and output must be separate directories")
    artifacts = {
        "index.html": render(scratch),
        "changes.json": json_text(CHANGES),
        "README.md": readme_text(),
    }
    manifest = {
        "algorithm": "sha256",
        "sources": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
                    for path in SOURCES},
        "artifacts": {name: hashlib.sha256(text.encode("utf-8")).hexdigest()
                      for name, text in artifacts.items()},
        "manifest_policy": "Artifact map excludes itself to avoid a circular digest; "
                           "source paths are repository-relative; no headers were replaced.",
    }
    artifacts["manifest.json"] = json_text(manifest)
    output.mkdir(parents=True, exist_ok=True)
    for name, text in artifacts.items():
        (output / name).write_text(text, encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
