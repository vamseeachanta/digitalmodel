# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Prepare and reduce fixed-bottom wind cases without importing a solver.

Run this file directly; importing its parent package is outside this interface.
Only the standard library is needed. Inputs are text metadata and stored JSON
exports; this module never opens a model through a solver or loads a DLL.
See docs/domains/orcaflex/wind/fixed-bottom-worked-example/workflow.html.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import math
import re
from pathlib import Path
from typing import Any

DOMAIN = Path("docs/domains/orcaflex")
RAW = DOMAIN / "examples/raw/K02"
MODEL = RAW / "K02 10MW fixed-bottom OWT.yml"
TEMPLATE = DOMAIN / "library/templates/wind_turbine_fixed/spec.yml"
LIBRARY = DOMAIN / "library/model_library/k02_10mw_fixed_bottom_owt/spec.yml"
CHANNELS = ("fx", "fy", "fz", "mx", "my", "mz")
NOT_EVALUATED = "NOT_EVALUATED"
NIST_MEAN = "https://www.itl.nist.gov/div898/handbook/eda/section3/eda351.htm"
TURBINE_REFERENCE = "https://www.nlr.gov/docs/fy19osti/73492.pdf"


def number(value: Any, name: str, *, positive: bool = False) -> float:
    """Require a finite real number, excluding booleans and coerced strings."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name}: finite number required")
    if not math.isfinite(value) or (positive and value <= 0):
        raise ValueError(f"{name}: invalid numeric value")
    return value


def text(value: Any, name: str) -> str:
    """Require a nonempty text provenance field."""
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name}: nonempty text required")
    return value


def inventory(root: Path) -> list[dict[str, Any]]:
    """Record exact bytes and presence without importing any external file."""
    paths = [MODEL, LIBRARY, TEMPLATE] + [
        RAW / name
        for name in (
            "BladedControllerWrapper.py",
            "libdiscon.dll",
            "IEA-10.0-198-RWT_DISCON.IN",
            "IEA-10.0-198-RWT_Cp_Ct_Cq.txt",
            "90m_12mps_twr.inp",
            "wind.bts",
        )
    ]
    rows = []
    for path in paths:
        full = Path(root) / path
        held = full.is_file()
        rows.append(
            {
                "path": path.as_posix(),
                "held": held,
                "sha256": hashlib.sha256(full.read_bytes()).hexdigest()
                if held
                else None,
            }
        )
    return rows


def read_basis(root: Path) -> dict[str, dict[str, Any]]:
    """Read narrow scalar locators from held text, preserving line citations.

    This is not a YAML loader. Each selector must occur exactly once, so a
    changed source fails closed rather than silently selecting another case.
    No source file or external-function script is executed.
    """
    selectors = {
        "depth_m": (MODEL, r"^\s*SeabedOriginDepth:\s*(\S+)\s*$"),
        "hs_m": (MODEL, r"^\s*WaveHs:\s*(\S+)\s*$"),
        "tz_s": (MODEL, r"^\s*WaveTz:\s*(\S+)\s*$"),
        "steady_speed_m_s": (TEMPLATE, r"^\s*speed:\s*(\S+)\s*# Rated wind speed.*$"),
        "turbulent_speed_m_s": (RAW / "90m_12mps_twr.inp", r"^\s*(\S+)\s+URef\s+.*$"),
    }
    basis = {}
    for key, (path, pattern) in selectors.items():
        matches = [
            (i, re.fullmatch(pattern, line))
            for i, line in enumerate(
                (Path(root) / path).read_text(encoding="utf-8").splitlines(), start=1
            )
        ]
        matches = [(i, m) for i, m in matches if m]
        if len(matches) != 1:
            raise ValueError(f"{key}: unique source locator required")
        line, match = matches[0]
        if match is None:
            raise ValueError(f"{key}: source locator did not match")
        value = number(float(match[1]), key, positive=True)
        basis[key] = {"value": value, "source": f"{path.as_posix()}:L{line}"}
    return basis


def prepare_cases(
    root: Path, soft_factor: float | None = None, stiff_factor: float | None = None
) -> list[dict[str, Any]]:
    """Build the soil/wind matrix; sensitivity factors have no design defaults."""
    if soft_factor is not None:
        if number(soft_factor, "soft factor", positive=True) >= 1:
            raise ValueError("soft factor must be below the unchanged baseline")
    if stiff_factor is not None:
        if number(stiff_factor, "stiff factor", positive=True) <= 1:
            raise ValueError("stiff factor must exceed the unchanged baseline")
    # One is the exact identity multiplier: K_case = factor * K_held.
    # Sensitivity levels are caller inputs, never coefficients from memory.
    factors = {"softened": soft_factor, "baseline": 1, "stiffened": stiff_factor}
    basis = read_basis(root)
    held = {row["path"]: row["held"] for row in inventory(root)}
    cases = []
    for wind in ("steady", "turbulent"):
        for soil, factor in factors.items():
            blockers = [
                "native statics, modes and dynamics not supplied",
                "design criteria and equivalence to the public turbine definition not established",
            ]
            if factor is None:
                blockers.append("soil multiplier requires a cited engineering input")
            if wind == "turbulent":
                if not held[(RAW / "wind.bts").as_posix()]:
                    blockers.append("wind.bts is not held")
                blockers.append(
                    "reconcile wind grid, height, duration and controller before use"
                )
            cases.append(
                {
                    "case_id": f"{wind}_{soil}",
                    "wind": wind,
                    "soil": soil,
                    "soil_factor": factor,
                    "factor_source": "identity multiplier"
                    if soil == "baseline"
                    else "required caller input",
                    "representation": "template" if wind == "steady" else "raw",
                    "wind_speed_m_s": basis[f"{wind}_speed_m_s"],
                    "hs_m": basis["hs_m"],
                    "tz_s": basis["tz_s"],
                    "status": NOT_EVALUATED,
                    "blockers": blockers,
                }
            )
    return cases


def validate_export(case: dict[str, Any], result: dict[str, Any]) -> dict[str, Any]:
    """Validate a stored export and reduce only the explicitly selected window."""
    if type(result.get("synthetic")) is not bool:
        raise ValueError("synthetic must be a boolean")
    for field in ("solver_version", "load_convention", "soil_factor_source"):
        text(result.get(field), field)
    for field in ("input_sha256", "comparison_basis_sha256"):
        if not re.fullmatch(r"[0-9a-f]{64}", str(result.get(field, ""))):
            raise ValueError(f"{field}: SHA-256 required")
    for field, expected in (
        ("representation", case["representation"]),
        ("frame", "global"),
        ("origin", "mudline"),
        ("force_unit", "kN"),
        ("moment_unit", "kN.m"),
    ):
        if result.get(field) != expected:
            raise ValueError(f"{field}: incompatible export")
    factor = number(result.get("soil_factor"), "export soil factor", positive=True)
    if case["soil_factor"] is None or factor != case["soil_factor"]:
        raise ValueError("export soil factor does not match the case input")
    window = result.get("window_s")
    if not isinstance(window, list) or len(window) != 2:
        raise ValueError("window_s requires inclusive start and end")
    start, end = [number(v, "window") for v in window]
    if start >= end:
        raise ValueError("window must have positive duration")
    samples = result.get("samples")
    if not isinstance(samples, list) or not samples:
        raise ValueError("mudline samples required")
    previous = None
    selected = []
    for sample in samples:
        time = number(sample.get("time_s"), "time_s")
        if previous is not None and time <= previous:
            raise ValueError("sample times must increase strictly")
        previous = time
        for channel in CHANNELS:
            number(sample.get(channel), channel)
        if start <= time <= end:
            selected.append(sample)
    if samples[0]["time_s"] > start or samples[-1]["time_s"] < end or len(selected) < 2:
        raise ValueError(
            "samples must cover the window with at least two selected points"
        )
    modes = result.get("modes")
    if not isinstance(modes, list) or not modes:
        raise ValueError("shape-labelled modes required")
    frequencies = {}
    shapes = {}
    for mode in modes:
        key = text(mode.get("mode_key"), "mode_key")
        text(mode.get("shape_evidence"), "shape_evidence")
        if key in frequencies:
            raise ValueError("duplicate modal label")
        frequencies[key] = number(
            mode.get("frequency_hz"), "frequency_hz", positive=True
        )
        shapes[key] = mode["shape_evidence"]
    loads = {}
    for channel in CHANNELS:
        values = [row[channel] for row in selected]
        # NIST/SEMATECH e-Handbook, online edition read 2026-10-20,
        # §1.3.5.1 (NIST_MEAN): sum(Y_i)/N.
        # This is a sample mean, not a time integral or an extreme estimate.
        loads[channel] = {
            "sample_mean": math.fsum(v / len(values) for v in values),
            "minimum": min(values),
            "maximum": max(values),
            "absolute_peak": max(abs(v) for v in values),
        }
    return {
        "status": "SYNTHETIC" if result["synthetic"] else "EXPORTED",
        "synthetic": result["synthetic"],
        "solver_version": result["solver_version"],
        "input_sha256": result["input_sha256"],
        "comparison_basis_sha256": result["comparison_basis_sha256"],
        "soil_factor_source": result["soil_factor_source"],
        "shape_evidence": shapes,
        "window_s": window,
        "sample_count": len(selected),
        "load_convention": result["load_convention"],
        "frequencies_hz": frequencies,
        "loads": loads,
    }


def analyse(
    cases: list[dict[str, Any]], exports: list[dict[str, Any]]
) -> dict[str, Any]:
    """Reduce exports and compare labels within each wind/control arrangement.

    Relative change is defined as (variant - baseline) / baseline. It is a
    dimensionless comparison, not a code acceptance criterion. Shape labels
    must already have been reviewed by the exporter; no frequency-rank pairing
    or tower/monopile classification is inferred here.
    """
    case_map = {c["case_id"]: c for c in cases}
    results = {key: {"status": NOT_EVALUATED} for key in case_map}
    seen = set()
    synthetic_flags = set()
    versions = set()
    for result in exports:
        key = result.get("case_id")
        if key not in case_map or key in seen:
            raise ValueError("unknown or duplicate export case")
        seen.add(key)
        reduced = validate_export(case_map[key], result)
        synthetic_flags.add(reduced["synthetic"])
        versions.add(reduced["solver_version"])
        results[key] = reduced
    if len(synthetic_flags) > 1 or len(versions) > 1:
        raise ValueError(
            "one synthetic status and one solver version required per report"
        )
    sensitivity = {}
    for case in cases:
        if case["soil"] == "baseline":
            continue
        key = case["case_id"]
        variant = results[key]
        baseline = results[f"{case['wind']}_baseline"]
        row: dict[str, Any] = {
            "status": NOT_EVALUATED,
            "reason": "Baseline or variant export missing",
        }
        if "loads" in variant and "loads" in baseline:
            compatible = all(
                variant[k] == baseline[k]
                for k in ("window_s", "load_convention", "comparison_basis_sha256")
            )
            compatible &= (
                variant["frequencies_hz"].keys() == baseline["frequencies_hz"].keys()
            )
            row["reason"] = (
                "Non-soil basis, window, convention or mode-label set differs"
            )
            if compatible:
                row = {
                    "status": variant["status"],
                    "frequency_relative_change": {
                        k: (v - baseline["frequencies_hz"][k])
                        / baseline["frequencies_hz"][k]
                        for k, v in variant["frequencies_hz"].items()
                    },
                    "load_peak_relative_change": {},
                }
                for channel in CHANNELS:
                    base = baseline["loads"][channel]["absolute_peak"]
                    # Zero baseline: a relative change is undefined, never zero-filled.
                    row["load_peak_relative_change"][channel] = (
                        (variant["loads"][channel]["absolute_peak"] - base) / base
                        if base
                        else None
                    )
        sensitivity[key] = row
    return {
        "engineering_acceptance": NOT_EVALUATED,
        "results": results,
        "sensitivity": sensitivity,
        "note": "Stored exports only; native convergence and shape evidence require independent review.",
    }


def display(value: Any) -> str:
    """Format report numbers to one decimal place; retain machine inputs separately."""
    if value is None:
        return NOT_EVALUATED
    if isinstance(value, bool):
        return "held" if value else "not held"
    if isinstance(value, (int, float)):
        return f"{value:.1f}"
    return html.escape(str(value))


def table(headers: list[str], rows: list[list[str]]) -> str:
    """Render an escaped accessible HTML table from already formatted cells."""
    return (
        "<table><thead><tr>"
        + "".join(f"<th scope='col'>{h}</th>" for h in headers)
        + "</tr></thead><tbody>"
        + "".join(
            "<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>"
            for row in rows
        )
        + "</tbody></table>"
    )


def render_report(
    files: list[dict[str, Any]],
    basis: dict[str, dict[str, Any]],
    cases: list[dict[str, Any]],
    analysis: dict[str, Any],
) -> str:
    """Build a static analysis report using the repository report-pack structure."""
    synthetic = any(r.get("synthetic") for r in analysis["results"].values())
    state = (
        "SYNTHETIC ARITHMETIC EXAMPLE"
        if synthetic
        else "UNISSUED — NATIVE RESULTS NOT ESTABLISHED"
    )
    parts = [
        "<!doctype html><html lang='en'><meta charset='utf-8'>",
        "<meta name='viewport' content='width=device-width, initial-scale=1'>",
        "<title>Fixed-bottom wind turbine analysis</title><style>",
        "body{font:1rem system-ui;max-width:78rem;margin:auto;padding:2rem;color:#183347}",
        "table{border-collapse:collapse;width:100%;margin:1rem 0;display:block;overflow:auto}",
        "th,td{border:1px solid #bdcbd4;padding:.5rem;text-align:left;overflow-wrap:anywhere}",
        "th{background:#eaf1f5}p{line-height:1.6}.status{background:#fff0d5;padding:1rem}",
        "code{overflow-wrap:anywhere}@media print{body{padding:0}tr{break-inside:avoid}}",
        "</style><header><h1>Fixed-bottom wind turbine analysis</h1>",
        f"<p class='status'>{state}</p></header><main><h2>Summary</h2>",
        "<p>The workflow inventories held inputs, prepares a soil/wind case matrix and reduces ",
        "stored mudline loads and shape-labelled natural frequencies. Engineering acceptance: ",
        "NOT_EVALUATED. No solver execution or turbine design qualification is established.</p>",
        "<h2>Revision history</h2>",
        table(
            ["Revision", "Date", "Change and reason"],
            [
                [
                    "Initial",
                    "2026-10-20",
                    "Offline method and input gaps recorded to prepare reproducible analysis.",
                ]
            ],
        ),
        "<h2>Document control</h2><p>Unissued analysis. Prepared, checked and approved: pending. ",
        "Document number and governing standard editions: not assigned.</p>",
        "<h2>Introduction and scope</h2><p>Steady and turbulent wind with irregular waves; ",
        "tower/monopile mode identification and distributed soil-spring sensitivity. ",
        "Only results and method may be shared; vendor model redistribution terms remain unchecked.</p>",
        "<h2>References and traceability</h2><p>Layout follows ",
        "docs/domains/reporting/standard-report-pack.md; this standalone static report does not ",
        "claim engine-generated provenance validation. The defining public reference is ",
        f"<a href='{TURBINE_REFERENCE}'>IEA Wind Task 37, Systems Engineering in Wind Energy — ",
        "WP2.1 Reference Wind Turbines (May 2019), section 5, Offshore Wind Turbine</a>. ",
        "Equivalence of the held model to that definition and design acceptance limits remain ",
        "unestablished. Input values below are as-held model settings, ",
        "not standards-derived design coefficients.</p>",
        "<h2>Design basis and input echo</h2>",
    ]
    parts.append(
        table(
            ["Quantity", "Value (source units)", "Source locator"],
            [
                [display(k), display(v["value"]), display(v["source"])]
                for k, v in basis.items()
            ],
        )
    )
    parts += [
        "<p>Display rounding is one decimal place; the source inputs retain full precision. ",
        "Raw model: JONSWAP (L133–140); soil connections (L13052–13071); constraint coefficients ",
        "(L13097–14147). The template and library copy are distinct representations. ",
        "The template substitutes steady wind and specified torque for the external controller. ",
        "The held turbulence input describes a smaller turbine and is not accepted as a ",
        "regeneration recipe: reconcile rotor/grid coverage, reference height, duration and speed.</p>",
        "<h2>External-file inventory and data sources</h2>",
    ]
    parts.append(
        table(
            ["Repository-relative input", "Availability", "SHA-256 (exact bytes)"],
            [
                [display(f["path"]), display(f["held"]), display(f["sha256"])]
                for f in files
            ],
        )
    )
    parts += [
        "<p>The wrapper, binary, controller input and rotor-performance table are held. ",
        "The binary was hashed only. No wrapper, controller or solver was imported or loaded. ",
        "The initial route is the steady template; wind.bts regeneration remains unevaluated.</p>",
        "<h2>Methodology and assumptions</h2><p>For each approved soil factor a, ",
        "K_case = a K_held for all selected translational and rotational soil coefficients; ",
        "retain zero coefficients, damping and DOF definitions. The unchanged baseline has ",
        "identity factor one. Soft/stiff levels require a cited input; none is supplied by default. ",
        "This workflow writes no model and makes no stiffness-to-frequency approximation.</p>",
        f"<p>Sample mean = sum(x)/N: <a href='{NIST_MEAN}'>NIST/SEMATECH e-Handbook ",
        "(online edition read 2026-10-20), section 1.3.5.1, Measures of Location</a>. ",
        "Minimum, maximum and absolute peak ",
        "are selected from samples in the inclusive window. Means are sample means, not time-weighted. ",
        "Relative change is defined as (variant − baseline)/baseline; zero load baselines give ",
        "NOT_EVALUATED. Compare within a wind/control arrangement using reviewed shape labels, ",
        "matching windows, force conventions and non-soil comparison-basis digests. ",
        "No rank pairing or acceptance tolerance is used.</p>",
        "<h2>Case table</h2>",
    ]
    parts.append(
        table(
            ["Case", "Representation", "Soil factor", "Speed (m/s)", "Disposition"],
            [
                [
                    display(c["case_id"]),
                    display(c["representation"]),
                    display(c["soil_factor"]),
                    display(c["wind_speed_m_s"]["value"]),
                    display("; ".join(c["blockers"])),
                ]
                for c in cases
            ],
        )
    )
    parts += [
        "<h2>Results</h2><p>Missing exports remain NOT_EVALUATED; no zeros stand in for missing ",
        "results. Exports record one solver version, actual solved-input digest, global axes, ",
        "mudline origin, sign convention and selected time window. Export metadata are declarations, ",
        "not independent attestation of a completed native analysis.</p>",
    ]
    for key, result in analysis["results"].items():
        parts.append(f"<h3>{display(key)} — {display(result['status'])}</h3>")
        if "loads" not in result:
            continue
        parts.append(
            f"<p>Solver version: {display(result['solver_version'])}; input SHA-256: "
        )
        parts.append(f"<code>{display(result['input_sha256'])}</code>; window (s): ")
        parts.append(
            f"{display(result['window_s'][0])} to {display(result['window_s'][1])}; "
        )
        parts.append(f"load convention: {display(result['load_convention'])}.</p>")
        parts.append(f"<p>Soil factor basis: {display(result['soil_factor_source'])}; ")
        parts.append(
            f"comparison basis SHA-256: <code>{display(result['comparison_basis_sha256'])}</code>.</p>"
        )
        parts.append(
            table(
                ["Shape-reviewed label", "Frequency (Hz)", "Shape evidence"],
                [
                    [display(k), display(v), display(result["shape_evidence"][k])]
                    for k, v in result["frequencies_hz"].items()
                ],
            )
        )
        parts.append(
            table(
                ["Channel", "Unit", "Sample mean", "Min", "Max", "Absolute peak"],
                [
                    [channel, "kN" if channel.startswith("f") else "kN.m"]
                    + [
                        display(stats[k])
                        for k in ("sample_mean", "minimum", "maximum", "absolute_peak")
                    ]
                    for channel, stats in result["loads"].items()
                ],
            )
        )
    parts.append("<h2>Soil sensitivity</h2>")
    for key, row in analysis["sensitivity"].items():
        parts.append(f"<h3>{display(key)} — {display(row['status'])}</h3>")
        if "reason" in row:
            parts.append(f"<p>{display(row['reason'])}</p>")
        for metric in ("frequency_relative_change", "load_peak_relative_change"):
            if metric in row:
                parts.append(
                    table(
                        [display(metric), "Change (%)"],
                        [
                            [display(k), NOT_EVALUATED if v is None else f"{v:.1%}"]
                            for k, v in row[metric].items()
                        ],
                    )
                )
    parts += [
        "<h2>Worked sample calculation</h2><p>Explicitly synthetic arithmetic, not a turbine ",
        "prediction: Fx samples −3.0, 0.0, 6.0 kN give (−3.0 + 0.0 + 6.0)/3.0 = 1.0 kN ",
        "and absolute peak 6.0 kN. Tower frequencies 1.0, 2.0, 3.0 Hz for softened, baseline, ",
        "stiffened soil give relative changes −50.0% and +50.0%. These arbitrary test inputs ",
        "and their independent hand calculation are in test_fixed_bottom_workflow.py.</p>",
        "<h2>Conclusions and limitations</h2><p>Confirm equivalence to the defining reference, approve soil ",
        "factors, wind/control reconciliation, seeds and sampling/convergence plan before native ",
        "execution. Retain native static convergence, mode shapes and full time histories. ",
        "Export loads at the mudline section; do not substitute tower-base loads or sum ",
        "independent component peaks into a resultant. Confirm paired mode shapes at every soil ",
        "level. No structural capacity, fatigue, frequency-separation or soil adequacy criterion ",
        "has been evaluated. Steady versus turbulent differences are confounded by speed and ",
        "control changes until reconciled.</p>",
        f"<p>{display(analysis['note'])}</p>",
        "<h2>Appendix A — Reproduction and qualifications</h2><p>Run the standalone script ",
        "with --output pointing to a new report file. Add --exports only for validated JSON ",
        "and --soft-factor/--stiff-factor only with a recorded input basis. Output files are ",
        "created exclusively. Missing solver results prevent engineering qualification.</p>",
        "</main></html>\n",
    ]
    return "\n".join(part.rstrip() for part in parts) + "\n"


def main() -> None:
    """Create a new report; never overwrite an existing report or solver input."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--exports", type=Path)
    parser.add_argument("--soft-factor", type=float)
    parser.add_argument("--stiff-factor", type=float)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        exports = (
            json.loads(args.exports.read_text(encoding="utf-8")) if args.exports else []
        )
        if not isinstance(exports, list):
            raise ValueError("exports must be a list of case records")
        cases = prepare_cases(args.root, args.soft_factor, args.stiff_factor)
        report = render_report(
            inventory(args.root), read_basis(args.root), cases, analyse(cases, exports)
        )
        with args.output.open("x", encoding="utf-8") as stream:
            stream.write(report)
    except (OSError, ValueError, KeyError, TypeError) as exc:
        parser.exit(2, f"Cannot build report: {exc}\n")


if __name__ == "__main__":
    main()
