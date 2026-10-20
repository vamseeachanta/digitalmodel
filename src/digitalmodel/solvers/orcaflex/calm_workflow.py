# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Prepare generic CALM/SPM cases and report exported extrema without a solver.

No binding is imported or detected. Native loadability and engineering qualification
remain unevaluated: generic template coefficients and transfer-hose properties
require review before any solver run. Criteria are required inputs, never defaults.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
from pathlib import Path
from typing import TYPE_CHECKING, Any

import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexDumper
from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

if TYPE_CHECKING:
    from digitalmodel.reporting import ReportSpec, Section

DOMAIN = Path("docs/domains/orcaflex")
TEMPLATES = DOMAIN / "templates/mooring_systems"
BASE = TEMPLATES / "calm_buoy_hybrid/base/calm_buoy_base.yml"
GENERIC = TEMPLATES / "calm_buoy/includes"
FORCES = "https://www.orcina.com/webhelp/OrcaFlex/Content/html/Lineresults,Forces.htm"
MOMENTS = "https://www.orcina.com/webhelp/OrcaFlex/Content/html/Lineresults,Moments.htm"
SOURCE_EDITION = "public web help, accessed 2026-10-20"
LIMITS = (
    "Native solver loadability, coupled equilibrium and dynamic response are NOT_EVALUATED. "
    "Generic tanker RAOs lack qualified wind/current/drift coefficients. "
    "The inherited tanker is excluded from statics and requires equilibrium configuration. "
    "Transfer-hose properties are illustrative, with no manufacturer qualification. "
    "Whole-line extrema must cover the analysis stage; time and mesh sensitivity, "
    "pretension, offsets, anchor capacity, fatigue and tension-dependent hose limits "
    "require separate assessment. No environmental force or hawser load is inferred."
)


def _text(value: Any) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("a nonempty source or identifier is required")
    return value


def _number(value: Any, *, positive: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError("a finite numeric value is required")
    if not math.isfinite(value) or value < 0 or (positive and value == 0):
        raise ValueError(
            "value must be finite and nonnegative (positive for limits/periods)"
        )
    return float(value)


def _angle(value: Any) -> float:
    if (
        isinstance(value, bool)
        or not isinstance(value, (int, float))
        or not math.isfinite(value)
    ):
        raise ValueError("direction must be finite")
    # Full-turn identity, not an empirical coefficient: degrees modulo one revolution.
    return float(value) % 360


def _digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _canonical(data: Any) -> bytes:
    return json.dumps(
        data, sort_keys=True, allow_nan=False, separators=(",", ":")
    ).encode()


def _load_generic(
    path: Path, root: Path, sources: dict[str, str], stack: tuple[Path, ...] = ()
) -> Any:
    """Inline object includes only from generic templates and component libraries."""
    path = path.resolve()
    relative = path.relative_to(root.resolve())
    if not any(
        relative.is_relative_to(prefix)
        for prefix in (
            TEMPLATES,
            DOMAIN / "library/line_types",
            DOMAIN / "library/buoy_types",
        )
    ):
        raise ValueError("only generic template and component sources are allowed")
    if path in stack:
        raise ValueError("cyclic generic include")
    stack = (*stack, path)
    key = relative.as_posix()
    raw = path.read_bytes()
    sources[key] = _digest(raw)

    def expand(node: Any) -> Any:
        if isinstance(node, list):
            return [expand(item) for item in node]
        if not isinstance(node, dict):
            return node
        if "IncludeFile" in node:
            included = _load_generic(
                path.parent / node["IncludeFile"], root, sources, stack
            )
            node = {**included, **{k: v for k, v in node.items() if k != "IncludeFile"}}
        return {key: expand(value) for key, value in node.items()}

    return expand(yaml.safe_load(raw))


def _topology(model: dict, coupling: dict) -> dict[str, str]:
    lines = model["Lines"]
    names = [line["Name"] for line in lines]
    if len(names) != len(set(names)):
        raise ValueError("duplicate line name")
    buoy = model["6DBuoys"][0]["Name"]
    vessel = model["Vessels"][0]["Name"]
    type_names = [item["Name"] for item in model["LineTypes"]]
    types = set(type_names)
    if len(type_names) != len(types):
        raise ValueError("duplicate line type name")
    expected = {}
    for line in lines:
        sections = line["LineType, Length, TargetSegmentLength"]
        if not sections:
            raise ValueError("each line requires at least one section")
        for end in ("A", "B"):
            for axis in "XYZ":
                coordinate = line[f"End{end}{axis}"]
                if (
                    isinstance(coordinate, bool)
                    or not isinstance(coordinate, (int, float))
                    or not math.isfinite(coordinate)
                ):
                    raise ValueError("line coordinates must be finite numbers")
        for line_type, length, segment in sections:
            if line_type not in types:
                raise ValueError("unknown line type")
            _number(length, positive=True)
            _number(segment, positive=True)
        if line["EndAConnection"] != buoy:
            raise ValueError("all lines must begin at the CALM buoy")
        is_coupling = line["Name"] in (
            coupling["hawser"]["Name"],
            coupling["hose"]["Name"],
        )
        if line["EndBConnection"] != (vessel if is_coupling else "Anchored"):
            raise ValueError("line end does not match the buoy/tanker/anchor topology")
        expected[line["Name"]] = (
            "curvature"
            if line["Name"] == coupling["hose"]["Name"]
            else "effective_tension"
        )
    return expected


def prepare_campaign(config: dict, root: Path, output: Path) -> dict:
    """Build flattened native YAML, validating every case and recording digests.

    Output contains no absolute source or destination paths. All empirical values
    come from recorded generic sources or explicit, attributed coupling inputs.
    """
    config = copy.deepcopy(config)
    _text(config["headings_source"])
    headings = [_angle(h) for h in config["headings_deg"]]
    if not headings or len(headings) != len(set(headings)):
        raise ValueError("headings must be nonempty and unique modulo a full turn")
    conditions = config["conditions"]
    if not conditions or len({c["name"] for c in conditions}) != len(conditions):
        raise ValueError("conditions must be nonempty and uniquely named")
    for condition in conditions:
        _text(condition["name"])
        _text(condition["source"])
        for key in ("wave_height_m", "current_speed_m_s", "wind_speed_m_s"):
            _number(condition[key])
        _number(condition["wave_period_s"], positive=True)
        for key in ("current_offset_deg", "wind_offset_deg"):
            _angle(condition[key])
    coupling = config["coupling"]
    _text(coupling["source"])
    sources: dict[str, str] = {}
    base = _load_generic(root / BASE, root, sources)
    base["VesselTypes"] = _load_generic(
        root / GENERIC / "04_vessel_types.yml", root, sources
    )["VesselTypes"]
    base["Vessels"] = _load_generic(root / GENERIC / "06_vessels.yml", root, sources)[
        "Vessels"
    ]
    vessel = base["Vessels"][0]
    for key in ("InitialPosition", "Orientation"):
        vector = coupling[key]
        if len(vector) != 3 or any(
            isinstance(v, bool)
            or not isinstance(v, (int, float))
            or not math.isfinite(v)
            for v in vector
        ):
            raise ValueError(
                "position and orientation require three finite coordinates"
            )
        vessel[key] = vector
    base["LineTypes"].append(coupling["hose_line_type"])
    base["Lines"].extend([coupling["hawser"], coupling["hose"]])
    required = _topology(base, coupling)
    # StageDuration follows the generic base: build-up first, analysis afterwards.
    period = [0, sum(base["General"]["StageDuration"][1:])]
    output.mkdir(parents=True, exist_ok=True)
    cases = []
    for condition in conditions:
        for heading in headings:
            model = copy.deepcopy(base)
            env = model["Environment"]
            env["WindSpeed"] = condition["wind_speed_m_s"]
            env["WindDirection"] = _angle(heading + condition["wind_offset_deg"])
            env["RefCurrentSpeed"] = condition["current_speed_m_s"]
            env["RefCurrentDirection"] = _angle(
                heading + condition["current_offset_deg"]
            )
            wave = env["WaveTrains"][0]
            wave.update(
                WaveDirection=heading,
                WaveHeight=condition["wave_height_m"],
                WavePeriod=condition["wave_period_s"],
            )
            # Dean stream WaveHeight is a regular-wave height, never significant Hs.
            case_id = f"case-{len(cases) + 1:03d}"
            raw = yaml.dump(model, Dumper=OrcaFlexDumper, sort_keys=False).encode()
            path = output / f"{case_id}.yml"
            if path.exists() and path.read_bytes() != raw:
                raise ValueError(
                    "existing model differs; choose a new output directory"
                )
            if not path.exists():
                path.write_bytes(raw)
            validation = OrcaFlexYAMLValidator().validate_file(path)
            if not validation.valid:
                raise ValueError(
                    "generated YAML validation failed: "
                    + "; ".join(i.message for i in validation.errors)
                )
            cases.append(
                {
                    "case_id": case_id,
                    "condition": condition["name"],
                    "wave_direction_deg": heading,
                    "wind_direction_deg": env["WindDirection"],
                    "current_direction_deg": env["RefCurrentDirection"],
                    "wave_height_m": condition["wave_height_m"],
                    "wave_period_s": condition["wave_period_s"],
                    "wind_speed_m_s": env["WindSpeed"],
                    "current_speed_m_s": env["RefCurrentSpeed"],
                    "source": condition["source"],
                    "model_sha256": _digest(raw),
                    "yaml_valid": True,
                    "yaml_warnings": [i.message for i in validation.warnings],
                }
            )
    return {
        "input_sha256": _digest(_canonical(config)),
        "sources": sources,
        "cases": cases,
        "required_outputs": required,
        "period_s": period,
        "input": config,
        "known_limits": LIMITS,
        "native_solver_status": "NOT_EVALUATED",
    }


def assess_results(prepared: dict, responses: list[dict], criteria: dict) -> dict:
    """Validate stored whole-line extrema and compare with explicit allowables.

    PASS/FAIL applies only to each imported scalar and its supplied criterion.
    Hand calculations remain ILLUSTRATIVE; no model qualification is implied.
    """
    cases = {c["case_id"]: c for c in prepared["cases"]}
    required = prepared["required_outputs"]
    if set(criteria) - set(required):
        raise ValueError("unknown criterion object")
    for name, criterion in criteria.items():
        _number(criterion["value"], positive=True)
        _text(criterion["source"])
        if criterion["unit"] != ("m" if required[name] == "curvature" else "kN"):
            raise ValueError("criterion unit mismatch")
    indexed = {}
    versions = set()
    for row in responses:
        case_id = row["case_id"]
        if case_id in indexed:
            raise ValueError("duplicate case result")
        if (
            case_id not in cases
            or row["model_sha256"] != cases[case_id]["model_sha256"]
        ):
            raise ValueError("case or model digest mismatch")
        _text(row["source"])
        if row["period_s"] != prepared["period_s"]:
            raise ValueError("result period must cover the complete analysis stage")
        if row["evidence_kind"] == "solver_export":
            versions.add(_text(row["solver_version"]))
        elif (
            row["evidence_kind"] != "hand_calculation"
            or row["solver_version"] is not None
        ):
            raise ValueError("invalid evidence kind or solver version")
        if set(row["extrema"]) - set(required):
            raise ValueError("unknown result object")
        for name, extrema in row["extrema"].items():
            _number(extrema["max"])
            if extrema["quantity"] != required[name] or extrema["unit"] != (
                "1/m" if required[name] == "curvature" else "kN"
            ):
                raise ValueError("result quantity or unit mismatch")
            if extrema["coverage"] != "whole_line":
                raise ValueError("whole-line extrema are required")
        indexed[case_id] = row
    if len(versions) > 1:
        raise ValueError("one recorded solver version is required")
    checks = []
    for case_id in cases:
        row = indexed.get(case_id)
        for name, quantity in required.items():
            extrema = row["extrema"].get(name) if row else None
            criterion = criteria.get(name)
            check = {
                "case_id": case_id,
                "object": name,
                "quantity": quantity,
                "maximum": extrema["max"] if extrema else None,
                "criterion": criterion,
                "utilisation": None,
                "bend_radius_m": None,
                "status": "NOT_EVALUATED",
                "reason": "missing result or sourced criterion",
                "evidence_kind": row["evidence_kind"] if row else None,
                "result_source": row["source"] if row else None,
            }
            if extrema and quantity == "curvature" and extrema["max"] > 0:
                # OrcaFlex help, Moments: Bend radius, public edition accessed 2026-10-20.
                radius = 1 / extrema["max"]
                check["bend_radius_m"] = radius if math.isfinite(radius) else None
            if extrema and criterion:
                # Forces: Normalised tension = Te / allowable; Moments: normalised
                # curvature = kappa / (1/MBR). Both sources are public, accessed 2026-10-20.
                utilisation = (
                    extrema["max"] * criterion["value"]
                    if quantity == "curvature"
                    else extrema["max"] / criterion["value"]
                )
                if not math.isfinite(utilisation):
                    raise ValueError("nonfinite normalised result")
                check.update(
                    utilisation=utilisation, reason="comparison with supplied criterion"
                )
                # Unity is the mathematical boundary of the cited normalised ratio.
                check["status"] = "PASS" if utilisation <= 1 else "FAIL"
                if row["evidence_kind"] == "hand_calculation":
                    check.update(
                        status="ILLUSTRATIVE",
                        reason="hand calculation; no solver evidence",
                    )
            checks.append(check)
    status = "FAIL" if any(c["status"] == "FAIL" for c in checks) else "NOT_EVALUATED"
    if checks and all(c["status"] == "PASS" for c in checks):
        status = "PASS"
    return {
        "status": status,
        "responses_sha256": _digest(_canonical(responses)),
        "checks": checks,
        "solver_version": next(iter(versions), None),
        "native_solver_status": "NOT_EVALUATED",
        "known_limits": LIMITS,
    }


def build_report(prepared: dict, assessment: dict) -> ReportSpec:
    """Populate the repository standard report form, preserving evidence limits."""
    from digitalmodel.reporting import (
        DocumentMeta,
        Provenance,
        ReportSpec,
        Section,
        TableBlock,
        TextBlock,
    )

    def section(key: str, title: str, *blocks: Any) -> Section:
        return Section(key=key, title=title, blocks=list(blocks))

    def display(value: float | None) -> str:
        return "NOT_EVALUATED" if value is None else f"{value:.1f}"

    cfg = prepared["input"]
    return ReportSpec(
        document=DocumentMeta(
            number="C0000-RPT-001-00",
            revision="00",
            title="CALM buoy and single point mooring analysis",
            project="Generic worked example",
            client="Not applicable",
        ),
        provenance=Provenance()
        .add(
            "input", "input.yml with explicit criteria", digest=prepared["input_sha256"]
        )
        .add(
            "exported_extrema",
            "response collection",
            digest=assessment["responses_sha256"],
        ),
        input_echo=cfg,
        sections=[
            section(
                "summary",
                "Summary",
                TextBlock(
                    markdown=(
                        f"Offline preparation of {len(prepared['cases'])} generic combined-load cases. "
                        f"Imported scalar comparison: {assessment['status']}. "
                        "Engineering acceptance and native solver results: NOT_EVALUATED. "
                        + LIMITS
                    )
                ),
            ),
            section(
                "revision-history",
                "Revision history",
                TableBlock(
                    title="Issue history",
                    columns=["Revision", "Date", "Change and reason"],
                    rows=[
                        [
                            "00",
                            cfg["report_date"],
                            "Initial offline workflow and worked example; solver evidence pending",
                        ]
                    ],
                ),
            ),
            section(
                "objective",
                "Objective",
                TextBlock(
                    markdown=(
                        "Prepare a buoy, tanker, hawser, six anchor legs and transfer hose for a reviewed "
                        "analysis. Compare only supplied exported extrema with sourced input criteria."
                    )
                ),
            ),
            section(
                "design-data",
                "Design data and case table",
                TableBlock(
                    title="Combined environmental cases; regular-wave height H, not Hs",
                    columns=[
                        "Case",
                        "Condition",
                        "H (m)",
                        "T (s)",
                        "Wave (deg)",
                        "Current (m/s)",
                        "Current (deg)",
                        "Wind (m/s)",
                        "Wind (deg)",
                        "Source",
                    ],
                    rows=[
                        [
                            c["case_id"],
                            c["condition"],
                            *[
                                display(c[k])
                                for k in (
                                    "wave_height_m",
                                    "wave_period_s",
                                    "wave_direction_deg",
                                    "current_speed_m_s",
                                    "current_direction_deg",
                                    "wind_speed_m_s",
                                    "wind_direction_deg",
                                )
                            ],
                            c["source"],
                        ]
                        for c in prepared["cases"]
                    ],
                ),
                TextBlock(
                    markdown="Coupling input source: "
                    + cfg["coupling"]["source"]
                    + ". Heading source: "
                    + cfg["headings_source"]
                ),
            ),
            section(
                "method",
                "Analysis methodology",
                TextBlock(
                    markdown=(
                        "Flatten generic template includes; retain original sources and SHA-256 digests; "
                        "set each combined wind, regular wave and current case; validate native YAML structure. "
                        "No native load or solve is performed. Te is the whole-line maximum effective tension. "
                        "Ut = Te / allowable tension. Uk = maximum curvature × constant minimum bend radius. "
                        "Bend radius R = 1 / curvature; a straight line has infinite radius, represented by null. "
                        "Normalised values at or below unity meet the supplied scalar limit. "
                        "Missing data/criteria are NOT_EVALUATED; hand calculations are ILLUSTRATIVE. "
                        "Definitions: "
                        + FORCES
                        + " and "
                        + MOMENTS
                        + " ("
                        + SOURCE_EDITION
                        + ")."
                    )
                ),
            ),
            section(
                "results",
                "Results",
                TableBlock(
                    title="Whole-line extrema and supplied criteria; tension in kN, curvature in inverse metres",
                    columns=[
                        "Case",
                        "Object",
                        "Quantity",
                        "Maximum",
                        "Limit",
                        "Limit unit",
                        "Limit source",
                        "Utilisation",
                        "Status",
                        "Evidence",
                    ],
                    rows=[
                        [
                            c["case_id"],
                            c["object"],
                            c["quantity"],
                            display(c["maximum"]),
                            display(c["criterion"]["value"])
                            if c["criterion"]
                            else "NOT_EVALUATED",
                            c["criterion"]["unit"] if c["criterion"] else "missing",
                            c["criterion"]["source"] if c["criterion"] else "missing",
                            display(c["utilisation"]),
                            c["status"],
                            c["evidence_kind"] or "missing",
                        ]
                        for c in assessment["checks"]
                    ],
                ),
            ),
            section(
                "validation-status",
                "Validation status",
                TextBlock(
                    markdown=(
                        "YAML structural validation passed for generated cases. This does not establish native "
                        "loadability or physical validity. Solver version: "
                        + (assessment["solver_version"] or "NOT_EVALUATED")
                        + ". Criteria are supplied inputs; no standard acceptance factors are assumed."
                    )
                ),
            ),
            section(
                "way-forward",
                "Way forward",
                TextBlock(
                    markdown=(
                        "Qualify tanker hydrodynamics, line units, hose properties and manufacturer limits; "
                        "review equilibrium and pretension; run the case matrix under separate solver authorization. "
                        "Record one solver version, model digest, complete analysis period and whole-line extrema "
                        "for every case. Review time/mesh/seed sensitivity and stationkeeping criteria before acceptance."
                    )
                ),
            ),
            section(
                "sources",
                "Sources",
                TableBlock(
                    title="Generic source inventory",
                    columns=["Repository-relative source", "SHA-256"],
                    rows=[
                        [p, digest] for p, digest in sorted(prepared["sources"].items())
                    ],
                ),
                TextBlock(
                    markdown="Public equations: "
                    + FORCES
                    + "; "
                    + MOMENTS
                    + ". "
                    + SOURCE_EDITION
                ),
            ),
        ],
    )


def main(argv: list[str] | None = None) -> int:
    """Prepare models and the standard report in an explicitly supplied directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--repo-root", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--responses", type=Path)
    parser.add_argument(
        "--criteria",
        type=Path,
        help="Explicit sourced scalar limits, replacing input criteria",
    )
    args = parser.parse_args(argv)
    config = yaml.safe_load(args.input.read_text(encoding="utf-8"))
    if args.criteria:
        config["criteria"] = yaml.safe_load(args.criteria.read_text(encoding="utf-8"))
    prepared = prepare_campaign(config, args.repo_root, args.output / "models")
    responses = json.loads(args.responses.read_text()) if args.responses else []
    assessment = assess_results(prepared, responses, config.get("criteria", {}))
    from digitalmodel.reporting import render_html

    report = render_html(build_report(prepared, assessment))
    payload = {"preparation": prepared, "assessment": assessment}
    # Same inputs have identical digests, so a completed preparation can be resumed.
    (args.output / "manifest.json").write_text(
        json.dumps(payload, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    (args.output / "report.html").write_text(report, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
