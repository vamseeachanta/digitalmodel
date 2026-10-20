"""Offline splash-to-landing campaign screening; no solver binding is imported.

Response/limit comparisons are project-defined arithmetic, not DNV acceptance.
Native readback, model qualification and benchmark approval remain separate gates.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import itertools
import json
import math
import shutil
from datetime import date as calendar_date
from pathlib import Path
from tempfile import TemporaryDirectory

from digitalmodel.weather_window.workflow import window_lengths_hours

PHASES = ("splash", "submerged", "landing")
PENDING = "NOT_EVALUATED"


def _number(value, *, positive=False, signed=False):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError("A finite numeric value is required")
    if not math.isfinite(value) or (
        not signed and (value <= 0 if positive else value < 0)
    ):
        raise ValueError("Value outside finite physical range")
    return value


def _text(value):
    if not isinstance(value, str) or not value.strip():
        raise ValueError("Nonblank source or identifier required")
    return value


def _quantity(item, unit, *, positive=False, signed=False):
    if item["unit"] != unit:
        raise ValueError(f"Expected unit {unit}")
    _text(item["source"])
    return _number(item["value"], positive=positive, signed=signed)


def build_cases(basis):
    """Expand the explicitly sourced axes; no default engineering values."""
    _text(basis["basis_id"])
    if basis["purpose"] not in ("illustrative", "project_screening"):
        raise ValueError("Unknown intended use")
    digest = basis["model_sha256"]
    if len(digest) != 64 or any(c not in "0123456789abcdef" for c in digest):
        raise ValueError("A model SHA-256 is required")
    axes = []
    for key, unit in (("hs", "m"), ("tp", "s"), ("headings", "deg")):
        axis = basis[key]
        values = [_quantity(item, unit, positive=key != "headings") for item in axis]
        if not values or len(set(values)) != len(values):
            raise ValueError("Axes must be nonempty and unique")
        # Heading convention is a single turn [0,360), degrees by definition.
        if key == "headings" and any(v >= 360 for v in values):
            raise ValueError("Headings must be in [0,360) degrees")
        axes.append(sorted(axis, key=lambda item: item["value"]))
    cases = []
    for index, (hs, tp, heading) in enumerate(itertools.product(*axes)):
        cases.append(
            {
                "case_id": f"case-{index:04d}",
                "hs_m": hs["value"],
                "tp_s": tp["value"],
                "heading_deg": heading["value"],
                "sources": {
                    "hs": hs["source"],
                    "tp": tp["source"],
                    "heading": heading["source"],
                },
            }
        )
    return cases


def _criteria(basis):
    criteria = basis["criteria"]
    keys = set()
    for criterion in criteria:
        key = (_text(criterion["name"]), criterion["phase"])
        if key in keys or key[1] not in PHASES:
            raise ValueError("Duplicate criterion or unsupported phase")
        keys.add(key)
        if criterion["bound"] not in ("maximum", "minimum"):
            raise ValueError("A maximum or minimum bound is required")
        _quantity(criterion["limit"], _text(criterion["limit"]["unit"]), signed=True)
    return criteria


def prepare_models(basis, source, output):
    """Overlay sea states on a complete, digest-pinned SI single-JONSWAP model.

    The caller supplies the entire physical model and its source ledger. No
    template coefficients, rigging or solver settings are invented here.
    YAML validation establishes structure only, not solver loadability.
    """
    import yaml

    from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump
    from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator
    from digitalmodel.workflows.structure_installation_prepare import _dependencies

    cases = build_cases(basis)
    source, output = Path(source), Path(output)
    if output.exists():
        raise FileExistsError(output)
    raw = source.read_bytes()
    if hashlib.sha256(raw).hexdigest() != basis["model_sha256"]:
        raise ValueError("Source model digest mismatch")
    validator = OrcaFlexYAMLValidator()
    # Validate the same snapshot that will be transformed, not a reopened input.
    manifest = {
        "basis_id": basis["basis_id"],
        "source_sha256": basis["model_sha256"],
        "engineering_status": PENDING,
        "models": [],
    }
    with TemporaryDirectory(prefix="lowering-") as directory:
        staging = Path(directory)
        snapshot = staging / "source.yml"
        snapshot.write_bytes(raw)
        if not validator.validate_file(snapshot).valid:
            raise ValueError("Source model failed YAML validation")
        model = yaml.load(raw.decode("utf-8-sig"), Loader=OrcaFlexLoader)
        if not isinstance(model, dict):
            raise ValueError("A complete model mapping is required")
        if model.get("General", {}).get("UnitsSystem") != "SI":
            raise ValueError("Source model requires explicit SI units")
        _dependencies(model)
        waves = model.get("Environment", {}).get("WaveTrains", [])
        if len(waves) != 1 or waves[0].get("WaveType") != "JONSWAP":
            raise ValueError("Exactly one explicit JONSWAP wave train is supported")
        # Orcina OrcaFlex web help (accessed 2026-10-20), Environment:
        # Data for JONSWAP and ISSC spectra, Setting the data / Spectral parameters.
        # Automatic uses Tz; fully specified derives Hs. Only partially specified
        # accepts independent Hs/Tp, with gamma set BEFORE Tp.
        if waves[0].get("WaveJONSWAPParameters") != "Partially specified":
            raise ValueError("Partially specified JONSWAP parameters are required")
        _number(waves[0].get("WaveGamma"), positive=True)
        if "WaveTz" in waves[0] or "WaveFm" in waves[0]:
            raise ValueError("Remove linked Tz/Fm inputs before a Tp sweep")
        generated = staging / "generated"
        generated.mkdir()
        for case in cases:
            changed = copy.deepcopy(model)
            changed["Environment"]["WaveTrains"][0].pop("WaveTp", None)
            changed["Environment"]["WaveTrains"][0].update(
                WaveHs=case["hs_m"],
                WaveTp=case["tp_s"],
                WaveDirection=case["heading_deg"],
            )
            target = generated / (case["case_id"] + ".yml")
            orcaflex_dump(changed, target)
            validation = validator.validate_file(target)
            if not validation.valid:
                raise ValueError("Generated model failed YAML validation")
            if yaml.load(target.read_text(), Loader=OrcaFlexLoader) != changed:
                raise ValueError("Generated model semantic readback mismatch")
            manifest["models"].append(
                {
                    **case,
                    "file": target.name,
                    "sha256": hashlib.sha256(target.read_bytes()).hexdigest(),
                    "yaml_valid": True,
                    "warnings": [i.message for i in validation.warnings],
                }
            )
        (generated / "manifest.json").write_text(
            json.dumps(manifest, indent=2), encoding="utf-8"
        )
        shutil.copytree(generated, output)
        if json.loads((output / "manifest.json").read_text()) != manifest:
            raise ValueError("Manifest readback mismatch")
    return manifest


def _checks(record, criteria):
    metrics = {}
    for metric in record.get("metrics", []):
        key = (metric["name"], metric["phase"])
        if key in metrics:
            raise ValueError("Duplicate phase metric")
        metrics[key] = metric
    checks = []
    for criterion in criteria:
        metric = metrics.get((criterion["name"], criterion["phase"]))
        limit = criterion["limit"]
        check = {
            "name": criterion["name"],
            "phase": criterion["phase"],
            "bound": criterion["bound"],
            "limit": limit["value"],
            "unit": limit["unit"],
            "source": limit["source"],
            "value": None,
            "status": PENDING,
        }
        if metric is not None and metric["unit"] == limit["unit"]:
            value = _number(metric["value"], signed=True)
            passed = (
                value <= limit["value"]
                if criterion["bound"] == "maximum"
                else value >= limit["value"]
            )
            check.update(value=value, status="PASS" if passed else "FAIL")
        checks.append(check)
    return checks


def _weather(weather, limit):
    if weather is None or limit is None:
        return {
            "status": PENDING,
            "reason": "Complete screened envelope and weather input required",
        }
    _text(weather["source"])
    series = [_number(v) for v in weather["hs_timeseries_m"]]
    if not series:
        raise ValueError("Weather series must be nonempty")
    step = _number(weather["time_step_hours"], positive=True)
    duration = _number(weather["required_duration_hours"], positive=True)
    # Reuse the existing weather-window screen's inclusive Hs comparison.
    # Duration = count * time step; percent = 100 * fraction (unit definition).
    windows = window_lengths_hours(series, limit, step)
    viable = sum(length >= duration for length in windows)
    return {
        "status": "PASS" if viable else "FAIL",
        "hs_limit_m": limit,
        "workability_pct": 100 * sum(v <= limit for v in series) / len(series),
        "longest_window_hours": max(windows, default=0),
        "num_viable_windows": viable,
        "source": weather["source"],
        "required_duration_hours": duration,
        "time_step_hours": step,
        "limitation": "Sample persistence only; no forecast uncertainty or operational approval",
    }


def assess_sweep(basis, records, weather=None):
    """Require every period, heading and phase before deriving a grid envelope.

    The limit is the highest contiguous passing sampled Hs from the lowest Hs.
    No interpolation or extrapolation is performed. Imported extrema are never
    treated as verified native results, even when their identifiers agree.
    """
    cases = build_cases(basis)
    criteria = _criteria(basis)
    by_id = {}
    for record in records:
        key = record["case_id"]
        if key in by_id or key not in {c["case_id"] for c in cases}:
            raise ValueError("Duplicate or unknown result case")
        by_id[key] = record
    versions = {r.get("solver_version") for r in records}
    evidence_matches = len(versions) == 1 and all(
        isinstance(r.get("solver_version"), str)
        and r["solver_version"].strip()
        and r.get("basis_id") == basis["basis_id"]
        and r.get("model_sha256") == basis["model_sha256"]
        and r.get("kind") == basis["purpose"]
        for r in records
    )
    phase_coverage = {c["phase"] for c in criteria} == set(PHASES)
    for case in cases:
        record = by_id.get(case["case_id"], {})
        state_matches = record.get("sea_state") == {
            key: case[key] for key in ("hs_m", "tp_s", "heading_deg")
        }
        checks = _checks(record, criteria)
        statuses = [check["status"] for check in checks]
        status = (
            PENDING
            if not evidence_matches
            or not state_matches
            or not phase_coverage
            or not statuses
            or PENDING in statuses
            else "FAIL"
            if "FAIL" in statuses
            else "PASS"
        )
        case.update(status=status, checks=checks)
    levels = []
    for hs in sorted({case["hs_m"] for case in cases}):
        statuses = [case["status"] for case in cases if case["hs_m"] == hs]
        levels.append(
            {
                "hs_m": hs,
                "status": PENDING
                if PENDING in statuses
                else "FAIL"
                if "FAIL" in statuses
                else "PASS",
            }
        )
    complete = all(level["status"] != PENDING for level in levels)
    limit = None
    if complete:
        for level in levels:
            if level["status"] != "PASS":
                break
            limit = level["hs_m"]
    return {
        "basis_id": basis["basis_id"],
        "purpose": basis["purpose"],
        "solver_versions": sorted(v for v in versions if isinstance(v, str)),
        "cases": cases,
        "levels": levels,
        "screened_hs_limit_m": limit,
        "screening_status": (
            PENDING if not complete else "PASS" if limit is not None else "FAIL"
        ),
        "engineering_status": PENDING,
        "limitations": [
            "Imported extrema are not native-output qualification.",
            "No approved public numerical benchmark is established.",
            "Model, rigging, buoy properties and lowering history require qualification.",
            "Envelope is restricted to sampled periods and headings; no interpolation.",
            "Project criteria are required; no standards-derived limits are supplied.",
        ],
        "weather_window": _weather(weather, limit),
    }


def build_report(basis, result, *, date):
    """Render the standard report form with comparisons and explicit gaps."""
    from digitalmodel.reporting import (
        DocumentMeta,
        Provenance,
        ReportSpec,
        RevisionRow,
        Section,
        TableBlock,
        TextBlock,
        render_html,
    )
    from digitalmodel.reporting.thickness import revision_section

    rows = [
        [
            case["case_id"],
            f"{case['hs_m']:.1f}",
            f"{case['tp_s']:.1f}",
            f"{case['heading_deg']:.1f}",
            case["status"],
        ]
        for case in result["cases"]
    ]
    comparisons = [
        [
            case["case_id"],
            check["phase"],
            check["name"],
            "Not evaluated" if check["value"] is None else f"{check['value']:.1f}",
            check["bound"],
            f"{check['limit']:.1f}",
            check["unit"],
            check["status"],
            check["source"],
        ]
        for case in result["cases"]
        for check in case["checks"]
    ]
    limit = result["screened_hs_limit_m"]
    limit_text = "not established" if limit is None else f"{limit:.1f} m"

    def display(value):
        if isinstance(value, float):
            return f"{value:.1f}"
        if isinstance(value, dict):
            return {key: display(item) for key, item in value.items()}
        if isinstance(value, list):
            return [display(item) for item in value]
        return value

    spec = ReportSpec(
        document=DocumentMeta(
            number="G0000-RPT-001-00",
            revision="00",
            title="Subsea structure lowering analysis",
            project="Generic installation",
            client="Generic",
            date=date,
            revision_history=[
                RevisionRow(
                    rev="00",
                    date=date,
                    description="Initial offline screening; qualification remains open",
                )
            ],
        ),
        provenance=Provenance().add(
            "input", basis["basis_id"], description=basis["purpose"]
        ),
        sections=[
            Section(
                key="summary",
                title="Summary",
                blocks=[
                    TextBlock(
                        markdown=(
                            f"Purpose: {basis['purpose']}. Engineering status: **{PENDING}**. "
                            f"Screened grid Hs limit: {limit_text}. "
                            "The comparisons below govern the sampled envelope. "
                            "Qualification and approved criteria are required before operational use."
                        )
                    )
                ],
            ),
            Section(
                key="cases",
                title="Case table",
                blocks=[
                    TableBlock(
                        title="Sourced sea-state sweep",
                        columns=["Case", "Hs (m)", "Tp (s)", "Heading (deg)", "Status"],
                        rows=rows,
                    ),
                    TextBlock(
                        markdown=json.dumps(
                            {c["case_id"]: c["sources"] for c in result["cases"]},
                            indent=2,
                        )
                    ),
                ],
            ),
            Section(
                key="comparisons",
                title="Criterion comparisons",
                blocks=[
                    TableBlock(
                        title="Phase response against supplied limit",
                        columns=[
                            "Case",
                            "Phase",
                            "Metric",
                            "Value",
                            "Bound",
                            "Limit",
                            "Unit",
                            "Status",
                            "Source",
                        ],
                        rows=comparisons,
                    )
                ],
            ),
            Section(
                key="weather",
                title="Weather-window screen",
                blocks=[
                    TextBlock(
                        markdown=json.dumps(display(result["weather_window"]), indent=2)
                    )
                ],
            ),
            Section(
                key="limits",
                title="Limitations and way forward",
                blocks=[TextBlock(markdown="\n\n".join(result["limitations"]))],
            ),
        ],
        input_echo=display(basis),
    )
    spec.sections.insert(1, revision_section(spec.document))
    spec.sections.append(
        Section(
            key="evidence",
            title="Evidence identifiers",
            blocks=[
                TextBlock(
                    markdown=json.dumps(
                        {
                            key: value
                            for key, value in result.items()
                            if key.endswith("sha256") or key == "solver_versions"
                        },
                        indent=2,
                    )
                )
            ],
        )
    )
    return render_html(spec)


def main(argv=None):
    """Read JSON basis/results, write a new output directory, never run a solver."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("basis", type=Path)
    parser.add_argument("--records", type=Path)
    parser.add_argument("--weather", type=Path)
    parser.add_argument(
        "--model",
        type=Path,
        help="Complete single-JONSWAP YAML for offline case preparation",
    )
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--date", default=calendar_date.today().isoformat())
    args = parser.parse_args(argv)
    raw = args.basis.read_bytes()
    basis = json.loads(raw)
    records_raw = args.records.read_bytes() if args.records else b"[]"
    weather_raw = args.weather.read_bytes() if args.weather else b"null"
    result = assess_sweep(basis, json.loads(records_raw), json.loads(weather_raw))
    result.update(
        basis_sha256=hashlib.sha256(raw).hexdigest(),
        records_sha256=hashlib.sha256(records_raw).hexdigest(),
        weather_sha256=hashlib.sha256(weather_raw).hexdigest(),
    )
    html = build_report(basis, result, date=args.date)
    if args.output.exists():
        raise FileExistsError(args.output)
    if args.model:
        prepare_models(basis, args.model, args.output / "models")
    args.output.mkdir(parents=True, exist_ok=args.model is not None)
    (args.output / "assessment.json").write_text(
        json.dumps(result, indent=2, allow_nan=False), encoding="utf-8"
    )
    (args.output / "report.html").write_text(html, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
