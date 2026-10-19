"""Build and run static or resonant-roll outlet/vent placement holds.

The control retains the full-height pressure outlet.  The candidate closes that
patch and moves the atmospheric opening to the top of the tank.  Importing this
module only defines helpers; case construction and execution happen in ``main``.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import re
import subprocess
from collections.abc import Sequence
from dataclasses import asdict
from datetime import UTC, datetime
from pathlib import Path

from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.case_coupling import (
    CouplingSpec,
    _exchange_boundary_conditions,
    map_sweep_case_to_openfoam_case,
)
from digitalmodel.solvers.openfoam.gravity_conduit import ConduitGeometry
from digitalmodel.solvers.openfoam.models import BoundaryCondition, BoundaryType
from digitalmodel.solvers.openfoam.sloshing_sweep import (
    SweepCase,
    SweepConfig,
    generate_sweep,
)
from digitalmodel.solvers.openfoam.time_history import ExtractionConfig
from digitalmodel.solvers.openfoam.vent_runup import (
    assess_vent_hold,
    worst_fill_resonant_cases,
)

VARIANT_BASELINE = "baseline_full_height_outlet"
VARIANT_VENT_TOP = "vent_top"

def default_tolerance_pct(config: ExtractionConfig | None = None) -> float:
    """Percent form of the repo's relative mass-balance tolerance.

    The volume-balance verdict is judged against the tolerance the repo already
    applies to these cases, not against a threshold invented for this study and
    not against a number typed on the command line.
    ``ExtractionConfig.mass_balance_rtol`` is the RELATIVE mass-balance
    tolerance ``time_history`` uses to decide ``mass_balance_ok`` for the
    extracted histories; expressed in percent it is this study's pass mark.

    Taking the config as a parameter is what makes the derivation testable. A
    test that only asserts the shipped value equals 0.1 cannot distinguish this
    from a hardcoded ``0.1``, because the derived value IS 0.1; driving it with
    a different config can.
    """
    return 100.0 * (config or ExtractionConfig()).mass_balance_rtol


# Overridable via --tolerance-pct for sensitivity work.
DEFAULT_TOLERANCE_PCT = default_tolerance_pct()

# This stable identifier is generated from the issue-specified physical sweep.
REFERENCE_CASE_ID = "dm1528-filldrain-2a44db9cf998"

# The target CFD node has eight available MPI ranks; reject oversubscription.
MAX_MPI_RANKS = 8

# OpenFOAM may report a fatal error while returning process status zero.
_FATAL_LOG_MARKERS = ("FOAM FATAL ERROR", "FOAM FATAL IO ERROR")

_LIQUID_VOLUME_FUNCTION = """

functions
{
    liquidVolume
    {
        type            volFieldValue;
        libs            (fieldFunctionObjects);
        operation       volIntegrate;
        fields          (alpha.water);
        regionType      all;
        writeControl    timeStep;
        writeFields     false;
        writeToFile     true;
    }
}
"""


def outlet_vent_boundary_conditions(
    flow_rate: float, variant: str
) -> list[BoundaryCondition]:
    """Return exchange boundary conditions for one placement variant."""
    inlet_conditions = [
        bc for bc in _exchange_boundary_conditions(flow_rate)
        if bc.patch_name == "inlet"
    ]

    if variant == VARIANT_BASELINE:
        # FROZEN control: the full-height pressure outlet as the library emitted
        # it BEFORE #1528 slice 7 settled the placement.
        #
        # Deliberately spelled out rather than read from
        # _exchange_boundary_conditions.  That helper now emits the settled vent
        # placement, so a control that tracked it would silently become a copy of
        # the treatment and the study would compare the vent against itself while
        # still passing every assertion.
        return inlet_conditions + [
            BoundaryCondition(
                "outlet",
                BoundaryType.PRESSURE_INLET_OUTLET_VELOCITY,
                "U",
                value="uniform (0 0 0)",
            ),
            BoundaryCondition(
                "outlet",
                BoundaryType.TOTAL_PRESSURE,
                "p_rgh",
                value="uniform 0",
                extra={"p0": "uniform 0"},
            ),
            BoundaryCondition(
                "outlet",
                BoundaryType.INLET_OUTLET,
                "alpha.water",
                value="uniform 0",
                extra={"inletValue": "uniform 0"},
            ),
        ]

    if variant != VARIANT_VENT_TOP:
        raise ValueError(f"unknown outlet/vent variant: {variant}")

    # The treatment IS the shipped library default, so re-running this study
    # validates what the 144-case matrix will actually emit rather than a
    # script-local copy of it.
    return _exchange_boundary_conditions(flow_rate)


def parse_vol_field_value(path: str | Path) -> list[tuple[float, float]]:
    """Parse time and the final value column from a volFieldValue data file."""
    series: list[tuple[float, float]] = []
    with Path(path).open(encoding="utf-8") as stream:
        for raw_line in stream:
            line = raw_line.strip()
            if not line or line.startswith("#"):
                continue
            columns = line.split()
            series.append((float(columns[0]), float(columns[-1])))
    return series


def volume_drift_percent(series: list[tuple[float, float]]) -> float:
    """Return signed final-vs-initial liquid-volume drift in percent."""
    if len(series) < 2:
        raise ValueError("volume series must contain at least two points")
    initial_volume = series[0][1]
    if initial_volume <= 0.0:
        raise ValueError("initial liquid volume must be positive")
    return 100.0 * (series[-1][1] - initial_volume) / initial_volume


def decide_outlet_placement(
    baseline_drift_pct: float,
    vent_drift_pct: float,
    tolerance_pct: float,
) -> dict:
    """Apply the evidence-first placement decision rule."""
    if tolerance_pct < 0.0:
        raise ValueError("tolerance_pct must be non-negative")
    baseline_holds = bool(abs(baseline_drift_pct) <= tolerance_pct)
    vent_holds = bool(abs(vent_drift_pct) <= tolerance_pct)
    verdict = {
        "baseline_drift_pct": baseline_drift_pct,
        "vent_drift_pct": vent_drift_pct,
        "tolerance_pct": tolerance_pct,
        "baseline_holds_volume": baseline_holds,
        "vent_holds_volume": vent_holds,
        "recommended_variant": None,
    }
    if baseline_holds:
        verdict["recommended_variant"] = VARIANT_BASELINE
    elif vent_holds:
        verdict["recommended_variant"] = VARIANT_VENT_TOP
    else:
        verdict["inconclusive"] = True
    return verdict


def build_parser() -> argparse.ArgumentParser:
    """Create the command-line parser without reading process arguments."""
    parser = argparse.ArgumentParser(
        description="Build and run static or resonant OpenFOAM vent holds."
    )
    parser.add_argument("--work-root", required=True, type=Path)
    parser.add_argument("--ranks", type=int, default=8)
    parser.add_argument("--end-time", type=float, help="Static hold duration in seconds")
    parser.add_argument("--flow-rate", type=float, default=0.0)
    parser.add_argument(
        "--tolerance-pct", type=float, default=DEFAULT_TOLERANCE_PCT
    )
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument(
        "--variants",
        nargs="+",
        choices=(VARIANT_BASELINE, VARIANT_VENT_TOP),
        default=[VARIANT_BASELINE, VARIANT_VENT_TOP],
    )
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--reduce-only", action="store_true",
        help="Reassess a completed resonant manifest without rebuilding or solving",
    )
    parser.add_argument("--motion", choices=("static", "resonant"), default="static")
    parser.add_argument(
        "--sweep-case", choices=("reference", "worst-fill-resonant"),
        default="reference",
    )
    parser.add_argument("--cycles", type=int, help="Required whole roll cycles; no default")
    parser.add_argument(
        "--max-sample-gap-s", type=float,
        help="Required maximum time gap for resonant volume samples; no default",
    )
    parser.add_argument(
        "--runup-evidence", type=Path,
        help="Optional case-matched tank-frame spatial-maximum elevation JSON",
    )
    return parser


def _validate_args(args: argparse.Namespace) -> None:
    if not 1 <= args.ranks <= MAX_MPI_RANKS:
        raise ValueError(f"ranks must be between 1 and {MAX_MPI_RANKS}")
    for name in ("end_time", "flow_rate", "tolerance_pct", "max_sample_gap_s"):
        value = getattr(args, name)
        if value is not None and not math.isfinite(value):
            raise ValueError(f"{name} must be finite")
    if args.end_time is not None and args.end_time <= 0.0:
        raise ValueError("end-time must be positive")
    if args.tolerance_pct < 0.0:
        raise ValueError("tolerance-pct must be non-negative")
    if args.motion == "resonant":
        if args.cycles is None or args.cycles <= 0:
            raise ValueError("resonant motion requires positive --cycles from the run plan")
        if args.max_sample_gap_s is None or args.max_sample_gap_s <= 0:
            raise ValueError("resonant motion requires positive --max-sample-gap-s")
        if args.flow_rate != 0:
            raise ValueError("resonant vent hold requires zero flow-rate to isolate leakage")
        if args.end_time is not None:
            raise ValueError("resonant duration is set by --cycles, not --end-time")
    elif args.runup_evidence is not None or args.cycles is not None:
        raise ValueError("run-up evidence and cycles require resonant motion")
    if args.reduce_only and (args.motion != "resonant" or args.dry_run):
        raise ValueError("reduce-only requires resonant motion and cannot be a dry-run")


def _study_sweep():
    """Use the existing synthetic placement-study inputs (#1528 run configuration)."""
    return generate_sweep(
        SweepConfig(
            length=20.0,
            tank_height=10.0,
            study_name="dm1528-filldrain",
        )
    )


def _select_sweep_case(selection: str) -> SweepCase:
    sweep = _study_sweep()
    if selection == "worst-fill-resonant":
        candidates = worst_fill_resonant_cases(sweep.cases)
        if not candidates:
            raise ValueError("sweep contains no near-resonant cases")
        # Exact resonance first (Troll/Tnatural=1 by definition), then the
        # no-flow row. Retain all tied high-fill rows in the manifest for review.
        return min(candidates, key=lambda c: (
            abs(c.period_ratio - 1.0), c.conduit_capacity, c.case_id,
        ))
    if selection != "reference":
        raise ValueError("unknown sweep selection")
    try:
        return next(case for case in sweep.cases if case.case_id == REFERENCE_CASE_ID)
    except StopIteration as exc:
        raise RuntimeError(
            f"reference sweep case {REFERENCE_CASE_ID!r} was not generated"
        ) from exc


def _reference_sweep_case():
    return _select_sweep_case("reference")


def _coupling_spec() -> CouplingSpec:
    return CouplingSpec(
        tank_length=20.0,
        tank_width=6.0,
        tank_height=10.0,
        roll_amplitude_deg=5.0,
        conduit=ConduitGeometry(
            area=0.5,
            discharge_coefficient=0.6,
            loss_coefficient=1.5,
        ),
        dest_fill_fraction=0.30,
    )


def _append_liquid_volume_function(case_dir: Path) -> None:
    control_dict = case_dir / "system" / "controlDict"
    with control_dict.open("a", encoding="utf-8") as stream:
        stream.write(_LIQUID_VOLUME_FUNCTION)


def _build_case(
    work_root: Path,
    variant: str,
    flow_rate: float,
    end_time: float,
    ranks: int,
    *,
    sweep_case: SweepCase | None = None,
    motion: str = "static",
) -> tuple[Path, int]:
    if motion not in ("static", "resonant"):
        raise ValueError("unknown motion mode")
    row = sweep_case or _reference_sweep_case()
    if motion == "resonant" and (not row.near_resonance or flow_rate != 0):
        raise ValueError("resonant hold needs a near-resonant row and zero imposed flow")
    case = map_sweep_case_to_openfoam_case(row, _coupling_spec())
    if motion == "static":
        case.motion = None
    case.name = f"{case.name}-{variant}"
    case.solver_config.end_time = end_time
    case.solver_config.n_subdomains = ranks
    case.boundary_conditions = outlet_vent_boundary_conditions(flow_rate, variant)
    case_dir = OpenFOAMCaseBuilder(case).build(work_root / variant)
    _append_liquid_volume_function(case_dir)
    return case_dir, case.domain.estimate_cell_count()


def _openfoam_version() -> str | None:
    return os.environ.get("WM_PROJECT_VERSION") or os.environ.get("FOAM_API")


def _require_openfoam_environment() -> None:
    if not (os.environ.get("FOAM_APPBIN") or os.environ.get("WM_PROJECT_DIR")):
        raise RuntimeError(
            "OpenFOAM environment is not loaded: source it before running so "
            "FOAM_APPBIN or WM_PROJECT_DIR is set"
        )


def _run_utility(case_dir: Path, utility: str, command: Sequence[str]) -> None:
    log_path = case_dir / f"log.{utility}"
    with log_path.open("w", encoding="utf-8") as log:
        completed = subprocess.run(
            list(command),
            cwd=case_dir,
            stdout=log,
            stderr=subprocess.STDOUT,
            check=False,
            text=True,
        )
    log_text = log_path.read_text(encoding="utf-8", errors="replace")
    fatal_marker = next(
        (marker for marker in _FATAL_LOG_MARKERS if marker in log_text), None
    )
    if completed.returncode != 0 or fatal_marker is not None:
        detail = f"return code {completed.returncode}"
        if fatal_marker is not None:
            detail += f", log contains {fatal_marker!r}"
        raise RuntimeError(f"{utility} failed ({detail}); see {log_path}")


def _run_case(case_dir: Path, ranks: int) -> None:
    _run_utility(case_dir, "blockMesh", ["blockMesh"])
    if (case_dir / "system" / "setFieldsDict").exists():
        _run_utility(case_dir, "setFields", ["setFields"])
    if ranks > 1:
        _run_utility(case_dir, "decomposePar", ["decomposePar"])
        solver_command = ["mpirun", "-np", str(ranks), "interFoam", "-parallel"]
    else:
        solver_command = ["interFoam"]
    _run_utility(case_dir, "interFoam", solver_command)


def _volume_series(case_dir: Path) -> list[tuple[float, float]]:
    paths = list(
        case_dir.glob("postProcessing/liquidVolume/*/volFieldValue.dat")
    )
    if not paths:
        raise FileNotFoundError(
            f"no liquidVolume volFieldValue.dat found under {case_dir}"
        )

    def time_key(path: Path) -> tuple[int, float | str]:
        try:
            return (0, float(path.parent.name))
        except ValueError:
            return (1, path.parent.name)

    combined: dict[float, float] = {}
    for path in sorted(paths, key=time_key):
        combined.update(parse_vol_field_value(path))
    return sorted(combined.items())


def _cell_count_from_log(case_dir: Path, fallback: int) -> int:
    log_path = case_dir / "log.blockMesh"
    if not log_path.exists():
        return fallback
    match = re.search(r"^\s*cells:\s*(\d+)\s*$", log_path.read_text(), re.MULTILINE)
    return int(match.group(1)) if match else fallback


def _write_manifest(path: Path, manifest: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as stream:
        json.dump(manifest, stream, indent=2)
        stream.write("\n")


def _case_input_digest(case_dir: Path) -> str:
    """Hash stable case inputs; setFields rewrites alpha and generates polyMesh."""
    digest = hashlib.sha256()
    for directory in ("0", "constant", "system"):
        for path in sorted((case_dir / directory).rglob("*")):
            relative = path.relative_to(case_dir)
            if (not path.is_file() or relative.as_posix() == "0/alpha.water"
                    or relative.parts[:2] == ("constant", "polyMesh")):
                continue
            digest.update(relative.as_posix().encode())
            digest.update(b"\0")
            digest.update(path.read_bytes())
    return digest.hexdigest()


def _history_digest(series: list[tuple[float, float]]) -> str:
    return hashlib.sha256(json.dumps(series, allow_nan=False).encode()).hexdigest()


def _print_summary(manifest: dict) -> None:
    if manifest["dry_run"]:
        print(f"Built {len(manifest['variants'])} case(s); dry-run, solver not invoked.")
        return
    for variant, result in manifest["variants"].items():
        print(f"{variant}: liquid-volume drift {result['drift_pct']:.6g}%")
        if "assessment" in result:
            assessment = result["assessment"]
            print(f"Case assessment: {assessment['case_status']}; {assessment['checks']}")
    verdict = manifest.get("verdict")
    if verdict is not None:
        recommendation = verdict["recommended_variant"] or "inconclusive"
        print(f"Recommendation: {recommendation}")


def _initial_manifest(args: argparse.Namespace) -> dict:
    return {
        "study": "outlet_vent_placement",
        "generated_at_utc": datetime.now(UTC).isoformat(),
        "openfoam_version": _openfoam_version(),
        "ranks": args.ranks,
        "end_time_s": args.end_time,
        "flow_rate_m3_s": args.flow_rate,
        "tolerance_pct": args.tolerance_pct,
        "dry_run": bool(args.dry_run),
        "motion": args.motion,
        "variants": {},
        "verdict": None,
    }


def main(argv: Sequence[str] | None = None) -> int:
    """Build selected variants, optionally run them, and write the manifest."""
    args = build_parser().parse_args(argv)
    _validate_args(args)
    row = _select_sweep_case(args.sweep_case)
    if args.motion == "resonant":
        # Duration identity t = N*T; N is a required run-plan input, not an
        # assumed number of cycles sufficient for steady resonant response.
        args.end_time = args.cycles * row.roll_period_s
    elif args.end_time is None:
        args.end_time = 10.0  # Existing #1528 static-study run configuration.
    runup = None
    if args.runup_evidence is not None:
        runup = json.loads(args.runup_evidence.read_text(encoding="utf-8"))
    variants = list(dict.fromkeys(args.variants))
    previous = None
    if args.reduce_only:
        previous = json.loads(args.output.read_text(encoding="utf-8"))
        expected = {
            "motion": args.motion, "selected_case": asdict(row),
            "end_time_s": args.end_time, "flow_rate_m3_s": args.flow_rate,
            "ranks": args.ranks, "coupling_spec": json.loads(json.dumps(asdict(_coupling_spec()))),
        }
        if any(previous.get(key) != value for key, value in expected.items()):
            raise ValueError("completed manifest does not match requested rolling hold")
    else:
        if args.motion == "resonant" and args.output.exists():
            old = json.loads(args.output.read_text(encoding="utf-8"))
            if any(v.get("status") == "completed" for v in old.get("variants", {}).values()):
                raise ValueError("completed study exists; use --reduce-only to avoid rerunning")
        args.work_root.mkdir(parents=True, exist_ok=True)

    built: dict[str, tuple[Path, int]] = {}
    for variant in variants:
        if previous is not None:
            record = previous.get("variants", {}).get(variant, {})
            case_dir = args.work_root / variant / f"{row.case_id}-{variant}"
            if (record.get("status") != "completed"
                    or Path(record["case_directory"]).resolve() != case_dir.resolve()):
                raise ValueError("reduce-only requires a completed matching variant")
            if _case_input_digest(case_dir) != record.get("input_digest"):
                raise ValueError("case input digest mismatch; completed history cannot be reused")
            built[variant] = case_dir, record["cell_count"]
        else:
            built[variant] = _build_case(
                args.work_root, variant, args.flow_rate, args.end_time, args.ranks,
                sweep_case=row, motion=args.motion,
            )

    manifest = _initial_manifest(args)
    manifest["selected_case"] = asdict(row)
    manifest["highest_fill_near_resonant_case_ids"] = [
        c.case_id for c in worst_fill_resonant_cases(_study_sweep().cases)
    ]
    manifest["coupling_spec"] = asdict(_coupling_spec())
    manifest["cycles"] = args.cycles
    manifest["reduce_only"] = args.reduce_only
    manifest["matrix_status"] = "NOT_EVALUATED"
    manifest["resonance_basis"] = (
        "Matrix period uses tank length; prescribed roll is about x. "
        "Physical roll resonance requires axis/period review and measured response."
    )

    def assessment(series, variant):
        spec = _coupling_spec()
        return assess_vent_hold(
            series, case_id=row.case_id,
            initial_volume_m3=row.fill_fraction * spec.tank_volume,
            tank_height_m=spec.tank_height, fill_fraction=row.fill_fraction,
            roll_period_s=row.roll_period_s, cycles=args.cycles,
            tolerance_pct=args.tolerance_pct, max_sample_gap_s=args.max_sample_gap_s,
            runup_evidence=runup if variant == VARIANT_VENT_TOP else None,
        )

    pending_assessments = (
        {variant: assessment([], variant) for variant in variants}
        if args.motion == "resonant" else {}
    )

    if args.dry_run:
        for variant, (case_dir, cell_count) in built.items():
            manifest["variants"][variant] = {
                "case_directory": str(case_dir),
                "cell_count": cell_count,
                "status": "built",
            }
            if args.motion == "resonant":
                manifest["variants"][variant]["assessment"] = pending_assessments[variant]
    else:
        if not args.reduce_only:
            _require_openfoam_environment()
        for variant, (case_dir, estimated_cells) in built.items():
            input_digest = _case_input_digest(case_dir)
            if not args.reduce_only:
                _run_case(case_dir, args.ranks)
                if _case_input_digest(case_dir) != input_digest:
                    raise ValueError("case input digest changed during solver execution")
            series = _volume_series(case_dir)
            history_digest = _history_digest(series)
            if (previous is not None
                    and previous["variants"][variant].get("history_digest") != history_digest):
                raise ValueError("volume history digest mismatch")
            manifest["variants"][variant] = {
                "case_directory": str(case_dir),
                "drift_pct": volume_drift_percent(series),
                "initial_volume_m3": series[0][1],
                "final_volume_m3": series[-1][1],
                "cell_count": _cell_count_from_log(case_dir, estimated_cells),
                "status": "completed",
                "input_digest": input_digest,
                "history_digest": history_digest,
            }
            if args.motion == "resonant":
                result = manifest["variants"][variant]
                result["assessment"] = assessment(series, variant)
                result["drift_pct"] = result["assessment"]["final_drift_pct"]
                result["initial_volume_m3"] = result["assessment"]["initial_volume_m3"]
            # Retain each completed variant if a later variant cannot finish.
            _write_manifest(args.output, manifest)
        if (args.motion == "static"
                and VARIANT_BASELINE in built and VARIANT_VENT_TOP in built):
            manifest["verdict"] = decide_outlet_placement(
                manifest["variants"][VARIANT_BASELINE]["drift_pct"],
                manifest["variants"][VARIANT_VENT_TOP]["drift_pct"],
                args.tolerance_pct,
            )

    _write_manifest(args.output, manifest)
    _print_summary(manifest)
    if args.motion == "resonant" and not args.dry_run:
        return 0 if all(
            result["assessment"]["case_status"] == "PASS"
            for result in manifest["variants"].values()
        ) else 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
