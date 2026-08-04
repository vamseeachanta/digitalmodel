#!/usr/bin/env python3
"""Build and run the #1528 static outlet/vent placement study.

The control retains the full-height pressure outlet.  The candidate closes that
patch and moves the atmospheric opening to the top of the tank.  Importing this
module only defines helpers; case construction and execution happen in ``main``.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
from collections.abc import Sequence
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
from digitalmodel.solvers.openfoam.sloshing_sweep import SweepConfig, generate_sweep

VARIANT_BASELINE = "baseline_full_height_outlet"
VARIANT_VENT_TOP = "vent_top"

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
    baseline = _exchange_boundary_conditions(flow_rate)
    if variant == VARIANT_BASELINE:
        return baseline
    if variant != VARIANT_VENT_TOP:
        raise ValueError(f"unknown outlet/vent variant: {variant}")

    inlet_conditions = [bc for bc in baseline if bc.patch_name == "inlet"]
    return inlet_conditions + [
        BoundaryCondition("outlet", BoundaryType.NO_SLIP, "U"),
        BoundaryCondition("outlet", BoundaryType.ZERO_GRADIENT, "p_rgh"),
        BoundaryCondition("outlet", BoundaryType.ZERO_GRADIENT, "alpha.water"),
        BoundaryCondition(
            "top",
            BoundaryType.PRESSURE_INLET_OUTLET_VELOCITY,
            "U",
            value="uniform (0 0 0)",
        ),
        BoundaryCondition(
            "top",
            BoundaryType.TOTAL_PRESSURE,
            "p_rgh",
            value="uniform 0",
            extra={"p0": "uniform 0"},
        ),
        BoundaryCondition(
            "top",
            BoundaryType.INLET_OUTLET,
            "alpha.water",
            value="uniform 0",
            extra={"inletValue": "uniform 0"},
        ),
    ]


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
        description="Build and run the static OpenFOAM outlet/vent placement study."
    )
    parser.add_argument("--work-root", required=True, type=Path)
    parser.add_argument("--ranks", type=int, default=8)
    parser.add_argument("--end-time", type=float, default=10.0)
    parser.add_argument("--flow-rate", type=float, default=0.0)
    parser.add_argument("--tolerance-pct", required=True, type=float)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument(
        "--variants",
        nargs="+",
        choices=(VARIANT_BASELINE, VARIANT_VENT_TOP),
        default=[VARIANT_BASELINE, VARIANT_VENT_TOP],
    )
    parser.add_argument("--dry-run", action="store_true")
    return parser


def _validate_args(args: argparse.Namespace) -> None:
    if not 1 <= args.ranks <= MAX_MPI_RANKS:
        raise ValueError(f"ranks must be between 1 and {MAX_MPI_RANKS}")
    if args.end_time <= 0.0:
        raise ValueError("end-time must be positive")
    if args.tolerance_pct < 0.0:
        raise ValueError("tolerance-pct must be non-negative")


def _reference_sweep_case():
    sweep = generate_sweep(
        SweepConfig(
            length=20.0,
            tank_height=10.0,
            study_name="dm1528-filldrain",
        )
    )
    try:
        return next(case for case in sweep.cases if case.case_id == REFERENCE_CASE_ID)
    except StopIteration as exc:
        raise RuntimeError(
            f"reference sweep case {REFERENCE_CASE_ID!r} was not generated"
        ) from exc


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
) -> tuple[Path, int]:
    case = map_sweep_case_to_openfoam_case(_reference_sweep_case(), _coupling_spec())
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


def _print_summary(manifest: dict) -> None:
    if manifest["dry_run"]:
        print(f"Built {len(manifest['variants'])} case(s); dry-run, solver not invoked.")
        return
    for variant, result in manifest["variants"].items():
        print(f"{variant}: liquid-volume drift {result['drift_pct']:.6g}%")
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
        "variants": {},
        "verdict": None,
    }


def main(argv: Sequence[str] | None = None) -> int:
    """Build selected variants, optionally run them, and write the manifest."""
    args = build_parser().parse_args(argv)
    _validate_args(args)
    variants = list(dict.fromkeys(args.variants))
    args.work_root.mkdir(parents=True, exist_ok=True)

    built: dict[str, tuple[Path, int]] = {}
    for variant in variants:
        built[variant] = _build_case(
            args.work_root, variant, args.flow_rate, args.end_time, args.ranks
        )

    manifest = _initial_manifest(args)

    if args.dry_run:
        for variant, (case_dir, cell_count) in built.items():
            manifest["variants"][variant] = {
                "case_directory": str(case_dir),
                "cell_count": cell_count,
                "status": "built",
            }
    else:
        _require_openfoam_environment()
        for variant, (case_dir, estimated_cells) in built.items():
            _run_case(case_dir, args.ranks)
            series = _volume_series(case_dir)
            manifest["variants"][variant] = {
                "case_directory": str(case_dir),
                "drift_pct": volume_drift_percent(series),
                "initial_volume_m3": series[0][1],
                "final_volume_m3": series[-1][1],
                "cell_count": _cell_count_from_log(case_dir, estimated_cells),
                "status": "completed",
            }
        if VARIANT_BASELINE in built and VARIANT_VENT_TOP in built:
            manifest["verdict"] = decide_outlet_placement(
                manifest["variants"][VARIANT_BASELINE]["drift_pct"],
                manifest["variants"][VARIANT_VENT_TOP]["drift_pct"],
                args.tolerance_pct,
            )

    _write_manifest(args.output, manifest)
    _print_summary(manifest)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
