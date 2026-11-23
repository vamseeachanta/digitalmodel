#!/usr/bin/env python
"""Run the 2D CFD sloshing benchmark battery for the rectangular master curve.

Epic #1429. Two modes, both real OpenFOAM (interFoam VOF) runs on this box:

* ``free-decay`` (issue #1430) — a rectangular free-decay natural-frequency
  battery across several fills. Each case rings down a small first-mode
  perturbation; the FFT natural frequency is converted to the dimensionless
  master-curve ordinate ``Omega1 = 2*pi*f*sqrt(L/g)`` at ``x = h/L`` and compared
  to the analytical ``sqrt(pi*tanh(pi*x))``. These are the measured points that
  land on the rectangular curve in the explorer.

* ``forced-roll`` (issue #1431) — one fill driven at three roll periods
  bracketing the analytical first mode ``T1`` ({0.85, 1.0, 1.15}*T1). The
  free-surface response and first-harmonic roll-moment amplitude peak at ``T1``,
  confirming the free-decay natural period is the resonant period.

Both modes append to a single committed manifest
``docs/api/structural/sloshing-cfd-benchmark.json`` (small derivative data only;
the heavy case trees stay in ``--work-dir`` outside the repo).

Reuses the validated framework (do not rebuild):
  digitalmodel.solvers.openfoam.validation.sloshing_2d  (build/analyze cases)
  digitalmodel.solvers.openfoam.validation.sloshing_sweep.reduce_roll_moment
  digitalmodel.solvers.openfoam.runner.OpenFOAMRunner

Run inside an OpenFOAM environment, e.g.::

    source /usr/lib/openfoam/openfoam2312/etc/bashrc
    uv run python scripts/cfd/run_sloshing_benchmark.py all \
        --work-dir /mnt/local-analysis/sloshing_cfd_work
"""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Any, Dict, List

from digitalmodel.solvers.openfoam.runner import OpenFOAMRunConfig, OpenFOAMRunner
from digitalmodel.solvers.openfoam.validation.sloshing_2d import (
    SloshingForcedRollConfig,
    SloshingFreeDecayConfig,
    analyze_free_decay,
    build_forced_roll_case,
    build_free_decay_case,
    parse_interface_height,
    parse_roll_moment,
)
from digitalmodel.solvers.openfoam.validation.sloshing_sweep import reduce_roll_moment

G = 9.80665
_REPO = Path(__file__).resolve().parents[2]
_MANIFEST = _REPO / "docs" / "api" / "structural" / "sloshing-cfd-benchmark.json"

# Free-decay battery: L = 1.0 m rectangular tank; fills spread across the curve.
# 0.30 / 0.70 reproduce the known 0.29% / 0.002% points; 0.15 / 0.50 are new.
FREE_DECAY_BREADTH = 1.0
FREE_DECAY_TANK_HEIGHT = 1.0  # tall enough for freeboard even at h/L = 0.70
FREE_DECAY_FILLS = (0.15, 0.30, 0.50, 0.70)

# Forced-roll resonance: SPHERIC-style tank, one fill, 3 periods bracketing T1.
FORCED_BREADTH = 0.9
FORCED_TANK_HEIGHT = 0.508
FORCED_FILL_DEPTH = 0.315  # h/L ~ 0.35 (energetic mid fill)
FORCED_ROLL_DEG = 4.0
FORCED_PERIOD_RATIOS = (0.85, 1.0, 1.15)


def _omega1(freq_hz: float, breadth: float) -> float:
    """Dimensionless master-curve ordinate Omega1 = 2*pi*f*sqrt(Lc/g)."""
    return 2.0 * math.pi * freq_hz * math.sqrt(breadth / G)


# --------------------------------------------------------------------------- #
# Free-decay battery (issue #1430)
# --------------------------------------------------------------------------- #


def run_free_decay(work_dir: Path) -> Dict[str, Any]:
    points: List[Dict[str, Any]] = []
    for fill_hl in FREE_DECAY_FILLS:
        # fill_depth h = fill_hl * L; expressed as a fraction of tank_height.
        fill_level = fill_hl * FREE_DECAY_BREADTH / FREE_DECAY_TANK_HEIGHT
        config = SloshingFreeDecayConfig(
            breadth=FREE_DECAY_BREADTH,
            tank_height=FREE_DECAY_TANK_HEIGHT,
            fill_level=fill_level,
            name=f"freedecay_hl{round(fill_hl * 100):02d}",
        )
        case_dir = build_free_decay_case(config, work_dir)
        runner = OpenFOAMRunner(
            OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)
        )
        result = runner.run(case_dir)
        row: Dict[str, Any] = {
            "h_over_L": round(config.fill_depth / config.breadth, 4),
            "fill_depth_m": round(config.fill_depth, 5),
            "case": config.name,
            "mesh": [config.nx, config.ny, 1],
            "solver_status": result.status.value,
            "runtime_s": round(result.duration_seconds, 1),
        }
        if result.status.value == "completed":
            a = analyze_free_decay(case_dir, config)
            row.update(
                freq_meas_hz=round(a["measured_frequency"], 6),
                freq_analytical_hz=round(a["analytical_frequency"], 6),
                omega1_meas=round(_omega1(a["measured_frequency"], config.breadth), 5),
                omega1_analytical=round(
                    _omega1(a["analytical_frequency"], config.breadth), 5
                ),
                rel_error=round(a["relative_error"], 5),
                within_tolerance=bool(a["within_tolerance"]),
            )
        else:
            row["error"] = result.error_message
        print(f"[free-decay] h/L={row['h_over_L']}: {row.get('solver_status')} "
              f"err={row.get('rel_error', 'n/a')}")
        points.append(row)
    return {
        "shape": "rectangular",
        "breadth_m": FREE_DECAY_BREADTH,
        "tank_height_m": FREE_DECAY_TANK_HEIGHT,
        "tolerance": 0.05,
        "n_points": len(points),
        "points": points,
    }


# --------------------------------------------------------------------------- #
# Forced-roll resonance (issue #1431)
# --------------------------------------------------------------------------- #


def _response_amplitude(times: List[float], elev: List[float]) -> float:
    """Half peak-to-peak of the interface signal over the last 60% (steady part)."""
    if not elev:
        return 0.0
    n0 = int(0.4 * len(elev))
    tail = elev[n0:] or elev
    return 0.5 * (max(tail) - min(tail))


def run_forced_roll(work_dir: Path) -> Dict[str, Any]:
    base = SloshingForcedRollConfig(
        breadth=FORCED_BREADTH,
        tank_height=FORCED_TANK_HEIGHT,
        fill_depth=FORCED_FILL_DEPTH,
        roll_amplitude_deg=FORCED_ROLL_DEG,
    )
    t1 = base.first_mode_period
    points: List[Dict[str, Any]] = []
    for ratio in FORCED_PERIOD_RATIOS:
        drive_period = round(ratio * t1, 5)
        config = SloshingForcedRollConfig(
            breadth=FORCED_BREADTH,
            tank_height=FORCED_TANK_HEIGHT,
            fill_depth=FORCED_FILL_DEPTH,
            roll_amplitude_deg=FORCED_ROLL_DEG,
            roll_period=drive_period,
            name=f"forcedroll_r{round(ratio * 100):03d}",
        )
        case_dir = build_forced_roll_case(config, work_dir, with_moment=True)
        runner = OpenFOAMRunner(
            OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)
        )
        result = runner.run(case_dir)
        row: Dict[str, Any] = {
            "period_ratio": ratio,
            "drive_period_s": drive_period,
            "drive_freq_hz": round(1.0 / drive_period, 5),
            "case": config.name,
            "solver_status": result.status.value,
            "runtime_s": round(result.duration_seconds, 1),
        }
        if result.status.value == "completed":
            try:
                times, elev = parse_interface_height(
                    case_dir, expected_height=config.fill_depth
                )
                row["response_amp_m"] = round(_response_amplitude(times, elev), 5)
            except (FileNotFoundError, RuntimeError):
                row["response_amp_m"] = None
            try:
                mt, mz = parse_roll_moment(case_dir)
                red = reduce_roll_moment(
                    mt, mz, drive_period,
                    fill_level=config.fill_level,
                    roll_amplitude_deg=FORCED_ROLL_DEG,
                )
                row.update(
                    moment_amplitude_nm=round(red["moment_amplitude"], 5),
                    in_phase_coeff=round(red["in_phase_coeff"], 5),
                    quad_coeff=round(red["quad_coeff"], 5),
                )
            except (FileNotFoundError, RuntimeError, ValueError):
                row["moment_amplitude_nm"] = None
        else:
            row["error"] = result.error_message
        print(f"[forced-roll] ratio={ratio} T={drive_period}s: "
              f"{row.get('solver_status')} amp={row.get('moment_amplitude_nm', 'n/a')}")
        points.append(row)

    # Resonant period = the one with the largest FREE-SURFACE RESPONSE. The
    # free-surface run-up (and the quadrature/damping coefficient) peak at the
    # natural period; the *total* reaction moment is NOT a resonance indicator —
    # at long drive periods it is dominated by the hydrostatic restoring moment
    # of the quasi-statically tilted surface (in_phase_coeff), which grows
    # monotonically with period. Fall back to quad_coeff, then moment amplitude.
    def _key(p: Dict[str, Any]) -> float:
        for k in ("response_amp_m", "quad_coeff", "moment_amplitude_nm"):
            v = p.get(k)
            if v is not None:
                return v
        return -1.0

    solved = [p for p in points if p["solver_status"] == "completed"]
    resonant = max(solved, key=_key)["drive_period_s"] if solved else None
    return {
        "shape": "rectangular",
        "breadth_m": FORCED_BREADTH,
        "tank_height_m": FORCED_TANK_HEIGHT,
        "fill_depth_m": FORCED_FILL_DEPTH,
        "h_over_L": round(FORCED_FILL_DEPTH / FORCED_BREADTH, 4),
        "roll_amplitude_deg": FORCED_ROLL_DEG,
        "first_mode_period_s": round(t1, 5),
        "resonance_metric": "free-surface response amplitude (response_amp_m)",
        "resonant_period_s": resonant,
        "n_points": len(points),
        "points": points,
    }


# --------------------------------------------------------------------------- #
# Manifest I/O
# --------------------------------------------------------------------------- #


def _load_manifest() -> Dict[str, Any]:
    if _MANIFEST.exists():
        return json.loads(_MANIFEST.read_text())
    return {
        "meta": {
            "generated_by": "scripts/cfd/run_sloshing_benchmark.py",
            "solver": "interFoam (VOF), OpenFOAM ESI v2312",
            "epic": "#1429",
            "g": G,
            "analytical_relation": "omega1 = sqrt(pi*tanh(pi*h/L))",
        }
    }


def _write_manifest(manifest: Dict[str, Any]) -> None:
    _MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    _MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"wrote {_MANIFEST.relative_to(_REPO)}")


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="2D CFD sloshing benchmark battery (#1429)")
    ap.add_argument("mode", choices=("free-decay", "forced-roll", "all"))
    ap.add_argument("--work-dir", type=Path,
                    default=Path("/mnt/local-analysis/sloshing_cfd_work"),
                    help="parent dir for the heavy OpenFOAM case trees (outside the repo)")
    args = ap.parse_args(argv)
    args.work_dir.mkdir(parents=True, exist_ok=True)

    manifest = _load_manifest()
    if args.mode in ("free-decay", "all"):
        manifest["free_decay"] = run_free_decay(args.work_dir)
        _write_manifest(manifest)
    if args.mode in ("forced-roll", "all"):
        manifest["forced_roll"] = run_forced_roll(args.work_dir)
        _write_manifest(manifest)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
