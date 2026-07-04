#!/usr/bin/env python3
"""
ABOUTME: Fill / drive-frequency sweep harness (#641) for the ACMA B1546
ballast-tank sloshing study. Runs the matrix {fill x drive frequency} of 2D
forced-roll interFoam cases (built on the #658 motion engine + #659 partial
fill + the #639 validated 2D case) and, per case, extracts the tank
roll-reaction moment and reduces it to a first-harmonic amplitude/phase and an
in-phase (added-inertia / stiffness-like) and quadrature (damping-like)
coefficient. The per-point rows are the CONTRACT consumed by the dm#643 roll
coupling agent.

Sign convention (roll-reaction moment reduction)
------------------------------------------------
The imposed roll is ``theta(t) = A * sin(omega t)`` (engine YAW about z; A in
radians for the physics, reported in degrees). The extracted roll-reaction
moment about the z axis is fit at the drive frequency as::

    M(t) ~ M0 + Mc * cos(omega t) + Ms * sin(omega t)

- ``moment_amplitude`` = ``hypot(Mc, Ms)`` — first-harmonic amplitude (N.m).
- ``moment_phase_rad`` = ``atan2(Mc, Ms)`` — phase of M **relative to theta**
  (theta is a pure sine, so a moment purely in phase with theta has phase 0).
  Equivalently ``M(t) ~ M0 + moment_amplitude * sin(omega t + moment_phase_rad)``.

The moment is then split into components aligned with the *negatives* of the
displacement and the velocity (both "reaction" senses that oppose the motion):

- ``in_phase_coeff`` = moment component in phase with ``-theta(t)`` = ``-Ms``.
  This is the added-inertia / stiffness-like part. ``> 0`` means the moment
  opposes the roll displacement (restoring); ``< 0`` means it reinforces it.
- ``quad_coeff``     = moment component in phase with ``-theta_dot(t)`` = ``-Mc``
  (since ``theta_dot ~ cos(omega t)``). This is the damping-like part.
  ``> 0`` means the moment opposes the roll velocity (dissipative / positive
  damping); ``< 0`` means it feeds energy in.

Both coefficients are moment amplitudes (N.m) — they are NOT normalised by the
roll amplitude, so the dm#643 consumer divides by ``theta`` (or ``theta_dot``)
amplitude itself if it wants stiffness / damping coefficients in physical units.

Contract row (exact field names — do not deviate; dm#643 depends on them)::

    {"fill_level": 0.25, "drive_period": 1.9, "drive_freq_hz": 0.53,
     "roll_amplitude_deg": 4.0, "moment_amplitude": 0.0, "moment_phase_rad": 0.0,
     "in_phase_coeff": 0.0, "quad_coeff": 0.0}

Execution is a separate step from generation: :meth:`SloshingSweep.generate`
writes the nine case directories and a ``sweep_plan.json``; the orchestrator
fires the printed launch command later (the box is shared / the vessel geometry
is still being finalised); :meth:`SloshingSweep.collect` then parses each solved
case and writes the run manifest of contract rows.

Reference: digitalmodel #641 (fill / frequency sweep harness), building on #639
(validated 2D case), #658 (motion engine), #659 (partial fill).
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

from ..models import CaseType
from ..parametric import ParametricStudy, StudyParameter
from .sloshing_2d import (
    ROLL_MOMENT_FO_NAME,
    SloshingForcedRollConfig,
    build_forced_roll_case,
    parse_roll_moment,
)

# Contract field names (single source of truth for the dm#643 handoff schema).
CONTRACT_FIELDS: Tuple[str, ...] = (
    "fill_level",
    "drive_period",
    "drive_freq_hz",
    "roll_amplitude_deg",
    "moment_amplitude",
    "moment_phase_rad",
    "in_phase_coeff",
    "quad_coeff",
)

# StudyParameter names used to drive the ParametricStudy matrix.
FILL_PARAM = "fill"
PERIOD_PARAM = "period"


# ---------------------------------------------------------------------------
# Per-case moment reduction (the real new physics) -> the contract row
# ---------------------------------------------------------------------------


def reduce_roll_moment(
    times: Sequence[float],
    moment: Sequence[float],
    drive_period: float,
    *,
    fill_level: float,
    roll_amplitude_deg: float,
) -> Dict[str, float]:
    """First-harmonic reduction of a roll-reaction moment history -> contract row.

    Fits ``M(t) ~ M0 + Mc cos(wt) + Ms sin(wt)`` at the drive frequency
    ``w = 2*pi/drive_period`` by least squares (handles the non-uniform sampling
    of an adaptive-timestep solver), then decomposes per the module sign
    convention. Returns exactly the dm#643 contract fields.

    Args:
        times: Sample times (s), need not be uniform.
        moment: Roll-reaction moment about z at each time (N.m).
        drive_period: Imposed roll period (s); ``w = 2*pi/drive_period``.
        fill_level: Fill fraction for this case (carried into the row).
        roll_amplitude_deg: Imposed roll amplitude (deg; carried into the row).

    Returns:
        Dict with the eight contract fields.

    Raises:
        ValueError: If ``drive_period <= 0`` or fewer than 4 matching samples.
    """
    import numpy as np

    if drive_period <= 0.0:
        raise ValueError(f"drive_period must be > 0, got {drive_period}")
    t = np.asarray(times, dtype=float)
    m = np.asarray(moment, dtype=float)
    if t.size < 4 or t.size != m.size:
        raise ValueError("need >= 4 matching (time, moment) samples")

    omega = 2.0 * math.pi / drive_period
    basis = np.column_stack(
        [np.ones_like(t), np.cos(omega * t), np.sin(omega * t)]
    )
    coef, *_ = np.linalg.lstsq(basis, m, rcond=None)
    _m0, mc, ms = (float(c) for c in coef)

    amplitude = math.hypot(mc, ms)
    # Phase of M relative to theta (theta ~ sin(wt)):
    #   M1 sin(wt + phi) = M1 cos(phi) sin(wt) + M1 sin(phi) cos(wt)
    #   => Ms = M1 cos(phi), Mc = M1 sin(phi) => phi = atan2(Mc, Ms).
    phase = math.atan2(mc, ms)

    return {
        "fill_level": float(fill_level),
        "drive_period": float(drive_period),
        "drive_freq_hz": float(1.0 / drive_period),
        "roll_amplitude_deg": float(roll_amplitude_deg),
        "moment_amplitude": amplitude,
        "moment_phase_rad": phase,
        # component in phase with -theta(t) ~ -sin(wt) -> coefficient of (-sin) is -Ms
        "in_phase_coeff": -ms,
        # component in phase with -theta_dot(t) ~ -cos(wt) -> coefficient of (-cos) is -Mc
        "quad_coeff": -mc,
    }


# ---------------------------------------------------------------------------
# Sweep configuration
# ---------------------------------------------------------------------------


@dataclass
class SloshingSweepConfig:
    """Configuration for the fill / drive-frequency forced-roll sweep (#641).

    The sweep is the Cartesian product ``fill_levels x drive_periods`` — the
    default 3x3 = 9 forced-roll cases. Each case is a 2D forced-roll interFoam
    run (SPHERIC-Test-10-style tank by default) with the tank roll-reaction
    moment functionObject enabled.

    The three drive periods are, per the #641 brief, the **roll natural period**
    plus **two target periods**. The defaults here are PLACEHOLDERS derived from
    the tank first-mode period so the harness is runnable end-to-end now; the
    orchestrator overrides ``drive_periods`` with the finalised vessel roll
    natural period + two excitation targets once the B1546 geometry lands. The
    contract field names are what dm#643 depends on — not these placeholder
    numbers.

    Attributes:
        fill_levels: Fill fractions to sweep (of ``tank_height``).
        drive_periods: Drive periods (s): roll natural period + 2 targets. If
            ``None``, three periods are derived from the reference tank
            first-mode period (see :meth:`default_drive_periods`).
        breadth: Tank breadth L (m).
        tank_height: Tank height (m).
        roll_amplitude_deg: Forced-roll amplitude (deg), common to all cases.
        cells_per_breadth: Uniform mesh cells across the breadth.
        n_cycles: Number of drive periods to run each case.
        moment_write_interval: ``timeStep`` write stride for the moment FO.
        study_name: Prefix for the generated case directory names.
    """

    fill_levels: Tuple[float, ...] = (0.25, 0.50, 0.70)
    drive_periods: Optional[Tuple[float, ...]] = None
    breadth: float = 0.9
    tank_height: float = 0.508
    roll_amplitude_deg: float = 4.0
    cells_per_breadth: int = 90
    n_cycles: float = 6.0
    moment_write_interval: int = 1
    study_name: str = "b1546_sloshing_sweep"

    def default_drive_periods(self) -> Tuple[float, float, float]:
        """Placeholder drive periods = [T1, 0.85*T1, 1.15*T1] for the mid fill.

        ``T1`` is the analytical first-mode sloshing period at the middle fill
        level; the resonant period plus a shorter and a longer off-resonance
        target bracket the response. Overridden by the orchestrator with the
        real vessel roll natural period + 2 targets.
        """
        mid_fill = self.fill_levels[len(self.fill_levels) // 2]
        ref = SloshingForcedRollConfig(
            breadth=self.breadth,
            tank_height=self.tank_height,
            fill_depth=mid_fill * self.tank_height,
            cells_per_breadth=self.cells_per_breadth,
        )
        t1 = ref.first_mode_period
        return (round(t1, 6), round(0.85 * t1, 6), round(1.15 * t1, 6))

    @property
    def periods(self) -> Tuple[float, ...]:
        """Resolved drive periods (explicit or placeholder defaults)."""
        if self.drive_periods is not None:
            return tuple(self.drive_periods)
        return self.default_drive_periods()

    def forced_roll_config(
        self, fill_level: float, drive_period: float
    ) -> SloshingForcedRollConfig:
        """Build the forced-roll config for one ``(fill, period)`` matrix point."""
        name = _case_name(self.study_name, fill_level, drive_period)
        return SloshingForcedRollConfig(
            breadth=self.breadth,
            tank_height=self.tank_height,
            fill_depth=fill_level * self.tank_height,
            roll_amplitude_deg=self.roll_amplitude_deg,
            roll_period=drive_period,
            cells_per_breadth=self.cells_per_breadth,
            n_cycles=self.n_cycles,
            name=name,
        )

    def parametric_study(self) -> ParametricStudy:
        """The ParametricStudy whose product enumerates the sweep matrix.

        Uses the shared OpenFOAM ``ParametricStudy`` (itertools.product over
        ``StudyParameter`` values) as the sweep engine, tagged as a ``SLOSHING``
        case type. ``generate_cases()`` yields one case per ``(fill, period)``
        combination with the values on ``case.metadata['study_parameters']``.
        """
        study = ParametricStudy(
            case_type=CaseType.SLOSHING, study_name=self.study_name
        )
        study.add_parameter(StudyParameter(FILL_PARAM, list(self.fill_levels)))
        study.add_parameter(StudyParameter(PERIOD_PARAM, list(self.periods)))
        return study

    def matrix(self) -> List[Tuple[float, float]]:
        """The ``(fill_level, drive_period)`` points, via the ParametricStudy."""
        study = self.parametric_study()
        points: List[Tuple[float, float]] = []
        for case in study.generate_cases():
            sp = case.metadata["study_parameters"]
            points.append((float(sp[FILL_PARAM]), float(sp[PERIOD_PARAM])))
        return points


def _case_name(prefix: str, fill_level: float, drive_period: float) -> str:
    """Deterministic case-dir name for a matrix point (filesystem-safe)."""
    fill_tag = f"{round(fill_level * 100)}"
    period_tag = f"{drive_period:.4g}".replace(".", "p")
    return f"{prefix}_fill{fill_tag}_T{period_tag}"


# ---------------------------------------------------------------------------
# Sweep harness: generate (build dirs) / launch command / collect (reduce)
# ---------------------------------------------------------------------------


@dataclass
class SloshingSweep:
    """Generate, launch, and collect the #641 fill / frequency sweep.

    Generation and execution are deliberately separate steps: :meth:`generate`
    only writes the case directories + a plan; the caller runs the (printed)
    launch command; :meth:`collect` reduces the solved cases into the contract
    manifest.
    """

    config: SloshingSweepConfig = field(default_factory=SloshingSweepConfig)

    # -- generation --------------------------------------------------------- #

    def case_configs(self) -> List[SloshingForcedRollConfig]:
        """The nine forced-roll configs, one per ``(fill, period)`` point."""
        return [
            self.config.forced_roll_config(fill, period)
            for fill, period in self.config.matrix()
        ]

    def generate(self, parent_dir: Path | str) -> List[Path]:
        """Build every case directory (with the roll-moment FO) and a plan file.

        Does NOT run any solver. Writes ``<parent>/sweep_plan.json`` describing
        the matrix and the launch command.
        """
        parent = Path(parent_dir)
        parent.mkdir(parents=True, exist_ok=True)
        case_dirs: List[Path] = []
        plan_points: List[Dict[str, Any]] = []
        for cfg in self.case_configs():
            case_dir = build_forced_roll_case(
                cfg,
                parent,
                with_moment=True,
                moment_write_interval=self.config.moment_write_interval,
            )
            case_dirs.append(case_dir)
            plan_points.append(
                {
                    "case": cfg.name,
                    "case_dir": str(case_dir),
                    "fill_level": cfg.fill_level,
                    "drive_period": cfg.drive_period,
                    "drive_freq_hz": 1.0 / cfg.drive_period,
                    "roll_amplitude_deg": cfg.roll_amplitude_deg,
                    "end_time_s": cfg.end_time,
                    "moment_fo": ROLL_MOMENT_FO_NAME,
                }
            )
        plan = {
            "study": self.config.study_name,
            "issue": "#641",
            "n_cases": len(case_dirs),
            "fill_levels": list(self.config.fill_levels),
            "drive_periods": list(self.config.periods),
            "contract_fields": list(CONTRACT_FIELDS),
            "launch_command": self.launch_command(parent),
            "points": plan_points,
        }
        (parent / "sweep_plan.json").write_text(json.dumps(plan, indent=2) + "\n")
        return case_dirs

    # -- execution (separate step; the harness prints, the orchestrator runs) - #

    def launch_command(self, parent_dir: Path | str) -> str:
        """The exact shell command to run all nine cases + collect the manifest.

        Runs each case (blockMesh -> setFields -> interFoam) via the module CLI
        and then reduces the moment histories into the run manifest. Printed by
        :meth:`generate`; the orchestrator fires it when the box / geometry
        allow rather than the harness running the full sweep itself.
        """
        parent = Path(parent_dir)
        return (
            "uv run python -m digitalmodel.solvers.openfoam.validation."
            f"sloshing_sweep run --parent {parent}"
        )

    # -- collection --------------------------------------------------------- #

    def collect(
        self, parent_dir: Path | str, *, write_manifest: bool = True
    ) -> List[Dict[str, float]]:
        """Reduce every solved case into contract rows; write the run manifest.

        Skips cases whose moment file is missing (not yet run) and records the
        reason. Writes ``sweep_manifest.json`` and ``sweep_manifest.csv``.
        """
        parent = Path(parent_dir)
        rows: List[Dict[str, float]] = []
        skipped: List[Dict[str, str]] = []
        for cfg in self.case_configs():
            case_dir = parent / cfg.name
            try:
                times, moment = parse_roll_moment(case_dir)
                row = reduce_roll_moment(
                    times,
                    moment,
                    cfg.drive_period,
                    fill_level=cfg.fill_level,
                    roll_amplitude_deg=cfg.roll_amplitude_deg,
                )
            except (FileNotFoundError, RuntimeError, ValueError) as exc:
                skipped.append({"case": cfg.name, "reason": str(exc)})
                continue
            rows.append(row)

        if write_manifest:
            _write_manifest(parent, rows, skipped, self.config)
        return rows


def _write_manifest(
    parent: Path,
    rows: List[Dict[str, float]],
    skipped: List[Dict[str, str]],
    config: SloshingSweepConfig,
) -> None:
    """Write the JSON + CSV run manifest of contract rows."""
    manifest = {
        "study": config.study_name,
        "issue": "#641",
        "contract_fields": list(CONTRACT_FIELDS),
        "n_points": len(rows),
        "n_skipped": len(skipped),
        "skipped": skipped,
        "points": rows,
    }
    (parent / "sweep_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n"
    )
    lines = [",".join(CONTRACT_FIELDS)]
    for row in rows:
        lines.append(",".join(f"{row[k]:.10g}" for k in CONTRACT_FIELDS))
    (parent / "sweep_manifest.csv").write_text("\n".join(lines) + "\n")


# ---------------------------------------------------------------------------
# CLI: generate | run | collect
# ---------------------------------------------------------------------------


def _run_all_cases(parent: Path, config: SloshingSweepConfig) -> None:
    """Run every generated case (blockMesh -> setFields -> interFoam) in place."""
    from ..runner import OpenFOAMRunConfig, OpenFOAMRunner

    runner = OpenFOAMRunner(
        OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)
    )
    sweep = SloshingSweep(config)
    for cfg in sweep.case_configs():
        case_dir = parent / cfg.name
        result = runner.run(case_dir)
        print(f"[{cfg.name}] {result.status.value}"
              + (f" ({result.error_message})" if result.error_message else ""))


def main(argv: Optional[Sequence[str]] = None) -> int:
    """CLI entry point: ``generate`` | ``run`` | ``collect``."""
    import argparse

    parser = argparse.ArgumentParser(
        prog="sloshing_sweep",
        description="ACMA B1546 fill / frequency sloshing sweep (#641).",
    )
    parser.add_argument(
        "command", choices=("generate", "run", "collect"),
        help="generate case dirs, run the solver on each, or collect the manifest",
    )
    parser.add_argument(
        "--parent", type=Path, required=True,
        help="parent directory for the sweep case tree",
    )
    args = parser.parse_args(argv)

    config = SloshingSweepConfig()
    sweep = SloshingSweep(config)

    if args.command == "generate":
        dirs = sweep.generate(args.parent)
        print(f"Generated {len(dirs)} cases under {args.parent}")
        print("Launch the full sweep with:")
        print("  " + sweep.launch_command(args.parent))
        return 0

    if args.command == "run":
        _run_all_cases(args.parent, config)
        rows = sweep.collect(args.parent)
        print(f"Collected {len(rows)} contract rows -> "
              f"{args.parent / 'sweep_manifest.json'}")
        return 0

    # collect
    rows = sweep.collect(args.parent)
    print(f"Collected {len(rows)} contract rows -> "
          f"{args.parent / 'sweep_manifest.json'}")
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI dispatch
    raise SystemExit(main())
