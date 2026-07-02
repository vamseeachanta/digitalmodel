"""Validation tests for the Martin & Moyce dam-break case (#1165).

ALWAYS (no OpenFOAM): the digitized reference data and pure helpers return
known values and the case builder produces a structurally valid interFoam
VOF case.

CONDITIONALLY (solver-capable host only): the case is solved via
OpenFOAMRunner (blockMesh -> setFields -> interFoam) and the surge front is
compared to Martin & Moyce (1952) with mass conservation asserted; otherwise
skipped.
"""

from __future__ import annotations

import json
import math

import pytest

from digitalmodel.solvers.openfoam.runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunStatus,
)
from digitalmodel.solvers.openfoam.validation import (
    DAM_BREAK_CASE,
    DAM_BREAK_FRONT_TOLERANCE,
    DAM_BREAK_MASS_TOLERANCE,
    DAM_BREAK_RAW_FRONT_TOLERANCE,
    DAM_BREAK_SOLVE_TOLERANCE,
    GATE_RELEASE_TIME_SHIFT,
    MARTIN_MOYCE_N2_FRONT,
    MARTIN_MOYCE_N2_TIME,
    DamBreakConfig,
    build_dam_break_case,
    dimensionless_front,
    dimensionless_time,
    front_deviation,
    martin_moyce_front,
)
from digitalmodel.solvers.openfoam.validation.dam_break import (
    GRAVITY,
    extract_front_history,
    extract_mass_drift,
)

from .conftest import assert_valid_case_dir, solver_capable


# --------------------------------------------------------------------------- #
#  Reference data + pure helpers — always run                                 #
# --------------------------------------------------------------------------- #


def test_digitized_data_is_consistent() -> None:
    assert len(MARTIN_MOYCE_N2_TIME) == len(MARTIN_MOYCE_N2_FRONT) == 13
    # Strictly increasing in both T and Z (a monotone surge).
    assert all(
        a < b for a, b in zip(MARTIN_MOYCE_N2_TIME, MARTIN_MOYCE_N2_TIME[1:])
    )
    assert all(
        a < b for a, b in zip(MARTIN_MOYCE_N2_FRONT, MARTIN_MOYCE_N2_FRONT[1:])
    )
    # The front starts at the column edge (Z -> 1) and reaches Z=3.67 by T=2.96.
    assert MARTIN_MOYCE_N2_FRONT[0] == pytest.approx(1.11)
    assert MARTIN_MOYCE_N2_FRONT[-1] == pytest.approx(3.67)


def test_dimensionless_time_known_answer() -> None:
    a = 0.1461
    # T = t * sqrt(2g/a); at t = 1 s, sqrt(2*9.81/0.1461) ~= 11.588.
    assert dimensionless_time(1.0, a) == pytest.approx(
        math.sqrt(2.0 * GRAVITY / a), rel=1e-9
    )
    assert dimensionless_time(1.0, a) == pytest.approx(11.588, rel=1e-3)
    assert dimensionless_time(0.0, a) == 0.0


def test_dimensionless_front_is_x_over_a() -> None:
    assert dimensionless_front(0.2922, 0.1461) == pytest.approx(2.0)


@pytest.mark.parametrize("bad", [0.0, -1.0])
def test_helpers_reject_nonpositive_width(bad: float) -> None:
    with pytest.raises(ValueError):
        dimensionless_time(1.0, bad)
    with pytest.raises(ValueError):
        dimensionless_front(1.0, bad)
    with pytest.raises(ValueError):
        dimensionless_time(-0.1, 0.1461)


def test_martin_moyce_front_interpolates() -> None:
    # Exact at digitized points.
    assert martin_moyce_front(1.97) == pytest.approx(2.33)
    # Between points: linear.
    mid = martin_moyce_front((2.2 + 2.32) / 2)
    assert 2.56 < mid < 2.78
    with pytest.raises(ValueError):
        martin_moyce_front(0.1)
    with pytest.raises(ValueError):
        martin_moyce_front(5.0)


def test_front_deviation_zero_for_exact_experimental_curve() -> None:
    # Feeding the experimental points back (already in experimental time,
    # so no shift) gives zero deviation.
    mean_dev, max_dev = front_deviation(
        MARTIN_MOYCE_N2_TIME, MARTIN_MOYCE_N2_FRONT, time_shift=0.0
    )
    assert mean_dev == pytest.approx(0.0, abs=1e-12)
    assert max_dev == pytest.approx(0.0, abs=1e-12)


def test_front_deviation_detects_offset_curve() -> None:
    # A curve 20% high everywhere deviates by 20%.
    z_high = [z * 1.2 for z in MARTIN_MOYCE_N2_FRONT]
    mean_dev, max_dev = front_deviation(
        MARTIN_MOYCE_N2_TIME, z_high, time_shift=0.0
    )
    assert mean_dev == pytest.approx(0.2, rel=1e-6)
    assert max_dev == pytest.approx(0.2, rel=1e-6)


def test_front_deviation_requires_coverage() -> None:
    with pytest.raises(ValueError):
        front_deviation([0.5, 1.0], [1.2, 1.4], time_shift=0.0)


def test_validation_case_registered_with_issue_gate() -> None:
    assert DAM_BREAK_CASE.tolerance == DAM_BREAK_FRONT_TOLERANCE == 0.10
    assert DAM_BREAK_CASE.within_tolerance("front_z_at_T1p97", 2.33)
    # An 11% deviation fails the 10% gate.
    assert not DAM_BREAK_CASE.within_tolerance("front_z_at_T1p97", 2.33 * 1.11)


# --------------------------------------------------------------------------- #
#  Config consistency — always run                                            #
# --------------------------------------------------------------------------- #


def test_config_geometry_is_martin_moyce_n2() -> None:
    cfg = DamBreakConfig()
    assert cfg.column_height == pytest.approx(2 * cfg.column_width)  # n^2 = 2
    assert cfg.tank_length == pytest.approx(8 * cfg.column_width)
    assert cfg.tank_height == pytest.approx(4 * cfg.column_width)
    # Column edges land exactly on cell faces.
    assert cfg.column_width / cfg.cell_size == pytest.approx(cfg.cells_per_width)
    assert (cfg.nx, cfg.ny) == (8 * cfg.cells_per_width, 4 * cfg.cells_per_width)
    # Default end time covers the digitized range (T = 2.96 -> ~0.256 s).
    assert cfg.end_time >= cfg.time_at(MARTIN_MOYCE_N2_TIME[-1])


# --------------------------------------------------------------------------- #
#  Case builder — always run (no OpenFOAM needed)                             #
# --------------------------------------------------------------------------- #


def test_build_produces_valid_vof_case(tmp_path) -> None:
    case_dir = build_dam_break_case(DamBreakConfig(), parent_dir=tmp_path)
    assert_valid_case_dir(case_dir)
    control = (case_dir / "system" / "controlDict").read_text()
    assert "interFoam" in control
    assert "waterVolume" in control  # mass-conservation functionObject
    # VOF specifics: setFields region + phase fields + gravity.
    setfields = (case_dir / "system" / "setFieldsDict").read_text()
    assert "alpha.water" in setfields
    assert "0.1461" in setfields
    assert (case_dir / "0" / "alpha.water").is_file()
    assert (case_dir / "0" / "p_rgh").is_file()
    assert (case_dir / "constant" / "g").is_file()
    turb = (case_dir / "constant" / "turbulenceProperties").read_text()
    assert "laminar" in turb


def test_build_mesh_matches_config(tmp_path) -> None:
    cfg = DamBreakConfig(cells_per_width=36)
    case_dir = build_dam_break_case(cfg, parent_dir=tmp_path)
    blockmesh = (case_dir / "system" / "blockMeshDict").read_text()
    assert "(288 144 1)" in blockmesh


def test_build_stamps_provenance_with_citation(tmp_path) -> None:
    case_dir = build_dam_break_case(DamBreakConfig(), parent_dir=tmp_path)
    prov = json.loads((case_dir / "provenance.json").read_text())
    assert prov["issue"] == "#1165"
    assert any("10.1098/rsta.1952.0006" in c for c in prov["citations"])
    assert any("damBreak" in c for c in prov["citations"])
    assert prov["gate_release_time_shift"] == GATE_RELEASE_TIME_SHIFT


# --------------------------------------------------------------------------- #
#  Solve assertion — gated to solver-capable hosts                            #
# --------------------------------------------------------------------------- #


def test_dam_break_solve_matches_martin_moyce(tmp_path) -> None:
    """End-to-end: mesh + setFields + solve, then compare the surge front to
    Martin & Moyce (1952) and assert mass conservation. Gated to
    solver-capable hosts (skipped in dry-run CI). The case is the one
    validated in the report docs/api/cfd/dam-break-verification.html
    (mean front deviation ~3.0% with the gate-release shift; mass drift
    ~1e-6)."""
    if not solver_capable(tmp_path):
        pytest.skip("OpenFOAM not available — run gated to solver-capable hosts")
    pytest.importorskip("pyvista")

    cfg = DamBreakConfig()
    case_dir = build_dam_break_case(cfg, parent_dir=tmp_path)
    runner = OpenFOAMRunner(OpenFOAMRunConfig(run_set_fields=True, to_vtk=False))
    result = runner.run(case_dir)
    assert result.status == OpenFOAMRunStatus.COMPLETED, result.error_message
    stage_names = [s.name for s in result.stages]
    assert stage_names == ["blockMesh", "setFields", "interFoam"]

    # Mass conservation: total water volume drift < 1%.
    drift = extract_mass_drift(case_dir)
    assert drift <= DAM_BREAK_MASS_TOLERANCE, (
        f"water volume drift {drift:.2%} exceeds {DAM_BREAK_MASS_TOLERANCE:.0%}"
    )

    # Surge front vs the digitized experiment, with the standard gate-release
    # shift (the experiment's diaphragm removal is not instantaneous).
    t_cfd, z_cfd = extract_front_history(case_dir, cfg)
    mean_dev, max_dev = front_deviation(t_cfd, z_cfd)
    assert mean_dev <= DAM_BREAK_SOLVE_TOLERANCE, (
        f"mean front deviation {mean_dev:.1%} exceeds "
        f"{DAM_BREAK_SOLVE_TOLERANCE:.0%} (expected ~3.0% for the verified case)"
    )
    assert max_dev <= DAM_BREAK_FRONT_TOLERANCE, (
        f"max front deviation {max_dev:.1%} exceeds the issue gate "
        f"{DAM_BREAK_FRONT_TOLERANCE:.0%}"
    )

    # Sanity: even without the shift the mean deviation stays bounded.
    raw_mean, _raw_max = front_deviation(t_cfd, z_cfd, time_shift=0.0)
    assert raw_mean <= DAM_BREAK_RAW_FRONT_TOLERANCE, (
        f"raw mean front deviation {raw_mean:.1%} exceeds "
        f"{DAM_BREAK_RAW_FRONT_TOLERANCE:.0%}"
    )
