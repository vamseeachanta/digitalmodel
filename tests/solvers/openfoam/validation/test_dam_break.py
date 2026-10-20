"""Validation tests for the Martin & Moyce dam-break case (#1165).

ALWAYS (no OpenFOAM): the digitized reference data and pure helpers return
known values and the case builder produces a structurally valid interFoam
VOF case.

CONDITIONALLY (solver-capable host only): the case is solved via
OpenFOAMRunner (blockMesh -> setFields -> interFoam) and the surge front is
compared to Martin & Moyce (1952) with mass conservation asserted and interface
mixing width recorded as a diagnostic; otherwise skipped.
"""

from __future__ import annotations

import json
import math
import sys
from types import SimpleNamespace

import numpy as np
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
    DAM_BREAK_INTERFACE_TOLERANCE_CELLS,
    GRAVITY,
    extract_front_history,
    extract_interface_width,
    extract_mass_drift,
    interface_band_width,
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


@pytest.mark.parametrize("transpose", [False, True])
def test_interface_sharpness_rejects_mass_conserving_smear(transpose) -> None:
    """Equal water volume does not make an eight-cell interface acceptable."""
    dx = 0.01
    sharp = np.zeros((16, 32))
    sharp[:8] = 1
    smeared = np.repeat(
        (1 - np.clip((np.arange(16) + 0.5 - 4) / 8, 0, 1))[:, None],
        32, axis=1,
    )
    if transpose:
        sharp, smeared = sharp.T, smeared.T
    assert smeared.sum() == pytest.approx(sharp.sum())
    sharp_width = interface_band_width(sharp, dx) / dx
    assert sharp_width <= DAM_BREAK_INTERFACE_TOLERANCE_CELLS
    width = interface_band_width(smeared, dx) / dx
    # Midpoint-sampled linear ramp: 4 * sum(alpha * (1 - alpha)) / length.
    assert width == pytest.approx(5.375)
    assert width > DAM_BREAK_INTERFACE_TOLERANCE_CELLS


@pytest.mark.parametrize("dx", [0.001, 0.01, 0.1])
def test_resolved_interface_width_scales_with_mesh(dx) -> None:
    profile = [1, 1, 1, 0.75, 0.25, 0, 0, 0]
    alpha = np.repeat(np.array(profile)[:, None], 8, axis=1)
    assert interface_band_width(alpha, dx) == pytest.approx(1.5 * dx)
    width_cells = interface_band_width(alpha, dx) / dx
    assert width_cells <= DAM_BREAK_INTERFACE_TOLERANCE_CELLS


@pytest.mark.parametrize("angle", [0, 15, 30, 45])
@pytest.mark.parametrize("profile", ["tanh", "linear"])
def test_interface_mixing_width_for_rotated_profiles(angle, profile) -> None:
    """Rotation preserves a tanh width; a 3-cell ramp has sampling error."""
    dx = 0.01
    x, y = np.meshgrid(np.arange(256) + 0.5 - 128, np.arange(256) + 0.5 - 128)
    radians = math.radians(angle)
    normal_distance = x * math.sin(radians) + y * math.cos(radians)
    if profile == "tanh":
        # Half-width one cell: the continuous weighted width is 2 cells.
        alpha = 0.5 * (1 - np.tanh(normal_distance))
        relative_tolerance = 0.02
    else:
        # Continuous width = 2/3 of the 3-cell linear ramp. Cell sampling
        # causes up to 12% error, largest for this axis-aligned phase.
        alpha = 1 - np.clip(normal_distance / 3 + 0.5, 0, 1)
        relative_tolerance = 0.12
    width_cells = interface_band_width(alpha, dx) / dx
    assert width_cells == pytest.approx(2.0, rel=relative_tolerance)
    assert width_cells < DAM_BREAK_INTERFACE_TOLERANCE_CELLS


@pytest.mark.parametrize("alpha", [
    [], [0, 1], [[0, 1]], [[0, float("nan")], [1, 0]],
    [[0, float("inf")], [1, 0]], [[0, -0.1], [1, 0]],
    [[0, 1.1], [1, 0]], [[0, 0], [0, 0]], [[0.5, 0.5], [0.5, 0.5]],
])
def test_interface_width_rejects_invalid_or_unresolved_fields(alpha) -> None:
    with pytest.raises(ValueError):
        interface_band_width(alpha, 0.01)


@pytest.mark.parametrize("dx", [0, -1, float("nan"), float("inf")])
def test_interface_width_rejects_invalid_cell_size(dx) -> None:
    with pytest.raises(ValueError, match="cell_size"):
        interface_band_width([[0, 0], [1, 1]], dx)


def test_extract_interface_width_checks_all_written_fields(tmp_path, monkeypatch) -> None:
    """The diagnostic captures a smeared middle frame despite a sharp final one."""
    cfg = DamBreakConfig(cells_per_width=4)
    x, y = np.meshgrid(
        (np.arange(cfg.nx) + 0.5) * cfg.cell_size,
        (np.arange(cfg.ny) + 0.5) * cfg.cell_size,
    )
    points = np.column_stack((x.ravel(), y.ravel(), np.zeros(x.size)))
    # Exercise reconstruction from arbitrary field ordering.
    order = np.random.default_rng(42).permutation(x.size)
    sharp = np.where(y < 8 * cfg.cell_size, 1.0, 0.0)
    smeared = 1 - np.clip((y / cfg.cell_size - 4) / 8, 0, 1)

    class SavedFieldReader:
        time_values = [0.0, 0.1, 0.2]

        def set_active_time_value(self, t):
            self.t = t

        def read(self):
            field = smeared if self.t == 0.1 else sharp
            mesh = SimpleNamespace(
                cell_data={"alpha.water": field.ravel()[order]},
                cell_centers=lambda: SimpleNamespace(points=points[order]),
            )
            return {"internalMesh": mesh}

    monkeypatch.setitem(sys.modules, "pyvista", SimpleNamespace(
        OpenFOAMReader=lambda path: SavedFieldReader(),
    ))
    dat = tmp_path / "postProcessing/waterVolume/0/volFieldValue.dat"
    dat.parent.mkdir(parents=True)
    dat.write_text("# Time volume\n0 1\n0.1 1\n0.2 1\n")
    assert extract_mass_drift(tmp_path) == 0
    width = extract_interface_width(tmp_path, cfg)
    assert width == pytest.approx(5.375)
    assert width > DAM_BREAK_INTERFACE_TOLERANCE_CELLS


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
    assert "interface_band_width_cells" not in prov["tolerances"]
    assert prov["diagnostics"]["interface_band_width_cells"]["gate_qualified"] is False
    assert prov["diagnostics"]["interface_band_width_cells"]["definition"] == (
        "4 * integral(alpha * (1 - alpha)) / integral(abs(grad(alpha))) / cell_size"
    )


# --------------------------------------------------------------------------- #
#  Solve assertion — gated to solver-capable hosts                            #
# --------------------------------------------------------------------------- #


def test_dam_break_solve_matches_martin_moyce(tmp_path, record_property) -> None:
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

    width = extract_interface_width(case_dir, cfg)
    # No solved-field cutoff has been calibrated for the curved front.
    record_property("interface_band_width_cells", width)

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
