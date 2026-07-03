"""Validation tests for the 2D rectangular-tank sloshing case (#639).

These are ALWAYS-run assembly + analytical tests (no OpenFOAM): the analytical
first-mode frequency matches the tanh dispersion relation, the case builders
produce structurally valid interFoam VOF cases with the mesh/fill/motion wired
correctly, and the FFT natural-frequency estimator recovers a synthetic tone.

The real solver runs (measured vs analytical frequency, SPHERIC forced-roll
run-up) are invoked manually via the module's build/analyze functions on a
solver-capable host — see the #639 report — not from this test.
"""

from __future__ import annotations

import json
import math

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.motion import MotionType
from digitalmodel.solvers.openfoam.spectral_analysis import (
    prismatic_tank_natural_frequency,
)
from digitalmodel.solvers.openfoam.validation import (
    SLOSHING_2D_CASE,
    SLOSHING_FREQ_TOLERANCE,
    SloshingForcedRollConfig,
    SloshingFreeDecayConfig,
    build_forced_roll_case,
    build_free_decay_case,
    cosine_mode_setfields_body,
    measure_natural_frequency,
)

from .conftest import assert_valid_case_dir


# --------------------------------------------------------------------------- #
#  Analytical reference — always run                                          #
# --------------------------------------------------------------------------- #


def test_default_analytical_frequency_matches_tanh() -> None:
    cfg = SloshingFreeDecayConfig()
    expected = prismatic_tank_natural_frequency(cfg.breadth, cfg.fill_depth)
    assert cfg.analytical_frequency() == pytest.approx(expected)
    # L=1.0, h=0.3 -> ~0.758 Hz.
    assert cfg.analytical_frequency() == pytest.approx(0.758, abs=0.01)


def test_spheric_analytical_period_matches_published_T1() -> None:
    # SPHERIC Test 10, 18% fill: published first-mode period T1 = 1.9191 s.
    cfg = SloshingForcedRollConfig()
    assert cfg.first_mode_period == pytest.approx(1.9191, abs=0.01)


def test_registry_case_reference() -> None:
    assert SLOSHING_2D_CASE.tolerance == SLOSHING_FREQ_TOLERANCE
    assert "natural_frequency" in SLOSHING_2D_CASE.reference
    assert SLOSHING_2D_CASE.metadata["issue"] == "#639"


# --------------------------------------------------------------------------- #
#  Fill snapping / mesh sizing                                                 #
# --------------------------------------------------------------------------- #


def test_free_decay_fill_snapped_onto_cell_face() -> None:
    cfg = SloshingFreeDecayConfig()  # H=0.6, ny=60 -> dy=0.01; 0.5 -> h=0.3
    snap = cfg.fill_snap
    assert snap.fill_height == pytest.approx(0.3)
    assert cfg.fill_depth == pytest.approx(0.3)
    # h must be an integer number of cells (free surface on a face).
    ratio = snap.fill_height / snap.cell_height
    assert ratio == pytest.approx(round(ratio))
    assert snap.adjusted is False


def test_spheric_fill_is_18_percent() -> None:
    cfg = SloshingForcedRollConfig()
    assert cfg.fill_level == pytest.approx(0.093 / 0.508, abs=1e-6)


def test_mesh_is_square_cells() -> None:
    cfg = SloshingFreeDecayConfig()
    dx = cfg.breadth / cfg.nx
    dy = cfg.tank_height / cfg.ny
    assert dx == pytest.approx(dy, rel=0.05)


# --------------------------------------------------------------------------- #
#  Free-decay case assembly                                                    #
# --------------------------------------------------------------------------- #


def test_build_free_decay_case_structure(tmp_path) -> None:
    case_dir = build_free_decay_case(SloshingFreeDecayConfig(), tmp_path)
    assert_valid_case_dir(case_dir)
    assert (case_dir / "system" / "setFieldsDict").is_file()
    # static tank: NO dynamicMeshDict
    assert not (case_dir / "constant" / "dynamicMeshDict").exists()


def test_free_decay_uses_slip_walls_and_gravity_y(tmp_path) -> None:
    case_dir = build_free_decay_case(SloshingFreeDecayConfig(), tmp_path)
    u = (case_dir / "0" / "U").read_text()
    assert "slip" in u
    assert "movingWallVelocity" not in u
    g = (case_dir / "constant" / "g").read_text()
    assert "(0 -9.81 0)" in g


def test_free_decay_blockmesh_cell_counts(tmp_path) -> None:
    cfg = SloshingFreeDecayConfig()
    case_dir = build_free_decay_case(cfg, tmp_path)
    bm = (case_dir / "system" / "blockMeshDict").read_text()
    assert f"({cfg.nx} {cfg.ny} 1)" in bm


def test_free_decay_interface_probe_at_wall(tmp_path) -> None:
    cfg = SloshingFreeDecayConfig()
    case_dir = build_free_decay_case(cfg, tmp_path)
    cd = (case_dir / "system" / "controlDict").read_text()
    assert "interfaceHeight" in cd
    assert f"{cfg.probe_x:.6g}" in cd


def test_cosine_perturbation_is_volume_neutral() -> None:
    # The cosine mode integrates to ~zero perturbation volume over the breadth.
    cfg = SloshingFreeDecayConfig(cells_per_breadth=100)
    body = cosine_mode_setfields_body(cfg)
    # one boxToCell per column
    assert body.count("boxToCell") == cfg.nx
    # extract each box top (eta) and check mean ~ fill_depth
    etas = []
    for line in body.splitlines():
        line = line.strip()
        if line.startswith("box ("):
            # box (x0 y0 z0) (x1 eta z1);
            second = line.split(") (")[1]
            eta = float(second.split()[1])
            etas.append(eta)
    assert len(etas) == cfg.nx
    assert sum(etas) / len(etas) == pytest.approx(cfg.fill_depth, abs=1e-3)
    # the perturbation is antisymmetric: left wall high, right wall low
    assert etas[0] > cfg.fill_depth
    assert etas[-1] < cfg.fill_depth


def test_free_decay_provenance(tmp_path) -> None:
    case_dir = build_free_decay_case(SloshingFreeDecayConfig(), tmp_path)
    prov = json.loads((case_dir / "provenance.json").read_text())
    assert prov["issue"] == "#639"
    assert prov["validation_case"] == "sloshing_2d_free_decay"
    assert prov["analytical_frequency_hz"] == pytest.approx(0.758, abs=0.01)


# --------------------------------------------------------------------------- #
#  Forced-roll (SPHERIC) case assembly                                         #
# --------------------------------------------------------------------------- #


def test_build_forced_roll_case_structure(tmp_path) -> None:
    case_dir = build_forced_roll_case(SloshingForcedRollConfig(), tmp_path)
    assert_valid_case_dir(case_dir)
    # moving mesh: dynamicMeshDict present, reusing the #658 engine.
    dmd = (case_dir / "constant" / "dynamicMeshDict").read_text()
    assert "dynamicMotionSolverFvMesh" in dmd
    assert "motionSolver     solidBody;" in dmd
    assert "oscillatingRotatingMotion" in dmd


def test_forced_roll_uses_yaw_about_z_at_floor_centre(tmp_path) -> None:
    cfg = SloshingForcedRollConfig()
    case_dir = build_forced_roll_case(cfg, tmp_path)
    dmd = (case_dir / "constant" / "dynamicMeshDict").read_text()
    # YAW => Euler angle on z (0 0 A), amplitude in degrees.
    assert f"amplitude   (0 0 {cfg.roll_amplitude_deg:.10g})" in dmd
    # rotation axis at centre of floor: origin (L/2, 0, 0)
    assert f"origin      ({0.5 * cfg.breadth:.10g} 0 0)" in dmd
    assert cfg.motion().motion_type is MotionType.YAW


def test_forced_roll_moving_walls(tmp_path) -> None:
    case_dir = build_forced_roll_case(SloshingForcedRollConfig(), tmp_path)
    u = (case_dir / "0" / "U").read_text()
    assert "movingWallVelocity" in u


def test_forced_roll_partial_fill_box(tmp_path) -> None:
    cfg = SloshingForcedRollConfig()
    case_dir = build_forced_roll_case(cfg, tmp_path)
    sf = (case_dir / "system" / "setFieldsDict").read_text()
    assert "boxToCell" in sf
    # fill box top on y equals the snapped fill depth (~0.093)
    assert "alpha.water 1" in sf


def test_forced_roll_drive_period_default(tmp_path) -> None:
    cfg = SloshingForcedRollConfig()
    # default drive = 0.85 * T1
    assert cfg.drive_period == pytest.approx(0.85 * cfg.first_mode_period)


# --------------------------------------------------------------------------- #
#  FFT estimator recovers a synthetic tone                                     #
# --------------------------------------------------------------------------- #


def test_measure_natural_frequency_on_synthetic_tone() -> None:
    f0 = 0.758
    t = np.arange(0.0, 16.0, 0.02)
    y = 0.3 + 0.02 * np.cos(2.0 * math.pi * f0 * t)
    out = measure_natural_frequency(list(t), list(y))
    # parabolic refinement should land within 2% of the injected tone.
    assert out["refined_frequency"] == pytest.approx(f0, rel=0.02)
