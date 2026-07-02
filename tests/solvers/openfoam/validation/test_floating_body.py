"""Validation tests for the floating-body heave free decay (#1169).

ALWAYS (no OpenFOAM): the hydrostatic reference helpers return known values
and the case builder produces a structurally valid dynamic-mesh VOF case.

CONDITIONALLY (solver-capable host only): the fast variant is solved via
OpenFOAMRunner (blockMesh -> topoSet -> subsetMesh -> setFields -> interFoam,
~3 min) and the equilibrium draft, decay envelope and heave period are
gated; otherwise skipped.
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
    ADDED_MASS_BAND,
    DRAFT_TOLERANCE,
    FLOATING_BODY_CASE,
    PERIOD_RATIO_BAND,
    FloatingBodyConfig,
    analyze_decay,
    build_floating_body_case,
    equilibrium_draft,
    extract_heave_history,
    heave_stiffness,
    hydrostatic_heave_period,
)
from digitalmodel.solvers.openfoam.validation.floating_body import RHO_WATER

from .conftest import assert_valid_case_dir, solver_capable


# --------------------------------------------------------------------------- #
#  Hydrostatic reference helpers — always run                                 #
# --------------------------------------------------------------------------- #


def test_equilibrium_draft_archimedes() -> None:
    # 15 kg on a 0.06 m^2 waterplane in water of 998.2 kg/m^3 -> 0.2505 m.
    assert equilibrium_draft(15.0, 0.06) == pytest.approx(0.25045, rel=1e-3)
    # Half-density cuboid floats half-submerged.
    cfg = FloatingBodyConfig()
    assert cfg.draft == pytest.approx(cfg.lz / 2, rel=5e-3)


def test_heave_stiffness_closed_tank_correction() -> None:
    k_open = heave_stiffness(0.06)
    assert k_open == pytest.approx(998.2 * 9.81 * 0.06, rel=1e-9)
    # closed 1 m^2 tank stiffens by 1/(1 - 0.06)
    k_closed = heave_stiffness(0.06, 1.0)
    assert k_closed == pytest.approx(k_open / 0.94, rel=1e-9)


def test_hydrostatic_period_known_answer() -> None:
    cfg = FloatingBodyConfig()
    # T = 2 pi sqrt(m/k_eff) ~= 0.973 s for the tutorial cuboid.
    assert cfg.hydrostatic_period == pytest.approx(0.973, rel=2e-3)
    # Added mass lengthens the period.
    t_ma = hydrostatic_heave_period(cfg.mass, cfg.stiffness,
                                    added_mass=0.4 * cfg.mass)
    assert t_ma == pytest.approx(cfg.hydrostatic_period * math.sqrt(1.4), rel=1e-9)


@pytest.mark.parametrize("bad", [0.0, -1.0])
def test_reference_helpers_reject_nonpositive(bad: float) -> None:
    with pytest.raises(ValueError):
        equilibrium_draft(bad, 0.06)
    with pytest.raises(ValueError):
        heave_stiffness(bad)
    with pytest.raises(ValueError):
        hydrostatic_heave_period(bad, 100.0)
    with pytest.raises(ValueError):
        heave_stiffness(0.06, 0.05)  # tank smaller than waterplane


def test_config_hydrostatics_consistent() -> None:
    cfg = FloatingBodyConfig()
    assert cfg.mass == pytest.approx(500 * 0.3 * 0.2 * 0.5)
    # equilibrium CoM sits essentially at the (settled) waterline for a
    # half-density body
    assert cfg.equilibrium_com_height == pytest.approx(0.533, abs=2e-3)
    # release offset ~0.05 m below equilibrium (linear-decay regime)
    z0_com = cfg.centre_of_mass[2]
    assert cfg.equilibrium_com_height - z0_com == pytest.approx(0.05, abs=0.01)


def test_validation_case_registered() -> None:
    assert FLOATING_BODY_CASE.metadata["issue"] == "#1169"
    assert FLOATING_BODY_CASE.tolerance == DRAFT_TOLERANCE == 0.05


# --------------------------------------------------------------------------- #
#  Decay analysis on a synthetic signal — always run                          #
# --------------------------------------------------------------------------- #


def test_analyze_decay_recovers_synthetic_oscillation() -> None:
    import numpy as np

    cfg = FloatingBodyConfig()
    t_n = 1.15
    z_eq = cfg.equilibrium_com_height
    t = np.linspace(0, 6, 2000)
    z = z_eq - 0.05 * np.exp(-0.3 * t) * np.cos(2 * math.pi * t / t_n)
    res = analyze_decay(t.tolist(), z.tolist(), cfg)
    assert res["period_measured"] == pytest.approx(t_n, rel=0.02)
    assert abs(res["draft_error"]) <= 0.02
    assert res["monotone_decay"]
    # 6 s / 1.15 s with the first upcrossing excluded by the release-transient
    # filter -> 3 full measured periods.
    assert res["n_cycles"] >= 3


# --------------------------------------------------------------------------- #
#  Case builder — always run (no OpenFOAM needed)                             #
# --------------------------------------------------------------------------- #


def test_build_produces_valid_dynamic_mesh_case(tmp_path) -> None:
    case_dir = build_floating_body_case(FloatingBodyConfig(), parent_dir=tmp_path)
    assert_valid_case_dir(case_dir)
    control = (case_dir / "system" / "controlDict").read_text()
    assert "interFoam" in control
    assert "sixDoFRigidBodyState" in control  # heave recorder
    dyn = (case_dir / "constant" / "dynamicMeshDict").read_text()
    assert "sixDoFRigidBodyMotion" in dyn
    assert "sixDoFRigidBodyMotionConstraint line" in dyn  # heave-only
    assert "mass                15" in dyn
    assert (case_dir / "system" / "topoSetDict").is_file()
    assert (case_dir / "0" / "pointDisplacement").is_file()


def test_build_stamps_provenance_with_citation(tmp_path) -> None:
    case_dir = build_floating_body_case(FloatingBodyConfig(), parent_dir=tmp_path)
    prov = json.loads((case_dir / "provenance.json").read_text())
    assert prov["issue"] == "#1169"
    assert any("floatingObject" in c for c in prov["citations"])
    assert prov["expected"]["draft_m"] == pytest.approx(0.2505, rel=1e-3)


# --------------------------------------------------------------------------- #
#  Solve assertion — gated to solver-capable hosts                            #
# --------------------------------------------------------------------------- #


def test_floating_body_solve_matches_hydrostatics(tmp_path) -> None:
    """End-to-end: mesh -> topoSet -> subsetMesh -> setFields -> solve the
    fast decay variant (~3 min), then gate the equilibrium draft, damped
    envelope and heave period. Gated to solver-capable hosts. The 14 s
    reference run behind the report gives draft +1.1%, 11 cycles,
    T/T_hyd = 1.18 (implied Ca = 0.39)."""
    if not solver_capable(tmp_path):
        pytest.skip("OpenFOAM not available — run gated to solver-capable hosts")

    cfg = FloatingBodyConfig()
    case_dir = build_floating_body_case(cfg, parent_dir=tmp_path)
    runner = OpenFOAMRunner(OpenFOAMRunConfig(
        run_topo_set=True,
        subset_mesh_set="c0",
        subset_mesh_patch="floatingObject",
        run_set_fields=True,
        to_vtk=False,
    ))
    result = runner.run(case_dir)
    assert result.status == OpenFOAMRunStatus.COMPLETED, result.error_message
    names = [s.name for s in result.stages]
    assert names == ["blockMesh", "topoSet", "subsetMesh", "setFields", "interFoam"]

    t, z = extract_heave_history(case_dir)
    res = analyze_decay(t, z, cfg)
    assert abs(res["draft_error"]) <= DRAFT_TOLERANCE, (
        f"equilibrium draft error {res['draft_error']:.1%} exceeds "
        f"{DRAFT_TOLERANCE:.0%} (Archimedes {cfg.draft:.4f} m)"
    )
    assert res["monotone_decay"], f"decay envelope not damped: {res['peaks']}"
    assert res["n_cycles"] >= 2, f"too few decay cycles ({res['n_cycles']})"
    lo, hi = PERIOD_RATIO_BAND
    assert lo <= res["period_ratio"] <= hi, (
        f"period ratio {res['period_ratio']:.2f} outside the physical "
        f"added-mass band [{lo}, {hi}] (T={res['period_measured']:.3f}s vs "
        f"hydrostatic {res['period_hydrostatic']:.3f}s)"
    )
    ca_lo, ca_hi = ADDED_MASS_BAND
    assert ca_lo <= res["implied_added_mass_coeff"] <= ca_hi, (
        f"implied added-mass coefficient {res['implied_added_mass_coeff']:.2f} "
        f"outside [{ca_lo}, {ca_hi}]"
    )


def test_rho_water_matches_template() -> None:
    # The hydrostatic references assume the template's water density.
    assert RHO_WATER == 998.2
