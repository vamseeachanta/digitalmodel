"""Validation tests for the vertical-cylinder wave-loading case (#1171).

ALWAYS (no OpenFOAM): the frozen MacCamy-Fuchs closed form reproduces its
headline targets, converges onto the Morison inertia limit (Cm -> 2) as ka -> 0,
gives the diffraction reduction at the headline ka, and the case config keeps
the verified wave geometry with the waterline on a cell face.

CONDITIONALLY (solver-capable host AND DIGITALMODEL_RUN_LONG_CFD=1): the case is
solved and the peak inline force is asserted within tolerance of MacCamy-Fuchs
at the measured incident wave height, with the correct phase lead.
"""
from __future__ import annotations

import math
import os

import pytest

from digitalmodel.solvers.openfoam.validation.maccamy_fuchs import (
    DIFFRACTION_KA,
    MACCAMY_FUCHS_FORCE_TOLERANCE,
    MORISON_CM,
    CylinderWaveLoadingConfig,
    maccamy_fuchs_A,
    maccamy_fuchs_force,
    morison_inertia_force,
    provenance,
)


# --------------------------------------------------------------------------- #
#  Frozen MacCamy-Fuchs reference — always run                                #
# --------------------------------------------------------------------------- #


def test_headline_force_and_phase() -> None:
    r = maccamy_fuchs_force(0.03, 0.80, 0.40, 0.30)
    assert r["ka"] == pytest.approx(0.955, abs=0.005)
    assert r["F_amplitude"] == pytest.approx(14.74, abs=0.1)     # N
    assert r["phase_lead_velocity_deg"] == pytest.approx(69.58, abs=0.5)
    assert r["Cm_eff"] == pytest.approx(1.435, abs=0.01)


def test_converges_to_morison_inertia_limit() -> None:
    # as ka -> 0 the diffraction force collapses onto Morison inertia (Cm -> 2)
    # and the force leads the velocity by ~90 deg (pure inertia)
    for D in (0.02, 0.05, 0.10):                 # shrink the cylinder -> small ka
        mf = maccamy_fuchs_force(0.03, 0.80, 0.40, D)
        mor = morison_inertia_force(0.03, 0.80, 0.40, D)
        assert mf["ka"] < DIFFRACTION_KA
        assert mf["Cm_eff"] == pytest.approx(2.0, abs=0.07)
        assert mf["F_amplitude"] == pytest.approx(mor, rel=0.04)
        assert mf["phase_lead_velocity_deg"] == pytest.approx(90.0, abs=5.0)


def test_diffraction_reduces_force_below_morison_at_headline() -> None:
    # the whole point of the headline case: diffraction pulls the force well
    # below the Morison inertia estimate, so a Morison prediction fails the gate
    mf = maccamy_fuchs_force(0.03, 0.80, 0.40, 0.30)
    mor = morison_inertia_force(0.03, 0.80, 0.40, 0.30)
    assert mf["ka"] > DIFFRACTION_KA
    ratio = mf["F_amplitude"] / mor
    assert ratio == pytest.approx(0.717, abs=0.01)
    # the reduction (28%) is comfortably outside the 15% force gate
    assert (1.0 - ratio) > MACCAMY_FUCHS_FORCE_TOLERANCE


def test_A_factor_positive_and_force_linear_in_height() -> None:
    assert maccamy_fuchs_A(0.955) > 1.0
    f1 = maccamy_fuchs_force(0.02, 0.80, 0.40, 0.30)["F_amplitude"]
    f2 = maccamy_fuchs_force(0.04, 0.80, 0.40, 0.30)["F_amplitude"]
    assert f2 == pytest.approx(2.0 * f1, rel=1e-9)      # exactly linear in H


def test_morison_cm_is_two() -> None:
    assert MORISON_CM == 2.0


# --------------------------------------------------------------------------- #
#  Case configuration — always run                                            #
# --------------------------------------------------------------------------- #


def test_config_geometry_and_face_alignment() -> None:
    cfg = CylinderWaveLoadingConfig()
    # waterline on a background cell face (VOF lesson #1165/#1302)
    dz = cfg.tank_height / cfg.nz
    assert cfg.depth / dz == pytest.approx(round(cfg.depth / dz), abs=1e-9)
    # cylinder in the established region, clear of the inlet ramp zone (>~1 L)
    assert cfg.cylinder_x > 2.0 * cfg.wavelength
    # far slip wall well clear of the cylinder axis (low blockage)
    assert cfg.y_half_width > 4.0 * cfg.diameter
    # headline regime is diffraction, not pure Morison
    assert cfg.ka > DIFFRACTION_KA
    # background wave resolution >= ~20 cells/wavelength
    dx = cfg.tank_length / cfg.nx
    assert cfg.wavelength / dx >= 20.0


def test_config_reference_matches_closed_form() -> None:
    cfg = CylinderWaveLoadingConfig()
    r = cfg.reference
    assert r["F_amplitude"] == pytest.approx(14.74, abs=0.1)
    assert r["mf_over_morison"] == pytest.approx(0.717, abs=0.01)


def test_provenance_has_citation_and_targets() -> None:
    p = provenance()
    assert "MacCamy" in p["reference"] and "Fuchs" in p["reference"]
    assert p["issue"] == "#1171"
    assert p["targets"]["F_maccamy_fuchs_N"] == pytest.approx(14.74, abs=0.1)
    assert p["regime"]["diffraction"] is True


# --------------------------------------------------------------------------- #
#  End-to-end solve — opt-in                                                   #
# --------------------------------------------------------------------------- #


def test_cylinder_loading_solves_within_gate(tmp_path) -> None:
    from .conftest import solver_capable
    if not solver_capable(tmp_path):
        pytest.skip("no OpenFOAM on PATH")
    if os.environ.get("DIGITALMODEL_RUN_LONG_CFD") != "1":
        pytest.skip("long CFD solve — set DIGITALMODEL_RUN_LONG_CFD=1 to run")

    from digitalmodel.solvers.openfoam.validation.maccamy_fuchs import (
        analyze_cylinder_loading,
        build_maccamy_fuchs_case,
    )

    cfg = CylinderWaveLoadingConfig()
    root = build_maccamy_fuchs_case(cfg, tmp_path)
    # (mesh + solve driven on the solver host; see cases/maccamy_fuchs/)
    result = analyze_cylinder_loading(root, cfg)
    assert result["within_force_gate"], (
        f"peak inline force {result['force_amplitude']:.2f} N vs MacCamy-Fuchs "
        f"{result['force_reference']:.2f} N (at measured H) outside "
        f"{MACCAMY_FUCHS_FORCE_TOLERANCE:.0%}"
    )
    assert abs(result["phase_lead_error_deg"]) <= 15.0
