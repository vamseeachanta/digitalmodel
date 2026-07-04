"""Validation tests for the laminar flat-plate (Blasius) case (#1167).

ALWAYS (no OpenFOAM): the analytical Blasius references return known values and
the case builder produces a structurally valid case directory.

CONDITIONALLY (solver-capable host only): the case is solved via OpenFOAMRunner
and the extracted Cf/Cd are compared to Blasius within 5%; otherwise skipped.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.solvers.openfoam.runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunStatus,
)
from digitalmodel.solvers.openfoam.validation import (
    BLASIUS_TOLERANCE,
    FLAT_PLATE_CASE,
    FlatPlateConfig,
    blasius_cf,
    blasius_delta99,
    blasius_plate_cd,
    build_flat_plate_case,
)
from digitalmodel.solvers.openfoam.validation.flat_plate import (
    _CF_COEFF,
    _DELTA99_COEFF,
    _PLATE_CD_COEFF,
    BLASIUS_SOLVE_TOLERANCE,
    extract_plate_mean_cf_error,
)

from .conftest import assert_valid_case_dir, solver_capable


# --------------------------------------------------------------------------- #
#  Analytical reference functions — always run                                #
# --------------------------------------------------------------------------- #


def test_blasius_cf_known_answer() -> None:
    re_x = 1.0e5
    assert blasius_cf(re_x) == pytest.approx(_CF_COEFF / math.sqrt(re_x))
    # Spot value: 0.664 / sqrt(1e5) ~= 2.0997e-3.
    assert blasius_cf(re_x) == pytest.approx(2.0997e-3, rel=1e-3)


def test_blasius_cf_scales_inverse_sqrt() -> None:
    # Cf(4*Re) = Cf(Re) / 2.
    assert blasius_cf(4.0e5) == pytest.approx(blasius_cf(1.0e5) / 2.0)


def test_blasius_delta99_known_answer() -> None:
    x, re_x = 1.0, 1.0e5
    assert blasius_delta99(x, re_x) == pytest.approx(
        _DELTA99_COEFF * x / math.sqrt(re_x)
    )
    # 5.0 * 1.0 / sqrt(1e5) ~= 0.015811 m.
    assert blasius_delta99(x, re_x) == pytest.approx(0.0158113, rel=1e-4)


def test_blasius_plate_cd_known_answer() -> None:
    re_l = 1.0e5
    assert blasius_plate_cd(re_l) == pytest.approx(_PLATE_CD_COEFF / math.sqrt(re_l))


@pytest.mark.parametrize("bad", [0.0, -1.0])
def test_reference_functions_reject_nonpositive(bad: float) -> None:
    with pytest.raises(ValueError):
        blasius_cf(bad)
    with pytest.raises(ValueError):
        blasius_delta99(1.0, bad)
    with pytest.raises(ValueError):
        blasius_plate_cd(bad)


def test_validation_case_reference_and_tolerance() -> None:
    assert FLAT_PLATE_CASE.tolerance == BLASIUS_TOLERANCE
    # An exact reference value is trivially within tolerance.
    assert FLAT_PLATE_CASE.within_tolerance(
        "cf_at_re_1e5", FLAT_PLATE_CASE.reference["cf_at_re_1e5"]
    )
    # A 6% deviation fails the 5% gate.
    measured = FLAT_PLATE_CASE.reference["cf_at_re_1e5"] * 1.06
    assert not FLAT_PLATE_CASE.within_tolerance("cf_at_re_1e5", measured)


# --------------------------------------------------------------------------- #
#  Config consistency — always run                                            #
# --------------------------------------------------------------------------- #


def test_config_velocity_is_self_consistent() -> None:
    cfg = FlatPlateConfig(re_l=1.0e5, plate_length=1.0, nu=1.0e-6)
    # U = Re_L * nu / L.
    assert cfg.free_stream_velocity == pytest.approx(0.1)
    # Re_x at the trailing edge recovers re_l.
    assert cfg.reynolds_at(cfg.plate_length) == pytest.approx(cfg.re_l)


# --------------------------------------------------------------------------- #
#  Case builder — always run (no OpenFOAM needed)                             #
# --------------------------------------------------------------------------- #


def test_build_produces_valid_case_dir(tmp_path) -> None:
    case_dir = build_flat_plate_case(FlatPlateConfig(), parent_dir=tmp_path)
    assert_valid_case_dir(case_dir)
    # simpleFoam (single-phase, steady) + laminar closure.
    control = (case_dir / "system" / "controlDict").read_text()
    assert "simpleFoam" in control
    turb = (case_dir / "constant" / "turbulenceProperties").read_text()
    assert "laminar" in turb


# --------------------------------------------------------------------------- #
#  Solve assertion — gated to solver-capable hosts                            #
# --------------------------------------------------------------------------- #


def test_flat_plate_solve_matches_blasius(tmp_path) -> None:
    """End-to-end: mesh + solve the verified case, then check the computed skin
    friction against Blasius. Gated to solver-capable hosts (skipped in dry-run
    CI). The case is the one validated in the report
    docs/api/cfd/flat-plate-blasius-verification.html (mean Cf error 4.6%)."""
    if not solver_capable(tmp_path):
        pytest.skip("OpenFOAM not available — run gated to solver-capable hosts")
    pytest.importorskip("pyvista")

    cfg = FlatPlateConfig()
    case_dir = build_flat_plate_case(cfg, parent_dir=tmp_path)
    runner = OpenFOAMRunner(OpenFOAMRunConfig(to_vtk=False))
    result = runner.run(case_dir)
    assert result.status == OpenFOAMRunStatus.COMPLETED, result.error_message

    # Local skin friction Cf(Re_x) over the developed region vs 0.664/sqrt(Re_x).
    mean_err, n = extract_plate_mean_cf_error(case_dir, cfg)
    assert n >= 50, f"too few plate Cf samples ({n})"
    assert mean_err <= BLASIUS_SOLVE_TOLERANCE, (
        f"mean Cf error {mean_err:.1%} exceeds {BLASIUS_SOLVE_TOLERANCE:.0%} "
        f"(expected ~4.6% for the verified case)"
    )
