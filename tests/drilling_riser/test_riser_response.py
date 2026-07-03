"""#1281a: static tensioned beam-column riser-response model.

Test oracles are the closed-form solution of ``EI y'''' - T y'' = w`` with pinned
(moment-free) flex-joint ends, y(0)=0, y(L)=offset:

  * no offset, no current  -> zero deflection / angle / moment
  * offset, no current      -> rigid tilt y = X z/L; both flex-joint angles = X/L
                               (rad); ZERO bending moment (pinned-pinned tension member)
  * current, no offset      -> nonzero angles + interior bending moment
  * monotonicity            -> angle increases with offset; angle AND peak moment
                               increase with current (the anti-degeneracy guard)
"""
from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.drilling_riser.riser_response import (
    RiserResponse,
    solve_static_response,
)

# A representative highly-tensioned 21" drilling riser (SI, synthetic screening values).
_BASE = dict(
    length_m=1500.0,
    tension_n=3.0e6,          # ~674 kip effective tension
    ei_nm2=2.5e9,             # E*I for a 21" x 1" steel section, order of magnitude
    n_nodes=201,
)


def _drag_load_n_per_m(current_speed_mps: float, diameter_m: float = 0.5334) -> float:
    """Static current drag per unit length: w = 1/2 rho Cd D U^2 (SI)."""
    rho_sw, cd = 1025.0, 1.2
    return 0.5 * rho_sw * cd * diameter_m * current_speed_mps**2


# -- oracle 1: quiescent -> zero everything ------------------------------------


def test_no_offset_no_current_is_zero():
    r = solve_static_response(top_offset_m=0.0, current_load_n_per_m=0.0, **_BASE)
    assert isinstance(r, RiserResponse)
    assert r.angle_lower_rad == pytest.approx(0.0, abs=1e-12)
    assert r.angle_upper_rad == pytest.approx(0.0, abs=1e-12)
    assert np.allclose(r.deflection_m, 0.0, atol=1e-9)
    assert np.max(np.abs(r.bending_moment_nm)) == pytest.approx(0.0, abs=1e-3)


# -- oracle 2: pure offset -> rigid tilt, no bending ---------------------------


def test_pure_offset_is_rigid_tilt_no_moment():
    offset = 30.0  # m (2% of 1500 m WD)
    r = solve_static_response(top_offset_m=offset, current_load_n_per_m=0.0, **_BASE)
    expected_slope = offset / _BASE["length_m"]
    # both flex joints rotate by X/L; deflection is a straight line X z/L
    assert r.angle_lower_rad == pytest.approx(expected_slope, rel=1e-6)
    assert r.angle_upper_rad == pytest.approx(expected_slope, rel=1e-6)
    line = expected_slope * r.z_m
    assert np.allclose(r.deflection_m, line, atol=1e-6)
    # a pinned-pinned tensioned member under pure end offset carries NO moment
    assert np.max(np.abs(r.bending_moment_nm)) == pytest.approx(0.0, abs=1.0)


# -- oracle 3: pure current -> angles + interior moment ------------------------


def test_pure_current_bends_the_riser():
    w = _drag_load_n_per_m(1.0)  # 1 m/s surface-equivalent uniform current
    r = solve_static_response(top_offset_m=0.0, current_load_n_per_m=w, **_BASE)
    # cable-limit lower-joint angle ~ w L / (2 T); sign nonzero, small-angle
    approx_cable = w * _BASE["length_m"] / (2.0 * _BASE["tension_n"])
    assert r.angle_lower_rad == pytest.approx(approx_cable, rel=0.05)
    # moment is zero at the pinned ends, nonzero in the interior
    assert abs(r.bending_moment_nm[0]) == pytest.approx(0.0, abs=1.0)
    assert abs(r.bending_moment_nm[-1]) == pytest.approx(0.0, abs=1.0)
    assert np.max(np.abs(r.bending_moment_nm)) > 0.0


# -- oracle 4: monotonicity (the anti-degeneracy guard) ------------------------


def test_angle_increases_with_offset():
    a = solve_static_response(top_offset_m=15.0, current_load_n_per_m=0.0, **_BASE)
    b = solve_static_response(top_offset_m=45.0, current_load_n_per_m=0.0, **_BASE)
    assert b.angle_lower_rad > a.angle_lower_rad > 0.0


def test_angle_and_moment_increase_with_current():
    a = solve_static_response(
        top_offset_m=20.0, current_load_n_per_m=_drag_load_n_per_m(0.5), **_BASE
    )
    b = solve_static_response(
        top_offset_m=20.0, current_load_n_per_m=_drag_load_n_per_m(1.5), **_BASE
    )
    assert b.max_bending_moment_nm > a.max_bending_moment_nm > 0.0
    assert abs(b.angle_lower_rad) > abs(a.angle_lower_rad)


# -- degrees convenience + shape ----------------------------------------------


def test_degrees_and_grid_shape():
    r = solve_static_response(top_offset_m=30.0, current_load_n_per_m=_drag_load_n_per_m(1.0), **_BASE)
    assert r.angle_lower_deg == pytest.approx(math.degrees(r.angle_lower_rad))
    assert r.z_m.shape == r.deflection_m.shape == r.bending_moment_nm.shape
    assert r.z_m[0] == 0.0 and r.z_m[-1] == pytest.approx(_BASE["length_m"])
