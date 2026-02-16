# ABOUTME: Tests for API STD 2RD riser wall thickness design code strategy
# ABOUTME: Burst, collapse checks, thin-wall plastic moment, Method 1/2 envelopes

import math

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.structural.analysis.wall_thickness_codes.api_std_2rd import (
    ApiStd2rdStrategy,
)
from digitalmodel.structural.analysis.wall_thickness_mt_report import (
    _trace_envelope_method1,
    _trace_envelope_method2,
)


@pytest.fixture
def geometry():
    return PipeGeometry(
        outer_diameter=0.27305,
        wall_thickness=0.02144,
        corrosion_allowance=0.001,
    )


@pytest.fixture
def material():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


@pytest.fixture
def loads():
    return DesignLoads(
        internal_pressure=20e6,
        external_pressure=10e6,
    )


@pytest.fixture
def factors():
    return DesignFactors()


@pytest.fixture
def strategy():
    return ApiStd2rdStrategy()


def test_registry_contains_api_std_2rd():
    """API STD 2RD strategy is auto-registered in CODE_REGISTRY."""
    assert DesignCode.API_STD_2RD in CODE_REGISTRY
    assert CODE_REGISTRY[DesignCode.API_STD_2RD] is ApiStd2rdStrategy


def test_burst_utilisation_reasonable(strategy, geometry, material, loads, factors):
    """Burst utilisation is positive and < 1.0 for standard riser pipe."""
    results = strategy.run_checks(geometry, material, loads, factors)
    util, details = results["burst"]
    assert 0 < util < 1.0
    assert details["f_d"] == 0.80
    assert details["p_b"] > 0


def test_collapse_utilisation_reasonable(strategy, geometry, material, loads, factors):
    """Collapse utilisation is positive and < 1.0 for standard riser pipe."""
    results = strategy.run_checks(geometry, material, loads, factors)
    util, details = results["collapse"]
    assert 0 < util < 1.0
    assert details["f_c"] == 0.60


def test_plastic_moment_thin_wall(strategy, geometry, material):
    """API STD 2RD uses thin-wall plastic moment: M_p = (4/pi) * M_y."""
    D = geometry.outer_diameter
    t = geometry.wall_thickness
    smys = material.smys

    # Expected: M_y = (pi/4) * SMYS * (D-t)^2 * t, M_p = (4/pi) * M_y
    M_y = (math.pi / 4) * smys * (D - t) ** 2 * t
    M_p_expected = (4 / math.pi) * M_y

    M_p_actual = strategy.compute_plastic_moment(geometry, material)
    assert M_p_actual == pytest.approx(M_p_expected, rel=1e-10)

    # Also verify M_p = SMYS * (D-t)^2 * t (simplified form)
    M_p_simplified = smys * (D - t) ** 2 * t
    assert M_p_actual == pytest.approx(M_p_simplified, rel=1e-10)


def test_plastic_tension_matches_area_times_smys(strategy, geometry, material):
    """Plastic tension = A * SMYS for all API codes."""
    D = geometry.outer_diameter
    t = geometry.wall_thickness
    d_i = D - 2 * t
    A = math.pi / 4 * (D**2 - d_i**2)
    T_y_expected = A * material.smys

    T_y_actual = strategy.compute_plastic_tension(geometry, material)
    assert T_y_actual == pytest.approx(T_y_expected, rel=1e-10)


def test_method1_linear_envelope_shape(geometry, material):
    """Method 1 envelope decreases linearly from M_y at T=0 to 0 at T limit."""
    D = geometry.outer_diameter
    t = geometry.wall_thickness
    d_i = D - 2 * t
    smys = material.smys
    smts = material.smts
    A = math.pi / 4 * (D**2 - d_i**2)
    T_y = A * smys

    p_net = 10e6  # 10 MPa net
    f_d = 0.80

    T_vals, M_vals = _trace_envelope_method1(
        D, t, A, smys, smts, f_d, p_net,
        -T_y, T_y, None, T_y, n_points=100,
    )

    # At T=0 (middle of range), M should be maximum
    mid_idx = len(T_vals) // 2
    M_at_zero = M_vals[mid_idx]
    assert M_at_zero > 0

    # M should decrease as |T| increases from center
    # Check right half: M decreases monotonically
    right_half = M_vals[mid_idx:]
    for i in range(len(right_half) - 1):
        assert right_half[i + 1] <= right_half[i] + 1e-3


def test_method2_cosine_envelope_shape(geometry, material):
    """Method 2 envelope follows cosine shape from M_p at T=0 to 0 at T limit."""
    D = geometry.outer_diameter
    t = geometry.wall_thickness
    d_i = D - 2 * t
    smys = material.smys
    smts = material.smts
    A = math.pi / 4 * (D**2 - d_i**2)
    T_y = A * smys
    M_y = (math.pi / 4) * smys * (D - t) ** 2 * t
    M_p = (4 / math.pi) * M_y

    p_net = 10e6
    f_d = 0.80

    T_vals, M_vals = _trace_envelope_method2(
        D, t, A, smys, smts, f_d, p_net,
        -T_y, T_y, M_p, T_y, n_points=100,
    )

    # At T=0, M should be close to M_p * F_d_corr
    mid_idx = len(T_vals) // 2
    M_at_zero = M_vals[mid_idx]
    assert M_at_zero > 0

    # M should decrease as |T| increases
    right_half = M_vals[mid_idx:]
    for i in range(len(right_half) - 1):
        assert right_half[i + 1] <= right_half[i] + 1e-3


def test_method2_contains_method1(geometry, material):
    """Method 2 (cosine) envelope >= Method 1 (linear) at all T values.

    The cosine interaction is less conservative than linear for all
    non-boundary points.
    """
    D = geometry.outer_diameter
    t = geometry.wall_thickness
    d_i = D - 2 * t
    smys = material.smys
    smts = material.smts
    A = math.pi / 4 * (D**2 - d_i**2)
    T_y = A * smys
    M_y = (math.pi / 4) * smys * (D - t) ** 2 * t
    M_p = (4 / math.pi) * M_y

    p_net = 10e6
    f_d = 0.80

    T_vals_m1, M_vals_m1 = _trace_envelope_method1(
        D, t, A, smys, smts, f_d, p_net,
        -T_y, T_y, M_p, T_y, n_points=200,
    )
    T_vals_m2, M_vals_m2 = _trace_envelope_method2(
        D, t, A, smys, smts, f_d, p_net,
        -T_y, T_y, M_p, T_y, n_points=200,
    )

    # Method 2 should be >= Method 1 at every point (cosine >= linear)
    for i in range(len(T_vals_m1)):
        assert M_vals_m2[i] >= M_vals_m1[i] - 1e-3, (
            f"At T={T_vals_m1[i]:.0f}: Method 2 ({M_vals_m2[i]:.0f}) < "
            f"Method 1 ({M_vals_m1[i]:.0f})"
        )


def test_check_names(strategy):
    """API STD 2RD has burst and collapse checks."""
    assert strategy.check_names == ["burst", "collapse"]
    assert strategy.code_name == "API-STD-2RD"
