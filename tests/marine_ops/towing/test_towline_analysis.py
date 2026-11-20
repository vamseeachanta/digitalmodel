"""Analytic two-support catenary regressions; forces N and coordinates m."""
import math

import pytest


def test_two_point_catenary_matches_analytic_case():
    from digitalmodel.marine_ops.towing.towline_analysis import solve_catenary

    # y=a*cosh(x/a): a=100 m, supports x=+-50 m, w=10 N/m.
    length = 200 * math.sinh(0.5)
    result = solve_catenary(span_m=100, length_m=length, weight_n_m=10)
    assert result.horizontal_tension_n == pytest.approx(1000, rel=0.005)
    assert result.top_tension_n == pytest.approx(1000 * math.cosh(0.5), rel=0.005)
    assert result.sag_m == pytest.approx(100 * (math.cosh(0.5) - 1), rel=0.005)
    assert result.scope == pytest.approx(length / 100)
    assert result.x_m[0] == 0
    assert result.x_m[-1] == pytest.approx(100)
    assert result.z_m[0] == pytest.approx(0, abs=1e-10)
    assert result.z_m[-1] == pytest.approx(0, abs=1e-10)


def test_pull_controlled_geometry_recovers_supports_and_equilibrium():
    from digitalmodel.marine_ops.towing.towline_analysis import catenary_from_pull

    result = catenary_from_pull(horizontal_tension_n=1000, length_m=100, weight_n_m=10)
    assert result.span_m == pytest.approx(200 * math.asinh(0.5))
    assert result.top_tension_n == pytest.approx(math.hypot(1000, 500))
    assert result.sag_m == pytest.approx(100 * (math.sqrt(1.25) - 1))


@pytest.mark.parametrize('elevation', [25, -25])
def test_unequal_supports_close_at_prescribed_elevation(elevation):
    from digitalmodel.marine_ops.towing.towline_analysis import solve_catenary

    result = solve_catenary(span_m=100, length_m=115, weight_n_m=10,
                            elevation_m=elevation, n_points=1001)
    assert result.z_m[-1] == pytest.approx(elevation)
    arc = sum(math.hypot(x2 - x1, z2 - z1) for x1, x2, z1, z2 in
              zip(result.x_m, result.x_m[1:], result.z_m, result.z_m[1:]))
    assert arc == pytest.approx(115, rel=0.005)
    assert result.sag_m > 0


@pytest.mark.parametrize('overrides', [
    {'length_m': 100}, {'length_m': 99}, {'span_m': 0},
    {'weight_n_m': -1}, {'length_m': float('nan')},
    {'elevation_m': float('inf')}, {'n_points': 2}, {'n_points': True},
])
def test_invalid_catenary_is_rejected(overrides):
    from digitalmodel.marine_ops.towing.towline_analysis import solve_catenary

    args = dict(span_m=100, length_m=110, weight_n_m=10)
    with pytest.raises(ValueError):
        solve_catenary(**(args | overrides))
