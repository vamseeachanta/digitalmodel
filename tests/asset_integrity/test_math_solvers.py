"""
Tests for asset_integrity.common.math_solvers module.

Covers: Polynomial.solve_by_coefficients, Geometry.max_distance_for_spatial_sets.
All tests are pure-Python / numpy only — no licence or external service needed.
"""

import math
import pytest
import numpy as np


# ---------------------------------------------------------------------------
# Polynomial.solve_by_coefficients
# ---------------------------------------------------------------------------


class TestPolynomialSolveByCoefficients:
    """Tests for numpy-roots-based polynomial solver."""

    def _make(self):
        from digitalmodel.asset_integrity.common.math_solvers import Polynomial
        return Polynomial()

    def test_linear_equation_single_root(self):
        # 2x - 4 = 0  → x = 2
        poly = self._make()
        roots = poly.solve_by_coefficients([2, -4])
        assert len(roots) == 1
        assert roots[0] == pytest.approx(2.0)

    def test_quadratic_two_real_roots(self):
        # x^2 - 5x + 6 = 0  → x = 2, x = 3
        poly = self._make()
        roots = poly.solve_by_coefficients([1, -5, 6])
        real_roots = sorted(r.real for r in roots if abs(r.imag) < 1e-10)
        assert real_roots == pytest.approx([2.0, 3.0])

    def test_quadratic_equal_roots(self):
        # (x - 3)^2 = x^2 - 6x + 9  → x = 3 (double root)
        # numpy roots may introduce tiny imaginary noise; use generous threshold
        poly = self._make()
        roots = poly.solve_by_coefficients([1, -6, 9])
        real_roots = [r.real for r in roots if abs(r.imag) < 1e-6]
        assert len(real_roots) == 2
        for r in real_roots:
            assert r == pytest.approx(3.0, abs=1e-6)

    def test_cubic_three_real_roots(self):
        # x^3 - 6x^2 + 11x - 6 = (x-1)(x-2)(x-3) → x=1, 2, 3
        poly = self._make()
        roots = poly.solve_by_coefficients([1, -6, 11, -6])
        real_roots = sorted(r.real for r in roots if abs(r.imag) < 1e-10)
        assert real_roots == pytest.approx([1.0, 2.0, 3.0], abs=1e-8)

    def test_quadratic_complex_roots_count(self):
        # x^2 + 1 = 0  → two complex roots (no real solutions)
        poly = self._make()
        roots = poly.solve_by_coefficients([1, 0, 1])
        assert len(roots) == 2
        real_parts = [abs(r.imag) for r in roots]
        assert all(v > 0.5 for v in real_parts)

    def test_constant_leading_coefficient(self):
        # 3x - 9 = 0  → x = 3
        poly = self._make()
        roots = poly.solve_by_coefficients([3, -9])
        assert roots[0].real == pytest.approx(3.0)

    def test_degree_matches_coefficient_count_minus_one(self):
        # degree-4 polynomial should have 4 roots
        poly = self._make()
        roots = poly.solve_by_coefficients([1, 0, 0, 0, -16])
        assert len(roots) == 4

    def test_returns_numpy_array(self):
        poly = self._make()
        result = poly.solve_by_coefficients([1, -1])
        assert isinstance(result, np.ndarray)

    def test_float_coefficients(self):
        # 0.5x - 1.5 = 0  → x = 3.0
        poly = self._make()
        roots = poly.solve_by_coefficients([0.5, -1.5])
        assert roots[0].real == pytest.approx(3.0)

    def test_negative_leading_coefficient(self):
        # -x + 4 = 0  → x = 4
        poly = self._make()
        roots = poly.solve_by_coefficients([-1, 4])
        assert roots[0].real == pytest.approx(4.0)


# ---------------------------------------------------------------------------
# Geometry.max_distance_for_spatial_sets
# ---------------------------------------------------------------------------


class TestGeometryMaxDistance:
    """Tests for pairwise distance computation on spatial datasets."""

    def _make(self):
        from digitalmodel.asset_integrity.common.math_solvers import Geometry
        return Geometry()

    def _two_point_set(self):
        # Two points: (0,0,0) and (3,4,0)  → expected max distance = 5.0
        return np.array([[0.0, 0.0, 0.0], [3.0, 4.0, 0.0]])

    def test_result_contains_required_keys(self):
        geo = self._make()
        result = geo.max_distance_for_spatial_sets(self._two_point_set())
        assert "distance_max" in result
        assert "distance_min" in result
        assert "max_data_set" in result
        assert "min_data_set" in result

    def test_max_distance_for_two_known_points(self):
        geo = self._make()
        result = geo.max_distance_for_spatial_sets(self._two_point_set())
        assert result["distance_max"] == pytest.approx(5.0)

    def test_min_distance_for_two_known_points(self):
        # With only two points, min non-zero dist == max dist == 5.0
        geo = self._make()
        result = geo.max_distance_for_spatial_sets(self._two_point_set())
        assert result["distance_min"] == pytest.approx(5.0)

    def test_max_distance_non_negative(self):
        geo = self._make()
        pts = np.random.default_rng(42).uniform(-10, 10, (10, 3))
        result = geo.max_distance_for_spatial_sets(pts)
        assert result["distance_max"] >= 0.0

    def test_min_distance_positive_for_distinct_points(self):
        geo = self._make()
        pts = np.array([[0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [2.0, 0.0, 0.0]])
        result = geo.max_distance_for_spatial_sets(pts)
        assert result["distance_min"] > 0.0

    def test_max_distance_greater_or_equal_min_distance(self):
        geo = self._make()
        pts = np.random.default_rng(7).uniform(-5, 5, (15, 3))
        result = geo.max_distance_for_spatial_sets(pts)
        assert result["distance_max"] >= result["distance_min"]

    def test_three_collinear_points(self):
        # Points at x = 0, 5, 10 → max = 10, min = 5
        geo = self._make()
        pts = np.array([[0.0, 0.0, 0.0], [5.0, 0.0, 0.0], [10.0, 0.0, 0.0]])
        result = geo.max_distance_for_spatial_sets(pts)
        assert result["distance_max"] == pytest.approx(10.0)
        assert result["distance_min"] == pytest.approx(5.0)

    def test_unit_cube_corners_max_is_sqrt3(self):
        # Eight corners of a unit cube — max is body diagonal = sqrt(3)
        geo = self._make()
        corners = np.array([
            [0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [0.0, 1.0, 0.0],
            [1.0, 1.0, 0.0], [0.0, 0.0, 1.0], [1.0, 0.0, 1.0],
            [0.0, 1.0, 1.0], [1.0, 1.0, 1.0],
        ])
        result = geo.max_distance_for_spatial_sets(corners)
        assert result["distance_max"] == pytest.approx(math.sqrt(3.0))
