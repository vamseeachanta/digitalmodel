"""Tests for hotspot_stress — hotspot stress extrapolation."""

import numpy as np
import pytest

from digitalmodel.fatigue.hotspot_stress import (
    extrapolate_hotspot_linear,
    extrapolate_hotspot_quadratic,
    extrapolate_hotspot,
    through_thickness_linearisation,
    recommended_readout_distances,
    HotspotInput,
)


class TestLinearExtrapolation:
    """Test 2-point linear hotspot extrapolation."""

    def test_dnv_linear_formula(self):
        """Verify σ_hs = 1.67·σ(0.4t) − 0.67·σ(1.0t)."""
        s04 = 150.0  # MPa at 0.4t
        s10 = 120.0  # MPa at 1.0t
        result = extrapolate_hotspot_linear(20.0, s04, s10)
        expected = 1.67 * 150.0 - 0.67 * 120.0
        assert abs(result.hotspot_stress - expected) < 0.01

    def test_linear_gives_higher_than_readpoints(self):
        """For a stress gradient towards the weld, hotspot > stress at 0.4t."""
        result = extrapolate_hotspot_linear(25.0, 200.0, 160.0)
        assert result.hotspot_stress > 200.0

    def test_gradient_positive_towards_weld(self):
        """Gradient should be positive (stress increasing towards weld toe)."""
        result = extrapolate_hotspot_linear(25.0, 200.0, 160.0)
        assert result.gradient > 0


class TestQuadraticExtrapolation:
    """Test 3-point quadratic hotspot extrapolation."""

    def test_dnv_quadratic_formula(self):
        """Verify σ_hs = 2.52·σ(0.4t) − 2.24·σ(0.9t) + 0.72·σ(1.4t)."""
        s04, s09, s14 = 150.0, 130.0, 115.0
        result = extrapolate_hotspot_quadratic(20.0, s04, s09, s14)
        expected = 2.52 * s04 - 2.24 * s09 + 0.72 * s14
        assert abs(result.hotspot_stress - expected) < 0.01

    def test_quadratic_reasonable_range(self):
        """Hotspot stress from quadratic should be reasonable."""
        result = extrapolate_hotspot_quadratic(20.0, 180.0, 160.0, 145.0)
        assert 150.0 < result.hotspot_stress < 250.0

    def test_read_points_in_result(self):
        """Result should contain the read-out point data."""
        result = extrapolate_hotspot_quadratic(20.0, 180.0, 160.0, 145.0)
        assert "0.4t" in result.read_points
        assert "0.9t" in result.read_points
        assert "1.4t" in result.read_points


class TestThroughThicknessLinearisation:
    """Test Type 'b' through-thickness linearisation."""

    def test_uniform_stress_gives_pure_membrane(self):
        """Uniform stress of 100 MPa → membrane=100, bending≈0."""
        t = 20.0
        y = np.linspace(0, t, 21)
        s = np.full_like(y, 100.0)
        result = through_thickness_linearisation(t, y, s)
        assert abs(result.membrane_stress - 100.0) < 1.0
        assert abs(result.bending_stress) < 1.0

    def test_linear_gradient_gives_membrane_plus_bending(self):
        """Linear gradient 200 at surface to 0 at root."""
        t = 20.0
        y = np.linspace(0, t, 101)
        s = 200.0 * (1.0 - y / t)
        result = through_thickness_linearisation(t, y, s)
        # Membrane should be average = 100 MPa
        assert abs(result.membrane_stress - 100.0) < 2.0
        # Bending should be ~100 MPa (half the gradient)
        assert abs(result.bending_stress - 100.0) < 5.0

    def test_hotspot_equals_membrane_plus_bending(self):
        """Hotspot = membrane + bending always."""
        t = 25.0
        y = np.linspace(0, t, 51)
        s = 150.0 + 50.0 * np.cos(np.pi * y / t)
        result = through_thickness_linearisation(t, y, s)
        assert abs(result.hotspot_stress - (result.membrane_stress + result.bending_stress)) < 0.01


class TestRecommendedDistances:
    """Test read-out distance recommendations."""

    def test_linear_returns_2_points(self):
        dists = recommended_readout_distances(25.0, "linear")
        assert len(dists) == 2
        assert abs(dists[0] - 10.0) < 0.01  # 0.4×25
        assert abs(dists[1] - 25.0) < 0.01  # 1.0×25

    def test_quadratic_returns_3_points(self):
        dists = recommended_readout_distances(20.0, "quadratic")
        assert len(dists) == 3
        assert abs(dists[0] - 8.0) < 0.01   # 0.4×20

    def test_invalid_method_raises(self):
        with pytest.raises(ValueError):
            recommended_readout_distances(20.0, "cubic")
