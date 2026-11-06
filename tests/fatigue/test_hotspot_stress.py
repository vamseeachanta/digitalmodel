"""
Tests for Hotspot Stress Methodology — DNV-RP-C203 (2021) Section 4.3

TDD: tests validate existing hotspot_stress.py implementation.
Covers Type A surface extrapolation and Type B through-thickness linearisation.

Issue: #1676 (P0 — fatigue expansion, hotspot stress)
"""

import json
import math
from pathlib import Path
import pytest
import numpy as np

from digitalmodel.fatigue.hotspot_stress import (
    HotspotInput,
    HotspotResult,
    ThroughThicknessResult,
    extrapolate_hotspot_linear,
    extrapolate_hotspot_quadratic,
    extrapolate_hotspot,
    through_thickness_linearisation,
    recommended_readout_distances,
)


# ── Test: Type A — Linear surface extrapolation ─────────────────────────────

class TestLinearExtrapolation:
    """DNV-RP-C203 Eq. 4.1: sigma_hs = 1.67 * sigma_0.4t - 0.67 * sigma_1.0t"""

    def test_basic_linear_extrapolation(self):
        """Known hand calculation: 0.4t=200 MPa, 1.0t=180 MPa.
        sigma_hs = 1.67*200 - 0.67*180 = 334 - 120.6 = 213.4 MPa.
        """
        result = extrapolate_hotspot_linear(
            plate_thickness=25.0,
            stress_at_04t=200.0,
            stress_at_10t=180.0,
        )
        assert isinstance(result, HotspotResult)
        assert result.hotspot_stress == pytest.approx(213.4, abs=0.1)

    def test_linear_uniform_stress_field(self):
        """Uniform stress field: if 0.4t == 1.0t == S, then hs = S.
        sigma_hs = 1.67*S - 0.67*S = S.
        """
        S = 150.0
        result = extrapolate_hotspot_linear(
            plate_thickness=20.0,
            stress_at_04t=S,
            stress_at_10t=S,
        )
        assert result.hotspot_stress == pytest.approx(S, abs=0.01)

    def test_linear_gradient_positive(self):
        """When stress decreases away from weld toe, gradient is positive."""
        result = extrapolate_hotspot_linear(
            plate_thickness=25.0,
            stress_at_04t=200.0,
            stress_at_10t=180.0,
        )
        # gradient = (200-180)/(25.0-10.0) = (200-180)/(0.6*25) = 20/15 = 1.333
        assert result.gradient > 0

    def test_linear_result_method_string(self):
        """Result method should mention linear and DNV-RP-C203."""
        result = extrapolate_hotspot_linear(25.0, 200.0, 180.0)
        assert "linear" in result.method.lower() or "Linear" in result.method

    def test_linear_read_points_present(self):
        """Result should contain the read-out points used."""
        result = extrapolate_hotspot_linear(25.0, 200.0, 180.0)
        assert "0.4t" in result.read_points
        assert "1.0t" in result.read_points
        assert result.read_points["0.4t"] == 200.0
        assert result.read_points["1.0t"] == 180.0


# ── Test: Type A — Quadratic surface extrapolation ──────────────────────────

class TestQuadraticExtrapolation:
    """DNV-RP-C203 Eq. 4.2: sigma_hs = 2.52*s_0.4t - 2.24*s_0.9t + 0.72*s_1.4t"""

    def test_basic_quadratic_extrapolation(self):
        """Hand calc: 0.4t=200, 0.9t=185, 1.4t=170.
        sigma_hs = 2.52*200 - 2.24*185 + 0.72*170 = 504 - 414.4 + 122.4 = 212.0.
        """
        result = extrapolate_hotspot_quadratic(
            plate_thickness=25.0,
            stress_at_04t=200.0,
            stress_at_09t=185.0,
            stress_at_14t=170.0,
        )
        assert isinstance(result, HotspotResult)
        assert result.hotspot_stress == pytest.approx(212.0, abs=0.1)

    def test_quadratic_uniform_field(self):
        """Uniform stress: hs = 2.52*S - 2.24*S + 0.72*S = S."""
        S = 100.0
        result = extrapolate_hotspot_quadratic(25.0, S, S, S)
        assert result.hotspot_stress == pytest.approx(S, abs=0.01)

    def test_quadratic_has_three_read_points(self):
        """Quadratic result should have 3 read-out points."""
        result = extrapolate_hotspot_quadratic(25.0, 200.0, 185.0, 170.0)
        assert len(result.read_points) == 3
        assert "0.4t" in result.read_points
        assert "0.9t" in result.read_points
        assert "1.4t" in result.read_points


# ── Test: General polynomial extrapolation via HotspotInput ─────────────────

class TestExtrapolateHotspot:
    """General-purpose extrapolation from arbitrary points."""

    def test_general_linear_matches_specific(self):
        """General linear with 0.4t and 1.0t should be close to specific formula."""
        t = 25.0
        s_04t = 200.0
        s_10t = 180.0
        inp = HotspotInput(
            plate_thickness=t,
            distances=[0.4 * t, 1.0 * t],
            stresses=[s_04t, s_10t],
            method="linear",
        )
        result = extrapolate_hotspot(inp)
        # The polynomial fit at x=0 should be close to the DNV formula
        # Not exact due to different extrapolation method (polyfit vs coefficients)
        assert result.hotspot_stress == pytest.approx(213.3, abs=1.5)

    def test_general_quadratic_needs_three_points(self):
        """Quadratic with only 2 points should raise ValueError."""
        t = 25.0
        inp = HotspotInput(
            plate_thickness=t,
            distances=[0.4 * t, 1.0 * t],
            stresses=[200.0, 180.0],
            method="quadratic",
        )
        with pytest.raises(ValueError, match="3 points"):
            extrapolate_hotspot(inp)

    def test_general_mismatched_lengths_raises(self):
        """Mismatched distances/stresses should raise ValueError."""
        inp = HotspotInput(
            plate_thickness=25.0,
            distances=[10.0, 25.0],
            stresses=[200.0],
        )
        with pytest.raises(ValueError, match="equal length"):
            extrapolate_hotspot(inp)

    def test_unknown_method_raises(self):
        """Unknown method string raises ValueError."""
        inp = HotspotInput(
            plate_thickness=25.0,
            distances=[10.0, 25.0],
            stresses=[200.0, 180.0],
            method="cubic",
        )
        with pytest.raises(ValueError, match="Unknown method"):
            extrapolate_hotspot(inp)


# ── Test: Type B — Through-thickness linearisation ──────────────────────────

class TestThroughThicknessLinearisation:
    """IIW Type B decomposition: membrane + bending + non-linear peak."""

    def test_uniform_tension_membrane_only(self):
        """Pure uniform tension: sigma_m = S, sigma_b = 0."""
        t = 20.0
        positions = np.linspace(0, t, 20)
        stresses = np.full(20, 100.0)  # uniform 100 MPa
        result = through_thickness_linearisation(t, positions, stresses)
        assert isinstance(result, ThroughThicknessResult)
        assert result.membrane_stress == pytest.approx(100.0, abs=0.5)
        assert result.bending_stress == pytest.approx(0.0, abs=0.5)
        assert result.hotspot_stress == pytest.approx(100.0, abs=0.5)

    def test_pure_bending_distribution(self):
        """Linear stress: +S at surface (y=0), -S at root (y=t).
        sigma_m = 0, sigma_b = S.
        """
        t = 20.0
        S = 100.0
        positions = np.linspace(0, t, 50)
        # Linear from +S to -S
        stresses = S * (1.0 - 2.0 * positions / t)
        result = through_thickness_linearisation(t, positions, stresses)
        assert result.membrane_stress == pytest.approx(0.0, abs=1.0)
        assert result.bending_stress == pytest.approx(S, abs=2.0)
        assert result.hotspot_stress == pytest.approx(S, abs=2.0)

    def test_hotspot_is_membrane_plus_bending(self):
        """Hotspot stress = membrane + bending, always."""
        t = 25.0
        positions = np.linspace(0, t, 30)
        stresses = 150.0 + 50.0 * np.cos(np.pi * positions / t)
        result = through_thickness_linearisation(t, positions, stresses)
        assert result.hotspot_stress == pytest.approx(
            result.membrane_stress + result.bending_stress, abs=0.01
        )

    def test_insufficient_points_raises(self):
        """Needs at least 2 points."""
        with pytest.raises(ValueError, match="at least 2"):
            through_thickness_linearisation(20.0, [0.0], [100.0])

    def test_mismatched_arrays_raises(self):
        """Mismatched positions/stresses raises ValueError."""
        with pytest.raises(ValueError, match="equal length"):
            through_thickness_linearisation(20.0, [0, 10, 20], [100, 90])


@pytest.fixture(scope="module")
def linearisation_polynomials():
    """Load synthetic profiles and their independent hand integrations."""
    fixture = Path(__file__).with_name("through_thickness_polynomials.json")
    return json.loads(fixture.read_text(encoding="utf-8"))


class TestThroughThicknessPolynomials:
    """Check signed components against the fixture's polynomial integrals.

    The fixture derives the trapezoidal moment error and the rounding bound;
    no extrapolation factors or fatigue design constants enter these cases.
    The peak convention is the signed surface residual, not its magnitude.
    """

    signed_components = (
        "membrane_stress", "bending_stress", "hotspot_stress", "peak_stress"
    )

    @pytest.mark.parametrize("profile_name", ["constant", "affine", "superposition"])
    def test_independently_integrated_profile(
        self, linearisation_polynomials, profile_name
    ):
        data = linearisation_polynomials
        profile = data["profiles"][profile_name]
        positions = np.asarray(data["positions_mm"])
        stresses = profile["intercept_mpa"] + profile["slope_mpa_per_mm"] * positions
        result = through_thickness_linearisation(
            data["thickness_mm"], positions, stresses
        )
        expected = profile["integrated"]
        rounding = data["rounding_bound_mpa"]
        tolerance = profile["bending_quadrature_bound_mpa"] + rounding

        assert result.membrane_stress == pytest.approx(
            expected["membrane_mpa"], rel=0, abs=rounding
        )
        assert result.bending_stress == pytest.approx(
            expected["bending_mpa"], rel=0, abs=tolerance
        )
        assert result.hotspot_stress == pytest.approx(
            expected["surface_mpa"], rel=0, abs=tolerance
        )
        assert result.peak_stress == pytest.approx(
            expected["peak_mpa"], rel=0, abs=tolerance
        )
        # Two rounded components reconstruct either physical surface.
        assert result.membrane_stress - result.bending_stress == pytest.approx(
            expected["root_mpa"], rel=0, abs=tolerance + rounding
        )
        assert result.hotspot_stress + result.peak_stress == pytest.approx(
            expected["surface_mpa"], rel=0, abs=rounding + rounding
        )
        assert result.peak_stress == pytest.approx(
            stresses[0] - result.hotspot_stress,
            rel=0,
            abs=rounding + rounding,
        )
        if profile_name == "constant":
            assert result.bending_stress == 0.0
            assert result.peak_stress == 0.0

    @pytest.mark.parametrize("profile_name", ["constant", "affine", "superposition"])
    def test_sign_reversal(self, linearisation_polynomials, profile_name):
        """Negating stress negates every signed result, including the peak."""
        data = linearisation_polynomials
        profile = data["profiles"][profile_name]
        positions = np.asarray(data["positions_mm"])
        stresses = profile["intercept_mpa"] + profile["slope_mpa_per_mm"] * positions
        positive = through_thickness_linearisation(
            data["thickness_mm"], positions, stresses
        )
        negative = through_thickness_linearisation(
            data["thickness_mm"], positions, -stresses
        )
        for component in self.signed_components:
            assert getattr(negative, component) == pytest.approx(
                -getattr(positive, component), rel=0, abs=data["rounding_bound_mpa"]
            )

    @pytest.mark.parametrize("profile_name", ["constant", "affine", "superposition"])
    @pytest.mark.parametrize("thickness_scale", [0.25, 4.0], ids=["quarter", "quadruple"])
    def test_thickness_rescaling(
        self, linearisation_polynomials, profile_name, thickness_scale
    ):
        """Quarter/quadruple thickness preserves stress as a function of y/t."""
        data = linearisation_polynomials
        profile = data["profiles"][profile_name]
        positions = np.asarray(data["positions_mm"])
        stresses = profile["intercept_mpa"] + profile["slope_mpa_per_mm"] * positions
        original = through_thickness_linearisation(
            data["thickness_mm"], positions, stresses
        )
        scaled = through_thickness_linearisation(
            data["thickness_mm"] * thickness_scale,
            positions * thickness_scale,
            stresses,
        )
        for component in self.signed_components:
            assert getattr(scaled, component) == pytest.approx(
                getattr(original, component), rel=0, abs=data["rounding_bound_mpa"]
            )

    def test_superposition(self, linearisation_polynomials):
        """Linear integration makes the combined result the sum of its parts."""
        data = linearisation_polynomials
        positions = np.asarray(data["positions_mm"])
        results = {}
        for name, profile in data["profiles"].items():
            stresses = profile["intercept_mpa"] + profile["slope_mpa_per_mm"] * positions
            results[name] = through_thickness_linearisation(
                data["thickness_mm"], positions, stresses
            )
        # The combined value and both summands each have one rounding error.
        tolerance = sum(data["rounding_bound_mpa"] for _ in results)
        for component in self.signed_components:
            assert getattr(results["superposition"], component) == pytest.approx(
                getattr(results["constant"], component)
                + getattr(results["affine"], component),
                rel=0,
                abs=tolerance,
            )


# ── Test: Recommended read-out distances ────────────────────────────────────

class TestRecommendedReadoutDistances:
    """DNV-RP-C203 Table 4-1 read-out point spacing."""

    def test_linear_readout_distances(self):
        """Linear: 2 points at 0.4t and 1.0t."""
        t = 25.0
        dists = recommended_readout_distances(t, method="linear")
        assert len(dists) == 2
        assert dists[0] == pytest.approx(0.4 * t)
        assert dists[1] == pytest.approx(1.0 * t)

    def test_quadratic_readout_distances(self):
        """Quadratic: 3 points at 0.4t, 0.9t, 1.4t."""
        t = 30.0
        dists = recommended_readout_distances(t, method="quadratic")
        assert len(dists) == 3
        assert dists[0] == pytest.approx(0.4 * t)
        assert dists[1] == pytest.approx(0.9 * t)
        assert dists[2] == pytest.approx(1.4 * t)

    def test_unknown_method_raises(self):
        """Unknown method raises ValueError."""
        with pytest.raises(ValueError):
            recommended_readout_distances(25.0, method="invalid")
