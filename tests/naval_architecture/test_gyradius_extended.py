# ABOUTME: Extended tests for gyradius module — drillship/jack-up, inertia roundtrip, edge cases.
# ABOUTME: Issue #1972 — Test coverage uplift for overnight modules.
"""
Extended tests for digitalmodel.naval_architecture.gyradius

Covers:
- All platform types parameterized
- Inertia roundtrip property tests (I → k → I identity)
- Scaling laws (dimensional analysis)
- Zero/boundary mass and displacement values
- Cross-platform comparison invariants
- Gyradius ratio sanity checks
- GyradiusResult property coverage
"""

from __future__ import annotations

import math
import pytest

from digitalmodel.naval_architecture.gyradius import (
    GYRADIUS_RATIOS,
    GyradiusResult,
    estimate_inertia_from_gyradius,
    gyradius_for_platform_type,
    gyradius_from_inertia,
)


# ---------------------------------------------------------------------------
# Parameterized platform types
# ---------------------------------------------------------------------------

ALL_PLATFORM_TYPES = ["fpso", "semisubmersible", "spar", "tlp", "barge"]


class TestAllPlatformTypes:
    """Verify basic properties hold for every platform type."""

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_positive_gyradii(self, ptype):
        """All gyradius values must be positive for valid inputs."""
        r = gyradius_for_platform_type(ptype, 50000)
        assert r.roll > 0
        assert r.pitch > 0
        assert r.yaw > 0
        assert r.surge > 0
        assert r.sway > 0
        assert r.heave > 0

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_mass_equals_displacement_times_1000(self, ptype):
        """mass_kg should be displacement_t * 1000."""
        disp = 75000
        r = gyradius_for_platform_type(ptype, disp)
        assert r.mass_kg == pytest.approx(disp * 1000.0, rel=1e-9)

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_displacement_preserved(self, ptype):
        """displacement_t should be exactly the input."""
        disp = 120000.0
        r = gyradius_for_platform_type(ptype, disp)
        assert r.displacement_t == pytest.approx(disp, rel=1e-9)

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_kxx_kyy_kzz_aliases(self, ptype):
        """kxx/kyy/kzz should equal roll/pitch/yaw."""
        r = gyradius_for_platform_type(ptype, 50000)
        assert r.kxx == r.roll
        assert r.kyy == r.pitch
        assert r.kzz == r.yaw

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_moment_of_inertia_properties(self, ptype):
        """ixx/iyy/izz = mass * k**2."""
        r = gyradius_for_platform_type(ptype, 50000)
        assert r.ixx == pytest.approx(r.mass_kg * r.roll ** 2, rel=1e-9)
        assert r.iyy == pytest.approx(r.mass_kg * r.pitch ** 2, rel=1e-9)
        assert r.izz == pytest.approx(r.mass_kg * r.yaw ** 2, rel=1e-9)


# ---------------------------------------------------------------------------
# Scaling laws
# ---------------------------------------------------------------------------

class TestScalingLaws:
    """Verify that gyradii scale correctly with displacement."""

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_larger_displacement_larger_gyradii(self, ptype):
        """Doubling displacement should increase all gyradii (power-law scaling)."""
        r1 = gyradius_for_platform_type(ptype, 25000)
        r2 = gyradius_for_platform_type(ptype, 100000)
        assert r2.roll > r1.roll
        assert r2.pitch > r1.pitch
        assert r2.yaw > r1.yaw

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_gyradii_scale_sublinearly(self, ptype):
        """Gyradii should scale as power < 1 of displacement (dimensional analysis)."""
        r1 = gyradius_for_platform_type(ptype, 10000)
        r2 = gyradius_for_platform_type(ptype, 100000)
        # If displacement goes up 10x, gyradii should go up less than 10x
        ratio = r2.roll / r1.roll
        assert 1.0 < ratio < 10.0, f"Scaling ratio {ratio} out of expected range"

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_inertia_scales_with_mass(self, ptype):
        """Moment of inertia should increase with displacement."""
        r1 = gyradius_for_platform_type(ptype, 30000)
        r2 = gyradius_for_platform_type(ptype, 90000)
        assert r2.ixx > r1.ixx
        assert r2.iyy > r1.iyy
        assert r2.izz > r1.izz

    def test_very_small_displacement(self):
        """Very small displacement (1 tonne) should still produce valid results."""
        r = gyradius_for_platform_type("fpso", 1)
        assert r.roll > 0
        assert r.mass_kg == pytest.approx(1000.0)

    def test_very_large_displacement(self):
        """Very large displacement (1M tonnes) should produce finite results."""
        r = gyradius_for_platform_type("fpso", 1_000_000)
        assert math.isfinite(r.roll)
        assert math.isfinite(r.pitch)
        assert math.isfinite(r.yaw)
        assert r.mass_kg == pytest.approx(1e9)


# ---------------------------------------------------------------------------
# Inertia roundtrip property tests
# ---------------------------------------------------------------------------

class TestInertiaRoundtrip:
    """Verify I → k → I identity across many inputs."""

    @pytest.mark.parametrize("mass,ixx,iyy,izz", [
        (1000.0, 9000.0, 16000.0, 25000.0),
        (200e6, 6.8e10, 7.2e11, 8.5e10),
        (1e3, 1e6, 1e7, 1e8),
        (50e6, 1e9, 5e9, 2e9),
    ])
    def test_roundtrip_identity(self, mass, ixx, iyy, izz):
        """I → k → I should be identity."""
        r = gyradius_from_inertia(mass, ixx, iyy, izz)
        recovered = estimate_inertia_from_gyradius(mass, r.roll, r.pitch, r.yaw)
        assert recovered["ixx"] == pytest.approx(ixx, rel=1e-6)
        assert recovered["iyy"] == pytest.approx(iyy, rel=1e-6)
        assert recovered["izz"] == pytest.approx(izz, rel=1e-6)

    @pytest.mark.parametrize("mass,kx,ky,kz", [
        (1000.0, 3.0, 4.0, 5.0),
        (1e6, 10.0, 50.0, 12.0),
        (5e4, 7.5, 25.0, 8.0),
    ])
    def test_k_to_I_to_k_roundtrip(self, mass, kx, ky, kz):
        """k → I → k should preserve gyradii."""
        inertias = estimate_inertia_from_gyradius(mass, kx, ky, kz)
        r = gyradius_from_inertia(mass, inertias["ixx"], inertias["iyy"], inertias["izz"])
        assert r.roll == pytest.approx(kx, rel=1e-6)
        assert r.pitch == pytest.approx(ky, rel=1e-6)
        assert r.yaw == pytest.approx(kz, rel=1e-6)


# ---------------------------------------------------------------------------
# gyradius_from_inertia edge cases
# ---------------------------------------------------------------------------

class TestGyradiusFromInertiaEdges:
    """Edge cases for the direct inertia-to-gyradius computation."""

    def test_zero_mass_raises(self):
        with pytest.raises(ValueError, match="positive"):
            gyradius_from_inertia(0, 1, 1, 1)

    def test_very_small_mass(self):
        r = gyradius_from_inertia(0.001, 0.001, 0.001, 0.001)
        assert r.roll == pytest.approx(1.0, rel=1e-6)
        assert r.pitch == pytest.approx(1.0, rel=1e-6)
        assert r.yaw == pytest.approx(1.0, rel=1e-6)

    def test_all_zero_inertia(self):
        """Zero inertia → zero gyradii."""
        r = gyradius_from_inertia(1000.0, 0.0, 0.0, 0.0)
        assert r.roll == 0.0
        assert r.pitch == 0.0
        assert r.yaw == 0.0

    def test_asymmetric_inertia(self):
        """Large pitch inertia, small roll/yaw."""
        r = gyradius_from_inertia(1e6, 1e8, 1e12, 1e8)
        assert r.pitch > r.roll
        assert r.pitch > r.yaw

    def test_displacement_t_from_mass(self):
        """displacement_t should be mass/1000."""
        r = gyradius_from_inertia(5e6, 1e8, 1e9, 1e8)
        assert r.displacement_t == pytest.approx(5000.0, rel=1e-9)

    def test_surge_sway_heave_derived(self):
        """Surge/sway/heave should be derived from inertia with scaling factors."""
        m = 1e6
        ixx, iyy, izz = 1e8, 1e9, 1e8
        r = gyradius_from_inertia(m, ixx, iyy, izz)
        # surge = sqrt(ixx/m) * 0.9 (from source code)
        assert r.surge == pytest.approx(math.sqrt(ixx / m) * 0.9, rel=1e-9)
        # sway = sqrt(iyy/m) * 0.9
        assert r.sway == pytest.approx(math.sqrt(iyy / m) * 0.9, rel=1e-9)
        # heave = sqrt(izz/m) * 0.5
        assert r.heave == pytest.approx(math.sqrt(izz / m) * 0.5, rel=1e-9)


# ---------------------------------------------------------------------------
# estimate_inertia_from_gyradius edge cases
# ---------------------------------------------------------------------------

class TestEstimateInertiaEdges:
    """Edge cases for k → I conversion."""

    def test_negative_gyradius_raises(self):
        with pytest.raises(ValueError, match="non-negative"):
            estimate_inertia_from_gyradius(1000, -1, 0, 0)

    def test_negative_ky_raises(self):
        with pytest.raises(ValueError, match="non-negative"):
            estimate_inertia_from_gyradius(1000, 0, -1, 0)

    def test_negative_kz_raises(self):
        with pytest.raises(ValueError, match="non-negative"):
            estimate_inertia_from_gyradius(1000, 0, 0, -1)

    def test_very_large_gyradius(self):
        """Very large gyradius should produce finite inertia."""
        result = estimate_inertia_from_gyradius(1e6, 1000.0, 2000.0, 1500.0)
        assert math.isfinite(result["ixx"])
        assert math.isfinite(result["iyy"])
        assert math.isfinite(result["izz"])

    def test_result_keys(self):
        """Result dict must have exactly ixx, iyy, izz."""
        result = estimate_inertia_from_gyradius(1000, 3, 4, 5)
        assert set(result.keys()) == {"ixx", "iyy", "izz"}


# ---------------------------------------------------------------------------
# Platform-specific shape checks
# ---------------------------------------------------------------------------

class TestPlatformShapeInvariants:
    """Verify known shape relationships for each platform type."""

    def test_fpso_pitch_gt_roll(self):
        """FPSO is ship-shaped: L >> B → pitch > roll."""
        r = gyradius_for_platform_type("fpso", 100000)
        assert r.pitch > r.roll

    def test_fpso_pitch_gt_yaw(self):
        """FPSO: pitch (about transverse axis) > yaw (about vertical)."""
        r = gyradius_for_platform_type("fpso", 100000)
        assert r.pitch > r.yaw

    def test_spar_roll_equals_yaw(self):
        """Spar is cylindrical: roll ≈ yaw."""
        r = gyradius_for_platform_type("spar", 100000)
        assert r.roll == pytest.approx(r.yaw, rel=1e-9)

    def test_spar_roll_equals_pitch(self):
        """Spar: cylindrical → L = B → pitch should use same length."""
        r = gyradius_for_platform_type("spar", 100000)
        ratios = GYRADIUS_RATIOS["spar"]
        # Since L == B for spar, roll = kx_beam * B, pitch = ky_length * L
        # kx_beam == ky_length for spar
        assert ratios["kx_beam"] == ratios["ky_length"]
        assert r.roll == pytest.approx(r.pitch, rel=1e-9)

    def test_semi_pitch_gt_roll(self):
        """Semi: L > B → pitch > roll."""
        r = gyradius_for_platform_type("semisubmersible", 100000)
        assert r.pitch > r.roll

    def test_barge_pitch_gt_roll(self):
        """Barge: L > B → pitch > roll."""
        r = gyradius_for_platform_type("barge", 100000)
        assert r.pitch > r.roll

    def test_tlp_compact_shape(self):
        """TLP: ky_length < kx_beam (compact hull)."""
        ratios = GYRADIUS_RATIOS["tlp"]
        assert ratios["ky_length"] < ratios["kx_beam"]


# ---------------------------------------------------------------------------
# GYRADIUS_RATIOS table coverage
# ---------------------------------------------------------------------------

class TestGyradiusRatios:
    """Verify the ratio table is complete and sensible."""

    def test_all_platform_types_in_table(self):
        expected = {"fpso", "semisubmersible", "spar", "tlp", "barge"}
        assert expected == set(GYRADIUS_RATIOS.keys())

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_all_ratio_keys_present(self, ptype):
        ratios = GYRADIUS_RATIOS[ptype]
        assert "kx_beam" in ratios
        assert "ky_length" in ratios
        assert "kz_beam" in ratios

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_ratios_positive(self, ptype):
        ratios = GYRADIUS_RATIOS[ptype]
        for key, val in ratios.items():
            assert val > 0, f"{ptype}.{key} = {val} is not positive"

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_ratios_less_than_one(self, ptype):
        """Ratios should be < 1 (fraction of characteristic dimension)."""
        ratios = GYRADIUS_RATIOS[ptype]
        for key, val in ratios.items():
            assert val < 1.0, f"{ptype}.{key} = {val} should be < 1.0"


# ---------------------------------------------------------------------------
# Case insensitivity
# ---------------------------------------------------------------------------

class TestCaseInsensitivity:
    """Platform type should be case-insensitive."""

    @pytest.mark.parametrize("ptype_upper,ptype_lower", [
        ("FPSO", "fpso"),
        ("Spar", "spar"),
        ("TLP", "tlp"),
        ("BARGE", "barge"),
        ("SemiSubmersible", "semisubmersible"),
    ])
    def test_case_variants_produce_same_result(self, ptype_upper, ptype_lower):
        r_upper = gyradius_for_platform_type(ptype_upper, 50000)
        r_lower = gyradius_for_platform_type(ptype_lower, 50000)
        assert r_upper.roll == r_lower.roll
        assert r_upper.pitch == r_lower.pitch
        assert r_upper.yaw == r_lower.yaw


# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------

class TestGyradiusErrors:
    """Error handling for invalid inputs."""

    @pytest.mark.parametrize("bad_type", ["submarine", "aircraft_carrier", "drillship", "jack_up", ""])
    def test_unknown_platform_raises(self, bad_type):
        with pytest.raises(ValueError, match="Unknown platform type"):
            gyradius_for_platform_type(bad_type, 50000)

    def test_zero_displacement_raises(self):
        with pytest.raises(ValueError, match="positive"):
            gyradius_for_platform_type("fpso", 0)

    def test_negative_displacement_raises(self):
        with pytest.raises(ValueError, match="positive"):
            gyradius_for_platform_type("spar", -100)

    def test_float_displacement_accepted(self):
        """Floating-point displacement should be accepted."""
        r = gyradius_for_platform_type("fpso", 50000.5)
        assert r.mass_kg == pytest.approx(50000.5 * 1000.0, rel=1e-9)
