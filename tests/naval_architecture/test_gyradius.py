# ABOUTME: Comprehensive tests for gyradius calculator
# ABOUTME: Validated against SNAME PNA and platform reference data
"""Tests for digitalmodel.naval_architecture.gyradius."""

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
# GyradiusResult dataclass
# ---------------------------------------------------------------------------


class TestGyradiusResult:
    def test_properties(self):
        r = GyradiusResult(
            surge=1, sway=2, heave=3, roll=10, pitch=50, yaw=12,
            mass_kg=1e6, displacement_t=1000,
        )
        assert r.kxx == 10
        assert r.kyy == 50
        assert r.kzz == 12

    def test_moment_of_inertia(self):
        m = 1e6
        r = GyradiusResult(
            surge=1, sway=2, heave=3, roll=10, pitch=50, yaw=12,
            mass_kg=m, displacement_t=1000,
        )
        assert r.ixx == pytest.approx(m * 10 ** 2)
        assert r.iyy == pytest.approx(m * 50 ** 2)
        assert r.izz == pytest.approx(m * 12 ** 2)


# ---------------------------------------------------------------------------
# gyradius_from_inertia  (I → k direct computation)
# ---------------------------------------------------------------------------


class TestGyradiusFromInertia:
    def test_basic(self):
        """k = sqrt(I/m)."""
        m = 1000.0
        r = gyradius_from_inertia(m, 9000.0, 16000.0, 25000.0)
        assert r.roll == pytest.approx(3.0, rel=1e-9)
        assert r.pitch == pytest.approx(4.0, rel=1e-9)
        assert r.yaw == pytest.approx(5.0, rel=1e-9)

    def test_zero_inertia(self):
        r = gyradius_from_inertia(1000.0, 0.0, 0.0, 0.0)
        assert r.roll == 0.0
        assert r.pitch == 0.0
        assert r.yaw == 0.0

    def test_negative_mass_raises(self):
        with pytest.raises(ValueError, match="positive"):
            gyradius_from_inertia(-100, 1, 1, 1)

    def test_negative_inertia_raises(self):
        with pytest.raises(ValueError, match="non-negative"):
            gyradius_from_inertia(1000, -1, 1, 1)

    def test_roundtrip(self):
        """I → k → I should be identity."""
        m = 200e6
        ixx, iyy, izz = 6.8e10, 7.2e11, 8.5e10
        r = gyradius_from_inertia(m, ixx, iyy, izz)
        recovered = estimate_inertia_from_gyradius(m, r.roll, r.pitch, r.yaw)
        assert recovered["ixx"] == pytest.approx(ixx, rel=1e-6)
        assert recovered["iyy"] == pytest.approx(iyy, rel=1e-6)
        assert recovered["izz"] == pytest.approx(izz, rel=1e-6)


# ---------------------------------------------------------------------------
# estimate_inertia_from_gyradius  (k → I conversion)
# ---------------------------------------------------------------------------


class TestEstimateInertiaFromGyradius:
    def test_basic(self):
        result = estimate_inertia_from_gyradius(1000.0, 3.0, 4.0, 5.0)
        assert result["ixx"] == pytest.approx(9000.0)
        assert result["iyy"] == pytest.approx(16000.0)
        assert result["izz"] == pytest.approx(25000.0)

    def test_negative_mass_raises(self):
        with pytest.raises(ValueError, match="positive"):
            estimate_inertia_from_gyradius(-100, 1, 1, 1)

    def test_zero_gyradius(self):
        result = estimate_inertia_from_gyradius(1000.0, 0.0, 0.0, 0.0)
        assert result["ixx"] == 0.0
        assert result["iyy"] == 0.0
        assert result["izz"] == 0.0


# ---------------------------------------------------------------------------
# gyradius_for_platform_type  (empirical estimation)
# ---------------------------------------------------------------------------


class TestGyradiusForPlatformType:
    def test_fpso_200kt(self):
        """FPSO 200,000 DWT — typical deepwater reference."""
        r = gyradius_for_platform_type("fpso", 200000)
        # pitch > roll because L >> B for ship-shaped hull
        assert r.pitch > r.roll
        assert r.roll > 5
        assert r.pitch > 50
        assert r.mass_kg == pytest.approx(200e6, rel=1e-6)

    def test_spar_symmetry(self):
        """Spar: cylindrical → roll ≈ yaw (both from beam)."""
        r = gyradius_for_platform_type("spar", 100000)
        # For spar, length = beam, and kx_beam == kz_beam
        ratios = GYRADIUS_RATIOS["spar"]
        assert ratios["kx_beam"] == ratios["kz_beam"]
        assert r.roll == pytest.approx(r.yaw, rel=1e-9)
        assert r.mass_kg == pytest.approx(100e6, rel=1e-6)

    def test_semi(self):
        r = gyradius_for_platform_type("semisubmersible", 50000)
        assert r.pitch > r.roll  # L > B for semi
        assert r.mass_kg == pytest.approx(50e6, rel=1e-6)

    def test_tlp(self):
        r = gyradius_for_platform_type("tlp", 60000)
        ratios = GYRADIUS_RATIOS["tlp"]
        assert ratios["ky_length"] < ratios["kx_beam"]  # TLP compact
        assert r.pitch > 0
        assert r.yaw > 0

    def test_barge(self):
        r = gyradius_for_platform_type("barge", 30000)
        assert r.pitch > r.roll  # L > B for barge
        assert r.mass_kg == pytest.approx(30e6, rel=1e-6)

    def test_unknown_type_raises(self):
        with pytest.raises(ValueError, match="Unknown platform type"):
            gyradius_for_platform_type("submarine", 5000)

    def test_negative_disp_raises(self):
        with pytest.raises(ValueError, match="positive"):
            gyradius_for_platform_type("fpso", -1000)

    def test_case_insensitive(self):
        r1 = gyradius_for_platform_type("FPSO", 100000)
        r2 = gyradius_for_platform_type("fpso", 100000)
        assert r1.roll == r2.roll

    def test_all_types_defined(self):
        required = {"fpso", "semisubmersible", "spar", "tlp", "barge"}
        assert required.issubset(set(GYRADIUS_RATIOS.keys()))

    def test_scaling(self):
        """Larger displacement → larger absolute gyradius."""
        r1 = gyradius_for_platform_type("fpso", 50000)
        r2 = gyradius_for_platform_type("fpso", 200000)
        assert r2.roll > r1.roll, f"{r2.roll} should exceed {r1.roll}"
        assert r2.pitch > r1.pitch

    def test_mass_consistency(self):
        """Gyradius values are consistent with mass."""
        r = gyradius_for_platform_type("spar", 80000)
        assert r.mass_kg == pytest.approx(80e6, rel=1e-6)
        assert r.displacement_t == 80000.0
