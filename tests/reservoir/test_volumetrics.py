# ABOUTME: Tests for reservoir.volumetrics — OOIP and recovery estimates.
# ABOUTME: Includes the Collide PE Problem of the Day 5/21/2026 regression.

"""Tests for digitalmodel.reservoir.volumetrics module."""

import pytest

from digitalmodel.reservoir.volumetrics import (
    VolumetricInputs,
    incremental_recovery_stb,
    ooip_stb,
    recoverable_stb,
)


class TestOoip:
    """OOIP = 7758 * A * h * phi * So / Boi."""

    def test_known_volumetric(self):
        inp = VolumetricInputs(
            area_acres=1000.0,
            net_pay_ft=50.0,
            porosity=0.20,
            oil_saturation=0.75,
            formation_volume_factor=1.2,
        )
        # 7758*1000*50*0.20*0.75/1.2
        assert ooip_stb(inp) == pytest.approx(48_487_500.0, rel=1e-6)

    def test_higher_boi_lowers_ooip(self):
        base = VolumetricInputs(640.0, 30.0, 0.18, 0.7, 1.1)
        higher_boi = VolumetricInputs(640.0, 30.0, 0.18, 0.7, 1.4)
        assert ooip_stb(higher_boi) < ooip_stb(base)


class TestRecovery:
    def test_recoverable_is_fraction_of_ooip(self):
        assert recoverable_stb(100_000_000.0, 0.25) == pytest.approx(25_000_000.0)

    def test_incremental_uplift(self):
        assert incremental_recovery_stb(100_000_000.0, 0.22, 0.30) == pytest.approx(
            8_000_000.0
        )


# ---------------------------------------------------------------------------
# Regression: Collide "PE Problem of the Day" 5/21/2026 (OOIP volumetrics)
# https://app.collide.io/posts/pe-problem-of-the-day-5-21-2026
# 1200 acres, 40 ft net pay, 18% porosity, 80% So, Boi 1.25, primary RF 22%.
# ---------------------------------------------------------------------------

class TestCollidePeProblem20260521:
    def setup_method(self):
        self.inp = VolumetricInputs(
            area_acres=1200.0,
            net_pay_ft=40.0,
            porosity=0.18,
            oil_saturation=0.80,
            formation_volume_factor=1.25,
        )
        self.ooip = ooip_stb(self.inp)

    def test_a_ooip(self):
        # ~42.9 MMSTB
        assert self.ooip == pytest.approx(42_898_636.8, rel=1e-6)

    def test_b_primary_recoverable_22pct(self):
        assert recoverable_stb(self.ooip, 0.22) == pytest.approx(9_437_700.1, rel=1e-6)

    def test_c_secondary_uplift_22_to_30pct(self):
        # extra barrels from RF 22% -> 30%
        assert incremental_recovery_stb(self.ooip, 0.22, 0.30) == pytest.approx(
            3_431_891.0, rel=1e-6
        )
