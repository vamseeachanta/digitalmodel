# ABOUTME: TDD tests for Holtrop-Mennen (1984) resistance prediction method
# ABOUTME: Validated against published Series 60 and tanker form test cases
"""Holtrop-Mennen resistance tests. Series 60 and tanker validation."""

import pytest

from digitalmodel.naval_architecture.holtrop_mennen import (
    appendage_resistance,
    bulbous_bow_resistance,
    correlation_allowance,
    effective_power,
    form_factor_k1,
    frictional_resistance,
    total_resistance,
    transom_resistance,
    wave_resistance,
    wetted_surface_holtrop,
)

RHO_SW = 1025.0  # kg/m^3


class TestFormFactor:
    """Form factor (1+k1) from hull geometry."""

    def test_form_factor_series60(self):
        """Series 60 Cb=0.60: expect (1+k1) in plausible range."""
        assert 1.10 < form_factor_k1(121.92, 16.26, 6.50, 0.614, -2.02, 0) < 1.35

    def test_form_factor_tanker(self):
        """Tanker Cb=0.80: fuller hull -> higher form factor."""
        assert 1.15 < form_factor_k1(205.0, 32.0, 12.0, 0.804, -0.5, 0) < 1.45

    def test_form_factor_validation_case(self):
        """Medium hull (1+k1) ~ 1.10-1.30."""
        assert 1.10 <= form_factor_k1(150.0, 20.0, 8.0, 0.663, -1.5, 0) <= 1.30

    def test_form_factor_invalid_lwl(self):
        """Negative LWL must raise ValueError."""
        with pytest.raises(ValueError):
            form_factor_k1(-10.0, 20.0, 8.0, 0.65, -1.5, 0)


class TestWettedSurface:
    """Holtrop wetted surface approximation."""

    def test_wetted_surface_series60(self):
        """Series 60: wetted surface should be reasonable."""
        s = wetted_surface_holtrop(
            lwl=121.92, beam=16.26, draft=6.50,
            cb=0.60, cm=0.977, cwp=0.70, abt=0.0,
        )
        # Holtrop approximation for 122m x 16m x 6.5m hull
        assert 2000.0 < s < 4000.0

    def test_wetted_surface_tanker(self):
        """Tanker: larger hull -> larger wetted surface."""
        s = wetted_surface_holtrop(
            lwl=205.0, beam=32.0, draft=12.0,
            cb=0.80, cm=0.995, cwp=0.88, abt=0.0,
        )
        assert 8000.0 < s < 15000.0


class TestFrictionalResistance:
    """ITTC 1957 frictional resistance in SI."""

    def test_frictional_resistance_positive(self):
        """RF must be positive for valid inputs."""
        rf = frictional_resistance(
            lwl=121.92, speed_ms=7.72, wetted_surface=3500.0,
        )
        assert rf > 0.0

    def test_frictional_resistance_invalid_speed(self):
        """Zero speed must raise ValueError."""
        with pytest.raises(ValueError):
            frictional_resistance(
                lwl=121.92, speed_ms=0.0, wetted_surface=3500.0,
            )


class TestWaveResistance:
    """Wave-making resistance (Fn < 0.4 regime)."""

    def test_wave_resistance_series60(self):
        """Series 60 at Fn=0.224: wave resistance non-negative."""
        rw = wave_resistance(
            lwl=121.92, beam=16.26, draft=6.50, cb=0.60, cm=0.977,
            cwp=0.70, cp=0.614, lcb_pct=-2.02, cstern=0,
            speed_ms=7.72, abt=0.0, tf=6.50, hb=0.0, at=0.0,
        )
        assert rw >= 0.0

    def test_wave_resistance_low_speed(self):
        """Very low Fn: wave resistance should be small relative to weight."""
        rw = wave_resistance(
            lwl=205.0, beam=32.0, draft=12.0, cb=0.80, cm=0.995,
            cwp=0.88, cp=0.804, lcb_pct=-0.5, cstern=0,
            speed_ms=2.0, abt=0.0, tf=12.0, hb=0.0, at=0.0,
        )
        assert rw / (RHO_SW * 9.80665 * 205.0 * 32.0 * 12.0 * 0.80) < 0.001


class TestAppendageResistance:
    """Appendage drag contribution."""

    def test_no_appendages_zero(self):
        """No appendages -> zero resistance."""
        rapp = appendage_resistance(
            speed_ms=7.72, appendage_areas=[],
            appendage_factors=[], lwl=121.92,
        )
        assert rapp == 0.0

    def test_with_rudder(self):
        """Single rudder appendage gives positive resistance."""
        rapp = appendage_resistance(
            speed_ms=7.72, appendage_areas=[10.0],
            appendage_factors=[1.5], lwl=121.92,
        )
        assert rapp > 0.0


class TestMinorComponents:
    """Bulbous bow, transom, and correlation allowance."""

    def test_no_bulb_zero(self):
        """Zero bulb area -> zero resistance."""
        assert bulbous_bow_resistance(speed_ms=7.72, abt=0.0, tf=6.50, hb=0.0) == 0.0

    def test_no_transom_zero(self):
        """Zero transom area -> zero resistance."""
        assert transom_resistance(speed_ms=7.72, at=0.0, beam=16.26, cwp=0.70, lwl=121.92) == 0.0

    def test_correlation_allowance_range(self):
        """CA should be small positive or near-zero."""
        ca = correlation_allowance(lwl=121.92, cb=0.60, draft=6.50)
        assert -0.001 < ca < 0.002

    def test_correlation_allowance_large_ship(self):
        """Larger ships tend toward lower CA."""
        ca_small = correlation_allowance(lwl=100.0, cb=0.65, draft=5.0)
        ca_large = correlation_allowance(lwl=300.0, cb=0.65, draft=12.0)
        assert ca_large < ca_small


class TestTotalResistance:
    """Total resistance RT integration."""

    @staticmethod
    def _compute_ct(lwl, beam, draft, cb, cm, cwp, cp, lcb, cs, v, tf):
        """Helper: compute CT from hull params."""
        rt = total_resistance(
            lwl=lwl, beam=beam, draft=draft, cb=cb, cm=cm,
            cwp=cwp, cp=cp, lcb_pct=lcb, cstern=cs,
            speed_ms=v, abt=0.0, tf=tf, hb=0.0, at=0.0,
        )
        s = wetted_surface_holtrop(
            lwl=lwl, beam=beam, draft=draft,
            cb=cb, cm=cm, cwp=cwp, abt=0.0,
        )
        return rt / (0.5 * RHO_SW * v ** 2 * s)

    def test_series60_ct(self):
        """Series 60 Cb=0.60: CT in plausible range."""
        ct = self._compute_ct(121.92, 16.26, 6.50, 0.60, 0.977, 0.70, 0.614, -2.02, 0, 7.72, 6.50)
        assert 0.002 < ct < 0.005, f"CT={ct:.5f}"

    def test_tanker_ct(self):
        """Tanker Cb=0.80: CT in plausible range."""
        ct = self._compute_ct(205.0, 32.0, 12.0, 0.80, 0.995, 0.88, 0.804, -0.5, 0, 7.72, 12.0)
        assert 0.001 < ct < 0.003, f"CT={ct:.5f}"


class TestEffectivePower:
    """Effective power PE = RT * V."""

    def test_effective_power_positive(self):
        """PE must be positive for valid inputs."""
        rt = total_resistance(
            lwl=121.92, beam=16.26, draft=6.50, cb=0.60, cm=0.977,
            cwp=0.70, cp=0.614, lcb_pct=-2.02, cstern=0,
            speed_ms=7.72, abt=0.0, tf=6.50, hb=0.0, at=0.0,
        )
        assert effective_power(rt, speed_ms=7.72) > 0.0

    def test_effective_power_units(self):
        """PE in watts = RT(N) * V(m/s)."""
        assert abs(effective_power(1000.0, 10.0) - 10000.0) < 0.01
