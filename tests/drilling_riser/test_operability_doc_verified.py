"""Doc-verified tests for drilling riser operability calculations.

References:
- RAO-based operability envelope analysis
- 2H-TNE-0050-03 §5.2 — Operating Envelope Analysis
- Wave scatter diagram analysis for operability fraction
"""

import pytest

from digitalmodel.drilling_riser.operability import (
    operability_fraction,
    significant_wave_height_limit,
    watch_circle_radius_m,
)


class TestSignificantWaveHeightLimit:
    """Hs limit derived from RAO amplitude and allowable motion."""

    def test_basic_hs_limit(self):
        """RAO = 1.2 m/m, allowable = 3.0 m -> Hs_limit = 3.0 / 1.2 = 2.5 m."""
        result = significant_wave_height_limit(
            rao_amplitude=1.2, allowable_motion_m=3.0
        )
        assert result == pytest.approx(2.5, rel=0.01)

    def test_hs_limit_increases_with_allowable_motion(self):
        """Larger allowable motion permits higher sea states."""
        hs1 = significant_wave_height_limit(1.2, 2.0)
        hs2 = significant_wave_height_limit(1.2, 4.0)
        assert hs2 > hs1

    def test_hs_limit_decreases_with_rao(self):
        """Larger RAO means more sensitive, lower Hs limit."""
        hs1 = significant_wave_height_limit(0.8, 3.0)
        hs2 = significant_wave_height_limit(1.6, 3.0)
        assert hs1 > hs2

    def test_hs_limit_positive(self):
        """Hs limit must always be positive."""
        result = significant_wave_height_limit(2.0, 1.0)
        assert result > 0.0


class TestOperabilityFraction:
    """Fraction of sea states within Hs limit from wave scatter diagram."""

    def test_all_below_limit(self):
        """All sea states below limit -> 100% operability."""
        scatter = [1.0, 1.5, 2.0, 2.5]
        result = operability_fraction(hs_scatter=scatter, hs_limit=3.0)
        assert result == pytest.approx(1.0, rel=0.01)

    def test_none_below_limit(self):
        """All sea states above limit -> 0% operability."""
        scatter = [4.0, 5.0, 6.0]
        result = operability_fraction(hs_scatter=scatter, hs_limit=3.0)
        assert result == pytest.approx(0.0, rel=0.01)

    def test_partial_operability(self):
        """Half the sea states below limit -> 50%."""
        scatter = [1.0, 2.0, 4.0, 5.0]
        result = operability_fraction(hs_scatter=scatter, hs_limit=3.0)
        assert result == pytest.approx(0.5, rel=0.01)

    def test_operability_bounded_zero_to_one(self):
        """Result always between 0 and 1."""
        scatter = [1.0, 2.0, 3.0, 4.0, 5.0]
        result = operability_fraction(hs_scatter=scatter, hs_limit=3.5)
        assert 0.0 <= result <= 1.0


class TestWatchCircleRadius:
    """Allowable watch circle radius = offset_pct * water_depth."""

    def test_basic_watch_circle(self):
        """5% offset at 1500m depth -> 75m radius."""
        result = watch_circle_radius_m(offset_pct=5.0, water_depth_m=1500.0)
        assert result == pytest.approx(75.0, rel=0.01)

    def test_watch_circle_increases_with_depth(self):
        """Deeper water allows larger absolute watch circle."""
        r1 = watch_circle_radius_m(5.0, 1000.0)
        r2 = watch_circle_radius_m(5.0, 2000.0)
        assert r2 > r1

    def test_watch_circle_increases_with_offset(self):
        """Larger offset percentage gives larger radius."""
        r1 = watch_circle_radius_m(3.0, 1500.0)
        r2 = watch_circle_radius_m(8.0, 1500.0)
        assert r2 > r1

    def test_watch_circle_positive(self):
        """Watch circle radius must be positive for valid inputs."""
        result = watch_circle_radius_m(2.0, 500.0)
        assert result > 0.0
