"""
Tests for ASTM G80 (1998) cathodic disbonding calculations.

ASTM G80: Standard Test Method for Specific Cathodic Disbonding of
Pipeline Coatings.
"""

import math
import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_astm_g80 import (
    disbonding_area,
    net_disbonding_radius,
    evaluate_g80,
    G80TestResult,
)


# ---------------------------------------------------------------------------
# disbonding_area
# ---------------------------------------------------------------------------

class TestDisbondingArea:

    def test_nominal_annular_area(self):
        # A_d = pi * (r_d^2 - r_h^2) = pi * (15^2 - 5^2) = pi * 200
        result = disbonding_area(holiday_radius_mm=5.0, disbonding_radius_mm=15.0)
        expected = math.pi * (15.0**2 - 5.0**2)
        assert abs(result - expected) < 1e-6

    def test_zero_net_disbonding_returns_zero_area(self):
        # When disbonding radius equals holiday radius, no disbonding occurred
        result = disbonding_area(holiday_radius_mm=6.0, disbonding_radius_mm=6.0)
        assert result == pytest.approx(0.0, abs=1e-9)

    def test_small_holiday_large_disbond(self):
        # holiday = 1 mm, disbond = 30 mm
        result = disbonding_area(holiday_radius_mm=1.0, disbonding_radius_mm=30.0)
        expected = math.pi * (30.0**2 - 1.0**2)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_raises_on_zero_holiday_radius(self):
        with pytest.raises(ValueError, match="holiday_radius_mm"):
            disbonding_area(holiday_radius_mm=0.0, disbonding_radius_mm=10.0)

    def test_raises_on_negative_holiday_radius(self):
        with pytest.raises(ValueError, match="holiday_radius_mm"):
            disbonding_area(holiday_radius_mm=-3.0, disbonding_radius_mm=10.0)

    def test_raises_when_disbond_less_than_holiday(self):
        with pytest.raises(ValueError, match="disbonding_radius_mm"):
            disbonding_area(holiday_radius_mm=10.0, disbonding_radius_mm=5.0)


# ---------------------------------------------------------------------------
# net_disbonding_radius
# ---------------------------------------------------------------------------

class TestNetDisbondingRadius:

    def test_nominal(self):
        result = net_disbonding_radius(holiday_radius_mm=5.0, disbonding_radius_mm=15.0)
        assert result == pytest.approx(10.0, abs=1e-9)

    def test_zero_net(self):
        result = net_disbonding_radius(holiday_radius_mm=5.0, disbonding_radius_mm=5.0)
        assert result == pytest.approx(0.0, abs=1e-9)

    def test_raises_on_zero_holiday(self):
        with pytest.raises(ValueError, match="holiday_radius_mm"):
            net_disbonding_radius(holiday_radius_mm=0.0, disbonding_radius_mm=10.0)

    def test_raises_when_disbond_less_than_holiday(self):
        with pytest.raises(ValueError, match="disbonding_radius_mm"):
            net_disbonding_radius(holiday_radius_mm=10.0, disbonding_radius_mm=8.0)


# ---------------------------------------------------------------------------
# evaluate_g80
# ---------------------------------------------------------------------------

class TestEvaluateG80:

    def test_returns_g80_test_result(self):
        result = evaluate_g80(holiday_radius_mm=5.0, disbonding_radius_mm=15.0)
        assert isinstance(result, G80TestResult)

    def test_pass_when_within_acceptance(self):
        # net disbonding = 10 mm, acceptance = 12 mm → pass
        result = evaluate_g80(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            acceptance_radius_mm=12.0,
        )
        assert result.passes is True

    def test_fail_when_exceeds_acceptance(self):
        # net disbonding = 10 mm, acceptance = 8 mm → fail
        result = evaluate_g80(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            acceptance_radius_mm=8.0,
        )
        assert result.passes is False

    def test_exact_acceptance_boundary_passes(self):
        # net = 10 mm, acceptance = 10 mm → pass (≤)
        result = evaluate_g80(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            acceptance_radius_mm=10.0,
        )
        assert result.passes is True

    def test_no_acceptance_criterion_always_passes(self):
        result = evaluate_g80(holiday_radius_mm=5.0, disbonding_radius_mm=25.0)
        assert result.passes is True
        assert result.acceptance_radius_mm is None

    def test_area_field_matches_formula(self):
        result = evaluate_g80(holiday_radius_mm=5.0, disbonding_radius_mm=15.0)
        expected_area = math.pi * (15.0**2 - 5.0**2)
        assert result.disbonding_area_mm2 == pytest.approx(expected_area, rel=1e-4)

    def test_net_disbonding_field(self):
        result = evaluate_g80(holiday_radius_mm=5.0, disbonding_radius_mm=15.0)
        assert result.net_disbonding_mm == pytest.approx(10.0, abs=1e-3)

    def test_raises_on_non_positive_acceptance(self):
        with pytest.raises(ValueError, match="acceptance_radius_mm"):
            evaluate_g80(
                holiday_radius_mm=5.0,
                disbonding_radius_mm=10.0,
                acceptance_radius_mm=0.0,
            )

    def test_stored_inputs_match(self):
        result = evaluate_g80(
            holiday_radius_mm=3.0,
            disbonding_radius_mm=12.0,
            acceptance_radius_mm=10.0,
        )
        assert result.holiday_radius_mm == 3.0
        assert result.disbonding_radius_mm == 12.0
        assert result.acceptance_radius_mm == 10.0
