"""
Tests for ASTM G42 (1996) elevated-temperature cathodic disbonding calculations.

ASTM G42: Standard Test Method for Cathodic Disbonding of Pipeline Coatings
Subjected to Elevated Temperatures.
"""

import math
import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_astm_g42 import (
    arrhenius_correction_factor,
    evaluate_g42,
    G42TestResult,
    R_GAS,
    DEFAULT_ACTIVATION_ENERGY,
)


# ---------------------------------------------------------------------------
# arrhenius_correction_factor
# ---------------------------------------------------------------------------

class TestArrhenius:

    def test_at_reference_temperature_returns_one(self):
        f = arrhenius_correction_factor(
            test_temperature_c=25.0, reference_temperature_c=25.0
        )
        assert f == pytest.approx(1.0, rel=1e-9)

    def test_above_reference_returns_greater_than_one(self):
        f = arrhenius_correction_factor(
            test_temperature_c=65.0, reference_temperature_c=25.0
        )
        assert f > 1.0

    def test_below_reference_returns_less_than_one(self):
        f = arrhenius_correction_factor(
            test_temperature_c=5.0, reference_temperature_c=25.0
        )
        assert f < 1.0

    def test_formula_matches_manual_calculation(self):
        t_test_c = 60.0
        t_ref_c = 25.0
        ea = DEFAULT_ACTIVATION_ENERGY
        t_test_k = t_test_c + 273.15
        t_ref_k = t_ref_c + 273.15
        expected = math.exp(-(ea / R_GAS) * (1.0 / t_test_k - 1.0 / t_ref_k))
        result = arrhenius_correction_factor(t_test_c, t_ref_c, ea)
        assert result == pytest.approx(expected, rel=1e-9)

    def test_higher_activation_energy_gives_larger_factor(self):
        f_low = arrhenius_correction_factor(65.0, 25.0, activation_energy_j_mol=40_000)
        f_high = arrhenius_correction_factor(65.0, 25.0, activation_energy_j_mol=80_000)
        assert f_high > f_low

    def test_raises_on_out_of_range_test_temp(self):
        with pytest.raises(ValueError, match="test_temperature_c"):
            arrhenius_correction_factor(test_temperature_c=250.0)

    def test_raises_on_non_positive_activation_energy(self):
        with pytest.raises(ValueError, match="activation_energy_j_mol"):
            arrhenius_correction_factor(60.0, activation_energy_j_mol=0.0)


# ---------------------------------------------------------------------------
# evaluate_g42
# ---------------------------------------------------------------------------

class TestEvaluateG42:

    def test_returns_g42_test_result(self):
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=65.0,
        )
        assert isinstance(result, G42TestResult)

    def test_at_reference_temperature_correction_factor_is_one(self):
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=25.0,
            reference_temperature_c=25.0,
        )
        assert result.temperature_correction_factor == pytest.approx(1.0, rel=1e-4)

    def test_corrected_disbonding_smaller_than_measured_at_elevated_temp(self):
        # At elevated T, f_T > 1, so corrected = measured / f_T < measured
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=65.0,
        )
        assert result.corrected_net_disbonding_mm < result.net_disbonding_mm

    def test_pass_using_corrected_radius(self):
        # Net disbonding measured at 65°C.  Corrected value must be ≤ acceptance.
        # With high temperature f_T, corrected will be much smaller → pass
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=65.0,
            acceptance_radius_mm=5.0,
        )
        assert result.passes is True

    def test_fail_when_corrected_exceeds_acceptance(self):
        # At ambient temperature, no temperature correction → net = 10 mm
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=25.0,
            reference_temperature_c=25.0,
            acceptance_radius_mm=8.0,
        )
        assert result.passes is False

    def test_no_acceptance_criterion_always_passes(self):
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=50.0,
        )
        assert result.passes is True
        assert result.acceptance_radius_mm is None

    def test_disbonding_area_matches_formula(self):
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=40.0,
        )
        expected = math.pi * (15.0**2 - 5.0**2)
        assert result.disbonding_area_mm2 == pytest.approx(expected, rel=1e-4)

    def test_net_disbonding_field(self):
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=40.0,
        )
        assert result.net_disbonding_mm == pytest.approx(10.0, abs=1e-3)

    def test_temperature_field_stored(self):
        result = evaluate_g42(
            holiday_radius_mm=5.0,
            disbonding_radius_mm=15.0,
            test_temperature_c=80.0,
        )
        assert result.test_temperature_c == 80.0

    def test_raises_on_non_positive_acceptance(self):
        with pytest.raises(ValueError, match="acceptance_radius_mm"):
            evaluate_g42(
                holiday_radius_mm=5.0,
                disbonding_radius_mm=10.0,
                test_temperature_c=40.0,
                acceptance_radius_mm=0.0,
            )

    def test_raises_on_invalid_holiday_radius(self):
        with pytest.raises(ValueError, match="holiday_radius_mm"):
            evaluate_g42(
                holiday_radius_mm=0.0,
                disbonding_radius_mm=10.0,
                test_temperature_c=40.0,
            )

    def test_raises_when_disbond_less_than_holiday(self):
        with pytest.raises(ValueError, match="disbonding_radius_mm"):
            evaluate_g42(
                holiday_radius_mm=10.0,
                disbonding_radius_mm=5.0,
                test_temperature_c=40.0,
            )
