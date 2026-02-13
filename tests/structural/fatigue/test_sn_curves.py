"""
Comprehensive Unit Tests for S-N Curve Models
==============================================

Tests cover:
- PowerLawSNCurve: get_allowable_cycles, get_stress_range, scalar/array, edge cases
- BilinearSNCurve: two-slope behaviour, transition, scalar/array, edge cases
- StandardSNCurves: get_curve for all standards, list_curves, error handling
- MeanStressCorrection: Goodman, Gerber, Soderberg, Walker
- ThicknessCorrection: thickness effect application, edge cases
- SNDataFitting: power law fitting from experimental data
- Convenience functions: get_dnv_curve, get_api_curve, get_bs_curve
- plot_sn_curve utility
- MaterialProperties and SNPoint dataclasses
"""

import math
import numpy as np
import pandas as pd
import pytest

from digitalmodel.structural.fatigue.sn_curves import (
    BilinearSNCurve,
    MaterialProperties,
    MeanStressCorrection,
    PowerLawSNCurve,
    SNDataFitting,
    SNPoint,
    SNCurveBase,
    StandardSNCurves,
    ThicknessCorrection,
    get_api_curve,
    get_bs_curve,
    get_dnv_curve,
    plot_sn_curve,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def default_material():
    return MaterialProperties(
        ultimate_strength=400.0,
        yield_strength=250.0,
        elastic_modulus=200000.0,
        reference_thickness=25.0,
        thickness_exponent=0.25,
        name="Test Steel",
    )


@pytest.fixture
def simple_power_law():
    """N = 1e12 * S^(-3), fatigue_limit=0, cutoff=1e7"""
    return PowerLawSNCurve(name="Test-PL", A=1e12, m=3.0)


@pytest.fixture
def power_law_with_limit():
    """Power law with fatigue limit at 50 MPa."""
    return PowerLawSNCurve(
        name="Test-PL-FL", A=1e12, m=3.0, fatigue_limit=50.0, cutoff_cycles=1e8
    )


@pytest.fixture
def bilinear_curve():
    """Bilinear curve with continuity at transition.

    Transition at N=1e6:
        Slope 1: A1=1e12, m1=3  => S_transition = (1e12/1e6)^(1/3) = 100 MPa
        Slope 2: must give N=1e6 at S=100 => A2 = 1e6 * 100^5 = 1e16
    """
    return BilinearSNCurve(
        name="Test-BL",
        A1=1e12,
        m1=3.0,
        A2=1e16,
        m2=5.0,
        transition_cycles=1e6,
        fatigue_limit=10.0,
    )


@pytest.fixture
def dnv_d_curve():
    return StandardSNCurves.get_curve("DNV", "D")


@pytest.fixture
def test_data_for_fitting():
    """Synthetic test data following N = 2e12 * S^(-3)."""
    stress = np.array([100, 150, 200, 250, 300, 350, 400, 80, 120, 180])
    cycles = 2e12 * stress ** (-3.0)
    # Add small noise for realism
    rng = np.random.default_rng(42)
    noise = 10 ** (rng.normal(0, 0.05, len(cycles)))
    cycles = cycles * noise
    return pd.DataFrame({"stress_range": stress, "cycles_to_failure": cycles})


# ===========================================================================
# MaterialProperties and SNPoint dataclasses
# ===========================================================================


class TestMaterialProperties:
    def test_default_values(self):
        mat = MaterialProperties(ultimate_strength=500.0, yield_strength=350.0)
        assert mat.elastic_modulus == 200000.0
        assert mat.reference_thickness == 25.0
        assert mat.thickness_exponent == 0.25
        assert mat.name == "Generic Steel"

    def test_custom_values(self, default_material):
        assert default_material.ultimate_strength == 400.0
        assert default_material.yield_strength == 250.0
        assert default_material.name == "Test Steel"


class TestSNPoint:
    def test_default_probability(self):
        pt = SNPoint(stress_range=100.0, cycles=1e6)
        assert pt.probability == 0.5

    def test_custom_probability(self):
        pt = SNPoint(stress_range=100.0, cycles=1e6, probability=0.975)
        assert pt.probability == 0.975


# ===========================================================================
# PowerLawSNCurve
# ===========================================================================


class TestPowerLawGetAllowableCycles:
    """Tests for PowerLawSNCurve.get_allowable_cycles()"""

    def test_known_value_scalar(self, simple_power_law):
        """N = 1e12 * 100^(-3) = 1e12/1e6 = 1e6"""
        N = simple_power_law.get_allowable_cycles(100.0)
        assert N == pytest.approx(1e6, rel=1e-9)

    def test_known_value_high_stress(self, simple_power_law):
        """N = 1e12 * 1000^(-3) = 1e12/1e9 = 1000"""
        N = simple_power_law.get_allowable_cycles(1000.0)
        assert N == pytest.approx(1000.0, rel=1e-9)

    def test_array_input(self, simple_power_law):
        stress = np.array([100.0, 1000.0])
        N = simple_power_law.get_allowable_cycles(stress)
        assert isinstance(N, np.ndarray)
        assert N[0] == pytest.approx(1e6, rel=1e-9)
        assert N[1] == pytest.approx(1000.0, rel=1e-9)

    def test_scalar_returns_float(self, simple_power_law):
        N = simple_power_law.get_allowable_cycles(100.0)
        assert isinstance(N, float)

    def test_fatigue_limit_below_returns_inf(self, power_law_with_limit):
        N = power_law_with_limit.get_allowable_cycles(30.0)
        assert N == float("inf")

    def test_fatigue_limit_at_boundary(self, power_law_with_limit):
        """Stress exactly at fatigue limit should yield infinite life."""
        N = power_law_with_limit.get_allowable_cycles(50.0)
        assert N == float("inf")

    def test_fatigue_limit_just_above(self, power_law_with_limit):
        """Stress just above fatigue limit should yield finite cycles."""
        N = power_law_with_limit.get_allowable_cycles(50.001)
        assert np.isfinite(N)
        assert N > 0

    def test_cutoff_cycles_applied(self):
        """When computed N exceeds cutoff, it should be capped."""
        curve = PowerLawSNCurve(name="cutoff-test", A=1e20, m=3.0, cutoff_cycles=1e7)
        # Very low stress => huge N, but capped at cutoff
        N = curve.get_allowable_cycles(1.0)
        assert N == pytest.approx(1e7, rel=1e-9)

    def test_zero_stress_range(self, simple_power_law):
        """Zero stress range: S<=fatigue_limit(0), should give inf."""
        N = simple_power_law.get_allowable_cycles(0.0)
        assert N == float("inf")

    def test_very_high_stress(self, simple_power_law):
        """Very high stress should give very few cycles."""
        N = simple_power_law.get_allowable_cycles(1e6)
        assert N == pytest.approx(1e12 / (1e6) ** 3, rel=1e-9)
        assert N < 1.0

    def test_array_with_mixed_above_below_limit(self, power_law_with_limit):
        stress = np.array([30.0, 100.0, 50.0, 200.0])
        N = power_law_with_limit.get_allowable_cycles(stress)
        assert N[0] == float("inf")  # below limit
        assert np.isfinite(N[1])  # above limit
        assert N[2] == float("inf")  # at limit
        assert np.isfinite(N[3])  # above limit


class TestPowerLawGetStressRange:
    """Tests for PowerLawSNCurve.get_stress_range()"""

    def test_known_value_scalar(self, simple_power_law):
        """S = (A/N)^(1/m) = (1e12/1e6)^(1/3) = 100"""
        S = simple_power_law.get_stress_range(1e6)
        assert S == pytest.approx(100.0, rel=1e-9)

    def test_inverse_consistency(self, simple_power_law):
        """get_stress_range(get_allowable_cycles(S)) == S"""
        S_original = 150.0
        N = simple_power_law.get_allowable_cycles(S_original)
        S_recovered = simple_power_law.get_stress_range(N)
        assert S_recovered == pytest.approx(S_original, rel=1e-6)

    def test_infinite_cycles_returns_fatigue_limit(self, power_law_with_limit):
        S = power_law_with_limit.get_stress_range(float("inf"))
        assert S == pytest.approx(50.0, rel=1e-9)

    def test_array_input(self, simple_power_law):
        N = np.array([1e3, 1e6, 1e9])
        S = simple_power_law.get_stress_range(N)
        assert isinstance(S, np.ndarray)
        assert len(S) == 3
        # S = (1e12/N)^(1/3)
        expected = (1e12 / N) ** (1 / 3)
        np.testing.assert_allclose(S, expected, rtol=1e-9)

    def test_scalar_returns_float(self, simple_power_law):
        S = simple_power_law.get_stress_range(1e6)
        assert isinstance(S, float)

    def test_stress_never_below_fatigue_limit(self, power_law_with_limit):
        """Even at very high N, stress should not go below fatigue limit."""
        S = power_law_with_limit.get_stress_range(1e20)
        assert S >= power_law_with_limit.fatigue_limit


class TestPowerLawRepr:
    def test_repr(self, simple_power_law):
        r = repr(simple_power_law)
        assert "PowerLawSNCurve" in r
        assert "Test-PL" in r


# ===========================================================================
# BilinearSNCurve
# ===========================================================================


class TestBilinearGetAllowableCycles:
    """Tests for BilinearSNCurve.get_allowable_cycles()"""

    def test_high_stress_regime(self, bilinear_curve):
        """Above transition stress (100 MPa), use slope 1: N = A1*S^(-m1)"""
        S = 200.0  # well above transition
        N = bilinear_curve.get_allowable_cycles(np.array([S]))
        expected = 1e12 * S ** (-3.0)
        assert N[0] == pytest.approx(expected, rel=1e-6)

    def test_low_stress_regime(self, bilinear_curve):
        """Below transition stress (100 MPa), above fatigue limit, use slope 2."""
        S = 50.0  # between fatigue_limit(10) and transition(100)
        N = bilinear_curve.get_allowable_cycles(np.array([S]))
        expected = 1e16 * S ** (-5.0)
        assert N[0] == pytest.approx(expected, rel=1e-6)

    def test_below_fatigue_limit(self, bilinear_curve):
        N = bilinear_curve.get_allowable_cycles(np.array([5.0]))
        assert N[0] == float("inf")

    def test_transition_stress_value(self, bilinear_curve):
        """Transition stress = (A1/Nt)^(1/m1) = (1e12/1e6)^(1/3) = 100."""
        assert bilinear_curve.transition_stress == pytest.approx(100.0, rel=1e-6)

    def test_continuity_at_transition(self, bilinear_curve):
        """Both slopes should give same N at transition stress."""
        S_t = bilinear_curve.transition_stress
        N1 = bilinear_curve.A1 * S_t ** (-bilinear_curve.m1)
        N2 = bilinear_curve.A2 * S_t ** (-bilinear_curve.m2)
        assert N1 == pytest.approx(N2, rel=1e-3)

    def test_array_mixed_regimes(self, bilinear_curve):
        stress = np.array([200.0, 50.0, 5.0])
        N = bilinear_curve.get_allowable_cycles(stress)
        assert np.isfinite(N[0])  # high stress
        assert np.isfinite(N[1])  # low stress
        assert N[2] == float("inf")  # below fatigue limit

    def test_scalar_returns_float(self, bilinear_curve):
        N = bilinear_curve.get_allowable_cycles(200.0)
        assert isinstance(N, float)


class TestBilinearGetStressRange:
    """Tests for BilinearSNCurve.get_stress_range()"""

    def test_low_cycle_regime(self, bilinear_curve):
        """N <= transition_cycles: S = (A1/N)^(1/m1)"""
        N = 1e4  # low cycle
        S = bilinear_curve.get_stress_range(N)
        expected = (1e12 / N) ** (1 / 3)
        assert S == pytest.approx(expected, rel=1e-6)

    def test_high_cycle_regime(self, bilinear_curve):
        """N > transition_cycles: S = (A2/N)^(1/m2)"""
        N = 1e8  # high cycle
        S = bilinear_curve.get_stress_range(N)
        expected = (1e16 / N) ** (1 / 5)
        assert S == pytest.approx(expected, rel=1e-6)

    def test_infinite_cycles_returns_fatigue_limit(self, bilinear_curve):
        S = bilinear_curve.get_stress_range(float("inf"))
        assert S == pytest.approx(bilinear_curve.fatigue_limit, rel=1e-6)

    def test_scalar_returns_float(self, bilinear_curve):
        S = bilinear_curve.get_stress_range(1e5)
        assert isinstance(S, float)

    def test_array_input(self, bilinear_curve):
        N = np.array([1e4, 1e8])
        S = bilinear_curve.get_stress_range(N)
        assert isinstance(S, np.ndarray)
        assert len(S) == 2


class TestBilinearContinuityWarning:
    def test_discontinuous_curve_logs_warning(self, caplog):
        """A bilinear curve that is not continuous at transition should warn."""
        import logging

        with caplog.at_level(logging.WARNING):
            BilinearSNCurve(
                name="Discont",
                A1=1e12,
                m1=3.0,
                A2=1e10,  # discontinuous
                m2=5.0,
                transition_cycles=1e6,
            )
        assert "may not be continuous" in caplog.text


# ===========================================================================
# StandardSNCurves
# ===========================================================================


class TestStandardSNCurvesGetCurve:
    """Tests for StandardSNCurves.get_curve()"""

    def test_dnv_d_curve(self, dnv_d_curve):
        assert isinstance(dnv_d_curve, PowerLawSNCurve)
        assert dnv_d_curve.A == pytest.approx(5.73e11, rel=1e-6)
        assert dnv_d_curve.m == pytest.approx(3.0, rel=1e-6)
        assert dnv_d_curve.fatigue_limit == pytest.approx(52.63, rel=1e-3)

    def test_api_x_curve(self):
        curve = StandardSNCurves.get_curve("API", "X")
        assert curve.A == pytest.approx(1.01e12, rel=1e-6)
        assert curve.m == pytest.approx(3.0, rel=1e-6)

    def test_bs_d_curve(self):
        curve = StandardSNCurves.get_curve("BS", "D")
        assert curve.A == pytest.approx(3.9e11, rel=1e-6)

    def test_aws_a_curve(self):
        curve = StandardSNCurves.get_curve("AWS", "A")
        assert curve.A == pytest.approx(8.11e11, rel=1e-6)

    def test_curve_name_format(self, dnv_d_curve):
        assert dnv_d_curve.name == "DNV-D"

    def test_unknown_standard_raises(self):
        with pytest.raises(ValueError, match="Unknown standard"):
            StandardSNCurves.get_curve("INVALID", "D")

    def test_unknown_curve_class_raises(self):
        with pytest.raises(ValueError, match="Unknown DNV curve class"):
            StandardSNCurves.get_curve("DNV", "INVALID_CLASS")

    def test_custom_material_passed_through(self, default_material):
        curve = StandardSNCurves.get_curve("DNV", "D", material=default_material)
        assert curve.material.name == "Test Steel"


class TestStandardSNCurvesListCurves:
    def test_list_all_curves(self):
        result = StandardSNCurves.list_curves()
        assert "DNV" in result
        assert "API" in result
        assert "BS" in result
        assert "AWS" in result
        assert "DNV_MULTISLOPE" in result

    def test_list_single_standard(self):
        result = StandardSNCurves.list_curves("DNV")
        assert "DNV" in result
        assert len(result) == 1

    def test_list_unknown_standard_raises(self):
        with pytest.raises(ValueError, match="Unknown standard"):
            StandardSNCurves.list_curves("INVALID")

    def test_list_curves_case_insensitive(self):
        """list_curves checks standard.upper()."""
        result = StandardSNCurves.list_curves("dnv")
        assert "DNV" in result


class TestDNVCurveValuesFromStandard:
    """Verify specific DNV-RP-C203 S-N curve values against known reference data."""

    def test_dnv_d_at_1e6_cycles(self):
        """DNV-D: at N=1e6, S = (5.73e11/1e6)^(1/3) ~ 83.04 MPa"""
        curve = get_dnv_curve("D")
        S = curve.get_stress_range(1e6)
        expected = (5.73e11 / 1e6) ** (1 / 3)
        assert S == pytest.approx(expected, rel=1e-4)

    def test_dnv_c_at_1e5_cycles(self):
        """DNV-C: at N=1e5, S = (1.08e12/1e5)^(1/3) ~ 102.6 MPa (above fatigue limit)"""
        curve = get_dnv_curve("C")
        S = curve.get_stress_range(1e5)
        expected = (1.08e12 / 1e5) ** (1 / 3)
        assert S == pytest.approx(expected, rel=1e-4)

    def test_dnv_b1_higher_than_d(self):
        """B1 curve allows higher stress range than D for same cycles."""
        b1 = get_dnv_curve("B1")
        d = get_dnv_curve("D")
        S_b1 = b1.get_stress_range(1e6)
        S_d = d.get_stress_range(1e6)
        assert S_b1 > S_d

    def test_dnv_w3_lowest_fatigue_limit(self):
        """W3 has the lowest fatigue limit among DNV curves."""
        w3 = get_dnv_curve("W3")
        d = get_dnv_curve("D")
        assert w3.fatigue_limit < d.fatigue_limit

    def test_dnv_d_roundtrip(self):
        """get_stress_range(get_allowable_cycles(S)) == S for DNV-D."""
        curve = get_dnv_curve("D")
        S_in = 80.0
        N = curve.get_allowable_cycles(S_in)
        S_out = curve.get_stress_range(N)
        assert S_out == pytest.approx(S_in, rel=1e-6)


# ===========================================================================
# MeanStressCorrection
# ===========================================================================


class TestGoodmanCorrection:
    def test_zero_mean_stress(self):
        """With zero mean stress, corrected = original amplitude."""
        result = MeanStressCorrection.goodman(100.0, 0.0, 400.0)
        assert result == pytest.approx(100.0, rel=1e-9)

    def test_positive_mean_stress(self):
        """Sa_eq = Sa / (1 - Sm/Su) = 100 / (1 - 100/400) = 133.33"""
        result = MeanStressCorrection.goodman(100.0, 100.0, 400.0)
        assert result == pytest.approx(133.333, rel=1e-3)

    def test_mean_stress_at_ultimate_returns_inf(self):
        result = MeanStressCorrection.goodman(100.0, 400.0, 400.0)
        assert result == float("inf")

    def test_mean_stress_above_ultimate_returns_inf(self):
        result = MeanStressCorrection.goodman(100.0, 500.0, 400.0)
        assert result == float("inf")

    def test_half_ultimate_mean_stress(self):
        """Sa_eq = 100 / (1 - 200/400) = 100/0.5 = 200"""
        result = MeanStressCorrection.goodman(100.0, 200.0, 400.0)
        assert result == pytest.approx(200.0, rel=1e-9)


class TestGerberCorrection:
    def test_zero_mean_stress(self):
        result = MeanStressCorrection.gerber(100.0, 0.0, 400.0)
        assert result == pytest.approx(100.0, rel=1e-9)

    def test_positive_mean_stress(self):
        """Sa_eq = Sa / (1 - (Sm/Su)^2) = 100 / (1 - (100/400)^2)"""
        expected = 100.0 / (1 - (100.0 / 400.0) ** 2)
        result = MeanStressCorrection.gerber(100.0, 100.0, 400.0)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_mean_stress_at_ultimate_returns_inf(self):
        result = MeanStressCorrection.gerber(100.0, 400.0, 400.0)
        assert result == float("inf")

    def test_gerber_less_conservative_than_goodman(self):
        """Gerber parabolic should give lower corrected stress than Goodman linear."""
        goodman = MeanStressCorrection.goodman(100.0, 150.0, 400.0)
        gerber = MeanStressCorrection.gerber(100.0, 150.0, 400.0)
        assert gerber < goodman


class TestSoderbergCorrection:
    def test_zero_mean_stress(self):
        result = MeanStressCorrection.soderberg(100.0, 0.0, 250.0)
        assert result == pytest.approx(100.0, rel=1e-9)

    def test_positive_mean_stress(self):
        """Sa_eq = Sa / (1 - Sm/Sy) = 100 / (1 - 100/250) = 166.67"""
        result = MeanStressCorrection.soderberg(100.0, 100.0, 250.0)
        expected = 100.0 / (1 - 100.0 / 250.0)
        assert result == pytest.approx(expected, rel=1e-3)

    def test_mean_stress_at_yield_returns_inf(self):
        result = MeanStressCorrection.soderberg(100.0, 250.0, 250.0)
        assert result == float("inf")

    def test_soderberg_more_conservative_than_goodman(self):
        """Soderberg uses yield strength, so more conservative than Goodman."""
        goodman = MeanStressCorrection.goodman(100.0, 100.0, 400.0)
        soderberg = MeanStressCorrection.soderberg(100.0, 100.0, 250.0)
        assert soderberg > goodman


class TestWalkerCorrection:
    def test_zero_mean_stress(self):
        """With mean=0, stress_max=Sa, R=0, Sa_eq = Sa * (1-0)^(0.5-1) = Sa"""
        result = MeanStressCorrection.walker(100.0, 0.0, 400.0, gamma=0.5)
        assert result == pytest.approx(100.0, rel=1e-6)

    def test_positive_mean_stress_gamma_half(self):
        """Sa=100, Sm=50: S_max=150, R=50/150=1/3, Sa_eq=100*(1-1/3)^(-0.5)"""
        Sa, Sm = 100.0, 50.0
        gamma = 0.5
        S_max = Sa + Sm
        R = Sm / S_max
        expected = Sa * ((1 - R) ** (gamma - 1))
        result = MeanStressCorrection.walker(Sa, Sm, 400.0, gamma=gamma)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_default_gamma(self):
        """Default gamma should be 0.5."""
        r1 = MeanStressCorrection.walker(100.0, 50.0, 400.0)
        r2 = MeanStressCorrection.walker(100.0, 50.0, 400.0, gamma=0.5)
        assert r1 == pytest.approx(r2, rel=1e-9)


# ===========================================================================
# ThicknessCorrection
# ===========================================================================


class TestThicknessCorrection:
    def test_reference_thickness_no_change(self, dnv_d_curve):
        """At reference thickness (25 mm), correction factor = 1."""
        corrected = ThicknessCorrection.apply_thickness_effect(
            dnv_d_curve, actual_thickness=25.0
        )
        assert corrected.A == pytest.approx(dnv_d_curve.A, rel=1e-6)
        assert corrected.fatigue_limit == pytest.approx(
            dnv_d_curve.fatigue_limit, rel=1e-6
        )

    def test_thicker_section_reduces_strength(self, dnv_d_curve):
        """Thicker than reference should reduce allowable cycles."""
        corrected = ThicknessCorrection.apply_thickness_effect(
            dnv_d_curve, actual_thickness=50.0
        )
        # thicker => lower A => fewer cycles at same stress
        N_base = dnv_d_curve.get_allowable_cycles(80.0)
        N_corrected = corrected.get_allowable_cycles(80.0)
        assert N_corrected < N_base

    def test_thinner_section_increases_strength(self, dnv_d_curve):
        """Thinner than reference should increase allowable cycles (no correction below ref)."""
        corrected = ThicknessCorrection.apply_thickness_effect(
            dnv_d_curve, actual_thickness=10.0
        )
        N_base = dnv_d_curve.get_allowable_cycles(80.0)
        N_corrected = corrected.get_allowable_cycles(80.0)
        assert N_corrected > N_base

    def test_thickness_correction_factor_50mm(self, dnv_d_curve):
        """Verify the correction factor calculation for 50mm thickness."""
        t_actual = 50.0
        t_ref = 25.0
        k = 0.25
        factor = (t_actual / t_ref) ** k  # = 2^0.25 ~ 1.1892
        corrected = ThicknessCorrection.apply_thickness_effect(
            dnv_d_curve, actual_thickness=t_actual
        )
        expected_A = dnv_d_curve.A / (factor ** dnv_d_curve.m)
        assert corrected.A == pytest.approx(expected_A, rel=1e-6)

    def test_zero_thickness_raises(self, dnv_d_curve):
        with pytest.raises(ValueError, match="Thickness must be positive"):
            ThicknessCorrection.apply_thickness_effect(
                dnv_d_curve, actual_thickness=0.0
            )

    def test_negative_thickness_raises(self, dnv_d_curve):
        with pytest.raises(ValueError, match="Thickness must be positive"):
            ThicknessCorrection.apply_thickness_effect(
                dnv_d_curve, actual_thickness=-10.0
            )

    def test_custom_exponent(self, dnv_d_curve):
        corrected = ThicknessCorrection.apply_thickness_effect(
            dnv_d_curve,
            actual_thickness=50.0,
            thickness_exponent=0.20,
        )
        factor = (50.0 / 25.0) ** 0.20
        expected_A = dnv_d_curve.A / (factor ** dnv_d_curve.m)
        assert corrected.A == pytest.approx(expected_A, rel=1e-6)

    def test_corrected_name_contains_thickness(self, dnv_d_curve):
        corrected = ThicknessCorrection.apply_thickness_effect(
            dnv_d_curve, actual_thickness=32.0
        )
        assert "32" in corrected.name

    def test_fatigue_limit_corrected(self, dnv_d_curve):
        t_actual = 50.0
        factor = (t_actual / 25.0) ** 0.25
        corrected = ThicknessCorrection.apply_thickness_effect(
            dnv_d_curve, actual_thickness=t_actual
        )
        expected_fl = dnv_d_curve.fatigue_limit / factor
        assert corrected.fatigue_limit == pytest.approx(expected_fl, rel=1e-6)


# ===========================================================================
# SNDataFitting
# ===========================================================================


class TestSNDataFitting:
    def test_fit_recovers_slope(self, test_data_for_fitting):
        """Fitted slope should be close to 3.0 for N = A*S^(-3) data."""
        curve, stats = SNDataFitting.fit_power_law(test_data_for_fitting)
        assert curve.m == pytest.approx(3.0, abs=0.15)

    def test_fit_r_squared_high(self, test_data_for_fitting):
        """R-squared should be very high for clean synthetic data."""
        _, stats = SNDataFitting.fit_power_law(test_data_for_fitting)
        assert stats["r_squared"] > 0.95

    def test_fit_returns_power_law_curve(self, test_data_for_fitting):
        curve, _ = SNDataFitting.fit_power_law(test_data_for_fitting)
        assert isinstance(curve, PowerLawSNCurve)
        assert curve.name == "Fitted_Curve"

    def test_fit_statistics_keys(self, test_data_for_fitting):
        _, stats = SNDataFitting.fit_power_law(test_data_for_fitting)
        assert "r_squared" in stats
        assert "rmse_log_cycles" in stats
        assert "n_points" in stats
        assert "stress_range" in stats
        assert "cycle_range" in stats
        assert "fitted_parameters" in stats

    def test_fit_too_few_points_raises(self):
        df = pd.DataFrame(
            {"stress_range": [100.0, 200.0], "cycles_to_failure": [1e6, 1e5]}
        )
        with pytest.raises(ValueError, match="at least 3 data points"):
            SNDataFitting.fit_power_law(df)

    def test_fit_with_invalid_data_filtered(self):
        """Rows with zero/negative/inf should be filtered out."""
        df = pd.DataFrame(
            {
                "stress_range": [100.0, 200.0, 300.0, -50.0, 0.0, 150.0],
                "cycles_to_failure": [1e6, 1e5, 5e4, 1e6, 1e6, 2e5],
            }
        )
        curve, stats = SNDataFitting.fit_power_law(df)
        assert stats["n_points"] == 4  # only positive/finite kept

    def test_fit_all_invalid_raises(self):
        """If all data is invalid after filtering, should raise."""
        df = pd.DataFrame(
            {
                "stress_range": [-100.0, 0.0, np.inf],
                "cycles_to_failure": [1e6, 1e5, 5e4],
            }
        )
        with pytest.raises(ValueError, match="Insufficient valid data"):
            SNDataFitting.fit_power_law(df)

    def test_fit_estimates_fatigue_limit_wide_range(self):
        """When cycle range > 100x, a fatigue limit should be estimated."""
        stress = np.array([50, 80, 100, 150, 200, 300, 400, 500])
        cycles = 5e12 * stress ** (-3.0)
        df = pd.DataFrame({"stress_range": stress, "cycles_to_failure": cycles})
        curve, stats = SNDataFitting.fit_power_law(df)
        # Wide range => fatigue_limit estimated from 10th percentile
        assert curve.fatigue_limit > 0

    def test_fit_no_fatigue_limit_narrow_range(self):
        """When cycle range < 100x, fatigue limit should be 0."""
        stress = np.array([100.0, 110.0, 120.0, 130.0])
        cycles = 1e12 * stress ** (-3.0)
        df = pd.DataFrame({"stress_range": stress, "cycles_to_failure": cycles})
        curve, _ = SNDataFitting.fit_power_law(df)
        assert curve.fatigue_limit == 0.0


# ===========================================================================
# plot_sn_curve
# ===========================================================================


class TestPlotSNCurve:
    def test_returns_arrays(self, simple_power_law):
        cycles, stress = plot_sn_curve(simple_power_law)
        assert isinstance(cycles, np.ndarray)
        assert isinstance(stress, np.ndarray)

    def test_output_lengths_match(self, simple_power_law):
        cycles, stress = plot_sn_curve(simple_power_law)
        assert len(cycles) == len(stress)

    def test_log_scale_default(self, simple_power_law):
        cycles, stress = plot_sn_curve(simple_power_law, n_points=50)
        assert len(cycles) <= 50  # may be fewer if inf values filtered
        # Log spacing: ratio between consecutive points should be roughly constant
        if len(cycles) > 2:
            ratios = cycles[1:] / cycles[:-1]
            np.testing.assert_allclose(ratios, ratios[0], rtol=0.01)

    def test_linear_scale(self, simple_power_law):
        cycles, stress = plot_sn_curve(
            simple_power_law, log_scale=False, n_points=50
        )
        assert len(cycles) > 0

    def test_custom_range(self, simple_power_law):
        cycles, stress = plot_sn_curve(
            simple_power_law, cycles_range=(1e4, 1e6), n_points=20
        )
        assert cycles[0] >= 1e4
        assert cycles[-1] <= 1e6

    def test_all_stress_values_finite(self, simple_power_law):
        """plot_sn_curve should filter out infinite stress values."""
        _, stress = plot_sn_curve(simple_power_law)
        assert np.all(np.isfinite(stress))

    def test_all_stress_values_positive(self, simple_power_law):
        _, stress = plot_sn_curve(simple_power_law)
        assert np.all(stress > 0)


# ===========================================================================
# Convenience functions
# ===========================================================================


class TestConvenienceFunctions:
    def test_get_dnv_curve_basic(self):
        curve = get_dnv_curve("D")
        assert isinstance(curve, PowerLawSNCurve)
        assert "DNV" in curve.name

    def test_get_dnv_curve_with_thickness(self):
        curve = get_dnv_curve("D", thickness=50.0)
        assert "50" in curve.name

    def test_get_dnv_curve_ref_thickness_no_correction(self):
        """Passing 25.0 should not apply correction (thickness != 25.0 is False)."""
        curve = get_dnv_curve("D", thickness=25.0)
        # thickness=25.0 => thickness != 25.0 is False => no correction
        assert curve.name == "DNV-D"

    def test_get_api_curve(self):
        curve = get_api_curve("X")
        assert isinstance(curve, PowerLawSNCurve)

    def test_get_bs_curve(self):
        curve = get_bs_curve("D")
        assert isinstance(curve, PowerLawSNCurve)


# ===========================================================================
# Edge cases and mathematical properties
# ===========================================================================


class TestMathematicalProperties:
    def test_higher_stress_fewer_cycles(self, simple_power_law):
        """More stress => fewer allowable cycles (monotone decreasing)."""
        N_low = simple_power_law.get_allowable_cycles(100.0)
        N_high = simple_power_law.get_allowable_cycles(200.0)
        assert N_high < N_low

    def test_bilinear_higher_stress_fewer_cycles(self, bilinear_curve):
        stress = np.array([50.0, 80.0, 120.0, 200.0])
        N = bilinear_curve.get_allowable_cycles(stress)
        # Monotonically decreasing
        for i in range(len(N) - 1):
            assert N[i] > N[i + 1]

    def test_power_law_exponent_effect(self):
        """Higher m => steeper curve => bigger difference between stress levels."""
        curve_m3 = PowerLawSNCurve(name="m3", A=1e12, m=3.0)
        curve_m5 = PowerLawSNCurve(name="m5", A=1e12, m=5.0)

        # Both at 100 MPa
        N_m3 = curve_m3.get_allowable_cycles(100.0)
        N_m5 = curve_m5.get_allowable_cycles(100.0)

        # m=5 curve should be steeper; at 200 MPa ratio is bigger
        ratio_m3 = curve_m3.get_allowable_cycles(100.0) / curve_m3.get_allowable_cycles(200.0)
        ratio_m5 = curve_m5.get_allowable_cycles(100.0) / curve_m5.get_allowable_cycles(200.0)
        assert ratio_m5 > ratio_m3  # steeper slope = bigger ratio

    def test_negative_stress_treated_as_below_limit(self, power_law_with_limit):
        """Negative stress range is below fatigue limit (0 < limit), so infinite life."""
        N = power_law_with_limit.get_allowable_cycles(-10.0)
        assert N == float("inf")

    def test_extremely_small_positive_stress(self, simple_power_law):
        """Very small stress => very high N, capped at cutoff."""
        N = simple_power_law.get_allowable_cycles(0.001)
        # cutoff is 1e7 by default
        assert N == pytest.approx(1e7, rel=1e-6)

    def test_mean_stress_corrections_ordering(self):
        """For same inputs, Soderberg > Goodman > Gerber (conservatism order)."""
        Sa, Sm, Su, Sy = 100.0, 100.0, 400.0, 250.0
        goodman = MeanStressCorrection.goodman(Sa, Sm, Su)
        gerber = MeanStressCorrection.gerber(Sa, Sm, Su)
        soderberg = MeanStressCorrection.soderberg(Sa, Sm, Sy)
        assert soderberg > goodman > gerber
